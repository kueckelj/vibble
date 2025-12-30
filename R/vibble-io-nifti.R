# I/O and conversion between arrays and vibbles.


#' @title Convert DICOM data to a vibble
#' @description
#' Convert a DICOM directory or file set into a NIfTI volume and return the result as a \link[=vbl_doc_vbl]{vibble}.
#'
#' @param x Path to a DICOM directory or vector of DICOM files.
#' @param var Character name of the variable under which voxel values will be
#'   stored in the output vibble (e.g. \code{"t1"}, \code{"flair"}).
#' @param ... Additional arguments forwarded to \link{convertDicom}().
#'
#' @inheritParams vbl_doc
#'
#' @return A \link[=vbl_doc_vbl]{vibble}.
#'
#' @details
#' The function performs:
#' \enumerate{
#'   \item Conversion of the provided DICOM input into a temporary NIfTI file via \link{convertDicom}().
#'   \item Import of that NIfTI volume using \link{nifti_to_vbl}().
#'   \item Automatic cleanup of the temporary NIfTI file.
#' }
#'
#' All spatial metadata (orientation, dimensions, spacing) follow the
#' conventions defined in the vibble 3D spatial reference system.
#'
#' @seealso \link{nifti_to_vbl}
#'
#' @examples
#' \dontrun{
#' d <- dcm_to_vbl("path/to/dicom_folder", var = "t1")
#' }
#'
#' @export
dcm_to_vbl <- function(x,
                       var,
                       rm0 = FALSE,
                       verbose = vbl_opts("verbose"),
                       ...){

  stopifnot(is.character(var))

  xpath <- divest::convertDicom(path = x, interactive = FALSE)

  out <- nifti_to_vbl(x = xpath, var = var, rm0 = rm0, verbose = verbose)

  unlink(xpath)

  return(out)

}

#' @title Convert vibble variable to NIfTI object.
#'
#' @description
#' `vbl_to_nifti()` converts a single non-ccs variable of a vibble into an
#' in-memory NIfTI object. Voxel values are taken from the vibble, arranged
#' according to the requested orientation, and written into a NIfTI volume
#' with minimal but valid spatial header information.
#'
#' @details
#' If `datatype` is not explicitly provided, it is inferred from the vibble
#' variable content using `.infer_datatype()`.
#'
#' Voxel spacing (`pixdim`) is derived from the vibble coordinate step sizes
#' and reordered to match the specified orientation. A simple diagonal
#' voxel-to-world transform is written to both sform and qform to ensure that
#' the orientation is well-defined. No attempt is made to recover real scanner
#' geometry beyond voxel spacing and axis orientation.
#'
#' @param var Character scalar. Name of a variable in `vbl` to be
#' converted into a NIfTI volume.
#' @param orientation Character scalar. Three-letter orientation string
#' specifying axis directions (e.g. `"RAS"`, `"LPI"`).
#' @param missing Numeric scalar. Value used to fill voxels that are missing
#' in the vibble.
#' @param datatype Integer scalar. NIfTI datatype code. If `NULL`, the datatype
#' is inferred automatically from the variable content.
#'
#' @inheritParams vbl_doc
#'
#' @return
#' A `oro.nifti` object containing the voxel data and basic spatial metadata.
#'
#' @keywords internal

vbl_to_nifti <- function(vbl,
                         var,
                         orientation = "RAS",
                         missing = 0,
                         datatype = NULL,
                         verbose = vbl_opts("verbose")
                         ){

  if(is.null(datatype)){

    datatype <- .infer_datatype(vbl, var = var)

  }

  data <-
    vbl_to_array(
      vbl = vbl,
      var = var,
      orientation = orientation,
      missing = missing,
      verbose = verbose
      )

  nii <- oro.nifti::nifti(data, datatype = datatype)

  nii@pixdim[1] <- -1 # TODO

  # basic header geometry
  nii@pixdim[2:4] <-
    ccs_steps(vbl) %>%
    .reorder_lip(x = ., orientation = orientation) %>%
    purrr::flatten_int()

  # set a simple sform/qform so orientation is defined
  # (voxel -> world: diagonal scaling, zero offset)
  M <- diag(c(nii@pixdim[2:4], 1))
  nii@sform_code <- 1L
  nii@qform_code <- 1L
  nii@srow_x <- M[1, ]
  nii@srow_y <- M[2, ]
  nii@srow_z <- M[3, ]

  nii

}



#' @title Write NIfTI volume from a vibble
#' @description
#' Export a voxel-wise variable from a \link{vibble} as a NIfTI image using a
#' reference header, optional resampling to a target volume via FreeSurfer
#' `mri_vol2vol`, and optional LUT handling for labeled data.
#'
#' @details
#' The reference NIfTI is chosen as:
#' \itemize{
#'   \item `path_ref` from `...`, read with `oro.nifti::readNIfTI()` if provided
#'   \item otherwise `attr(vbl, "nifti")`, which must be a valid NIfTI object
#' }
#'
#' The data array of the reference is replaced by the result of
#' \link{vbl_to_array}(), using the orientation from `RNifti::orientation(nii)`
#' and filling missing voxels with `missing`.
#'
#' If `path_out` is a character path, the function writes a NIfTI file:
#' \itemize{
#'   \item If `type == "label"` (from \link{var_type}()), a LUT is created
#'         with \link{make_lut}(). If `path_lut` is not a character, it is
#'         derived from `path_out` by replacing `.nii.gz` with `_lut.csv`.
#'         A message indicates the LUT path
#'   \item If `path_target` is a character path, the volume is first written
#'         to a temporary file and then resampled into the target space using
#'         FreeSurfer `mri_vol2vol`. The interpolation method defaults to
#'         `"cubic"` for numeric variables and `"nearest"` otherwise, unless
#'         overridden via `interp`
#'   \item If `path_target` is `NULL`, the NIfTI is written directly with
#'         `oro.nifti::writeNIfTI()`
#' }
#'
#' When `path_out` is not supplied, no file is written; only the modified NIfTI
#' object is returned invisibly.
#'
#' @param var Character scalar. Name of the variable in `vbl` to export as a NIfTI volume.
#' @param path_out Character scalar. The file path to the output NIfTI file (`.nii.gz`).
#' @param path_lut
#' Optional character path for a LUT CSV associated with labeled variables.
#' If `NULL` and the variable type is labeled, a default path is derived
#' from `path_out` by replacing `.nii.gz` with `_lut.csv`.
#' @param path_target
#' Optional character path to a target NIfTI volume. If provided, the output
#' volume is resampled into the target space using FreeSurfer `mri_vol2vol`.
#' @param dir_temp
#' Directory for temporary files used during resampling.
#' @param path_fs
#' File system path to the FreeSurfer installation used to source
#' `SetUpFreeSurfer.sh` and call `mri_vol2vol`.
#' @param interp
#' Interpolation method for `mri_vol2vol`. One of `"cubic"`, `"linear"`,
#' `"nearest"`. If `NULL`, defaults to `"cubic"` for numeric variables and
#' `"nearest"` for non-numeric variables.
#' @param missing
#' Numeric value used to fill voxel locations not present in `vbl` when
#' reconstructing the array.
#'
#' @return Invisibly returns `TRUE` in case of successfull execution.
#'
#' @export

write_nifti <- function(vbl,
                        var,
                        path_out,
                        orientation = "RAS",
                        missing = 0,
                        datatype = NULL,
                        path_native = NULL,
                        path_target = NULL,
                        path_lut = NULL,
                        interp = NULL,
                        path_fs = "/Applications/freesurfer/7.4.0",
                        dir_temp = getwd(),
                        verbose = vbl_def(),
                        ...){

  verbose <- .resolve_verbose(verbose)
  type <- var_type(vbl[[var]])

  if(is.character(path_native)){

    nii <- oro.nifti::readNIfTI(path_native, reorient = FALSE)
    orientation <- RNifti::orientation(nii)

    nii@.Data <-
      vbl_to_array(
        vbl = vbl,
        var = var,
        orientation = orientation,
        missing = missing,
        verbose = verbose
        )

  } else {

    nii <-
      vbl_to_nifti(
        vbl = vbl,
        var = var,
        orientation = orientation,
        missing = missing,
        datatype = datatype,
        verbose = verbose
      )

  }

  # handle LUT if required
  if(type == "categorical" & !is.character(path_lut)){

    lut <- make_lut(vbl, var = var)

    if(!is.character(path_lut)){

      path_lut <- stringr::str_replace(path_out, "\\.nii\\.gz$", "_lut.csv")

    }

    stopifnot(stringr::str_detect(path_lut, "_lut.csv$"))

    .glue_message("Writing LUT to '{path_lut}'.", verbose = verbose)

    readr::write_csv(x = lut, file = path_lut)

  }

  # adjust to target volume with mri_vol2vol
  if(is.character(path_target)){

    if(is.null(interp)){

      interp <- ifelse(type == "numeric", "cubic", "nearest")

    } else {

      interp <- match.arg(interp, choices = c("cubic", "linear", "nearest"))

    }

    path_nii_mov <-
      stringr::str_c(sample(x = letters, size = 30, replace = T), collapse = "") %>%
      stringr::str_c("temp_", ., ".nii.gz") %>%
      file.path(dir_temp, .)

    oro.nifti::writeNIfTI(nii, filename = stringr::str_remove(path_nii_mov, ".nii.gz"))

    cmd <- paste0(
      'bash -lc "',
      'export FREESURFER_HOME=', shQuote(path_fs), '; ',
      'source ', shQuote(file.path(path_fs, "SetUpFreeSurfer.sh")), '; ',
      'mri_vol2vol ',
      '--mov ',  shQuote(path_nii_mov), ' ',
      '--targ ', shQuote(path_target), ' ',
      '--regheader ',
      '--o ',    shQuote(path_out), ' ',
      '--interp ', interp, '"',
      collapse = ""
    )

    .glue_message("Writing NIFTI to '{path_out}' with:\n {stringr::str_replace_all(cmd,';', ';\n')}", verbose = verbose)
    out <- system(cmd, show.output.on.console = verbose)

    unlink(path_nii_mov)

    nii <- oro.nifti::readNIfTI(fname = path_out, reorient = FALSE)

    # directly save as output
  } else {

    .glue_message("Writing NIFTI to '{path_out}'.", verbose = verbose)

    oro.nifti::writeNIfTI(nii, filename = stringr::str_remove(path_out, ".nii.gz"))

  }

  # return invisibly
  invisible(nii)

}


#' @title Convert a NIfTI image into a vibble.
#' @description
#' Create a \link[=vbl_doc_vbl]{vibble} from a NIfTI object or file path by melting the voxel
#' data into long format.
#'
#' @details
#' The function accepts either a NIfTI object or a character file path. When a
#' path is supplied, the file is read without reorientation. If the original
#' NIfTI object has \code{@reoriented == TRUE}, a warning is issued because
#' voxel orientation may be unreliable.
#'
#' Coordinate directions are extracted from the NIfTI header via
#' \code{RNifti::orientation()}. Axes are renamed to \code{x}, \code{y},
#' \code{z} according to the package's canonical coordinate mapping and - if required -
#' flipped.
#'
#' The voxel values of the array in `@.Data` are inspected and interpreted:
#'
#' \itemize{
#'   \item if binary 0/1 values → mask
#'   \item if all integers & LUT provided → label
#'   \item else -> numeric
#' }
#'
#' If \code{lut} is supplied and the variable appears to represent labeled
#' classes, the lookup table is applied via \link{map_lut}(). Otherwise the
#' variable is left numeric with a message (depending on \code{verbose}).
#'
#' @param x
#' A NIfTI object or a character path to a \code{.nii} or \code{.nii.gz} file.
#'
#' @param var
#' Character. The column name with which the data extracted from the nifti is stored
#' in vibble data.frame.
#'
#' @param ordered
#' Logical. If a lookup table is provided, controls whether the
#' resulting labeled factor is ordered. Defaults to `FALSE`.
#'
#' @param add_id
#' Logical. If \code{TRUE}, add a unique voxel ID column.
#'
#' @inherit vbl_doc params
#'
#' @param ...
#' Additional arguments; currently supports the deprecated argument
#' \code{black_rm}.
#'
#' @return A \link{vibble}.
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' vbl <- nifti_to_vbl("t1_orig.nii.gz")
#'
#' # With lookup table for labeled images
#' lut <- data.frame(index = 1:3, label = c("GM", "WM", "CSF"))
#' seg_vbl <- nifti_to_vbl("seg_regMNI.nii.gz", var = "tissue", lut = lut)
#'
#' # Remove background voxels and add IDs
#' vbl <- nifti_to_vbl("mask.nii.gz", rm0 = TRUE, add_id = TRUE)
#' }
#'
#' @export
nifti_to_vbl <- function(x,
                         var = "value",
                         lut = vbl_def(),
                         ordered = FALSE,
                         add_id = FALSE,
                         rm0 = FALSE,
                         verbose = vbl_opts("verbose"),
                         ...){

  pointer_strings <- purrr::flatten_chr(ccs_orientation_mapping)

  # ensure compatibility with `pointers` downstream
  var_orig <- NULL
  xpath <- NULL
  if(var %in% pointer_strings){

    var_orig <- var
    var <- paste0(var, "xxx")

  }

  if(is.character(x)){

    msg <- .glue_message("Read from ~/{basename(x)}.", verbose = verbose)
    xpath <- x
    x <- oro.nifti::readNIfTI(fname = x, reorient = FALSE)

  } else {

    msg <- "NIfTI input."

  }

  # spatial checks
  if(isTRUE(x@reoriented)){

    rlang::warn("Nifti object has @reoriented == TRUE. Output orientation is not reliable.")

  }

  msg <- paste(msg, "Variable '{var}'.")

  # use to force XYZ==LIP orientation in the data.frame
  flip_check <- list(x = "R", y = "S", z = "A")

  pointers <-
    RNifti::orientation(x, useQuaternionFirst = FALSE) %>%
    stringr::str_split_1(pattern = "")

  names(pointers) <-
    purrr::map_chr(
      .x = pointers,
      .f = function(o){

        cc_match <- purrr::keep(ccs_orientation_mapping, .p = ~ o %in% .x)
        names(cc_match)

      })

  flip_axes <-
    purrr::imap(.x = pointers, .f = ~ .x == flip_check[[.y]]) %>%
    purrr::keep(.p = ~ .x) %>%
    names()

  if(length(dim(x)) < 3){

    rlang::abort("Input NIfTI has less than 3 dimensions. Invalid.")

  } else if(length(dim(x)) > 3){

    rlang::warn("Input NIfTI has more than 3 dimensions. Reducing.")
    data_array <- x@.Data[,,,1]

  } else {

    data_array <- x@.Data

  }

  # create preliminary data for vbl and reorient if required
  data <-
    reshape2::melt(data_array, varnames = unname(pointers), value.name = var) %>%
    tibble::as_tibble() %>%
    dplyr::select(!!!pointers[c(ccs_labels)], !!rlang::sym(var))

  for(fa in flip_axes){

    data[[fa]] <- max(data[[fa]]) - data[[fa]] + 1

  }

  # identify logical input
  if(is_mask_candidate(data[[var]])){

    data[[var]] <- as.logical(data[[var]])

  }

  # identify and manage categorical input
  if(is_label_candidate(data[[var]])){

    # how is LUT to be interpreted
    if(.is_vbl_def(lut)){

      if(is.character(xpath)){

        lut <- stringr::str_replace(xpath, pattern = ".nii.gz$", replacement = "_lut.csv")

        if(file.exists(lut)){

          msg <- paste0(msg, glue::glue(" Interpreting as categorical with detected lut: ~/{basename(lut)} "))

        } else {

          msg <- paste0(msg, glue::glue(" Interpreting as numeric - no LUT detected/specified."))
          lut <- NULL

        }

      } else {

        lut <- NULL

      }

    } else if(is.character(lut)) {

      msg <- paste0(msg, glue::glue(" Interpreting as categorical with provided LUT: ~/{basename(lut)}."))

    } else {

      msg <- paste0(msg, " Interpreting as numeric - LUT disabled.", collasep = "")
      lut <- NULL

    }

    # apply LUT
    if(is.character(lut)){

      if(file.exists(lut)){

        data <- map_lut(data, var = var, lut = lut, ordered = isTRUE(ordered), verbose = verbose)

      } else {

        rlang::warn(
          message = c(
            glue::glue("LUT specified '{lut}' but file does not exist. Interpreting '{var}' as numeric."),
            i = "See `?map_lut` for more information on how to apply a look up table to a vibble."
          )
        )

      }

    }

  } else { # numeric or mask

    var_type <- ifelse(is_mask_var(data[[var]]), "mask", "numeric")
    msg <- paste0(msg, " Interpreting as {var_type}.")

  }

  # identify numeric input
  if(is_numeric_candidate(data[[var]])){

    var_limits(data[[var]]) <- range(data[[var]], na.rm = TRUE)

  }

  # officially construct vibble with attributes
  ccs_limits <-
    purrr::map(
      .x = data[,c(vbl_ccs_axes)],
      .f = ~ range(.x) %>% as.integer()
      )

  ccs_steps <- vector(mode = "list", length = 3)
  names(ccs_steps) <- vbl_ccs_axes
  px_steps <- x@pixdim[2:4]

  for(axis in vbl_ccs_axes){

    idx <- which(names(pointers) == axis)

    ccs_steps[[axis]] <- px_steps[idx]

  }

  vbl <-
    new_vbl(
      data = data,
      ccs_limits = ccs_limits,
      ccs_steps = ccs_steps
      )

  .glue_message(msg, verbose = verbose)


  # post process
  if(is.character(var_orig)){

    vbl[[var_orig]] <- vbl[[var]]
    vbl[[var]] <- NULL

  }

  if(isTRUE(rm0)){ vbl <- vbl[vbl[[var]] != 0, ] }

  if(isTRUE(add_id)){ vbl <- id_add(vbl) }

  # output
  return(vbl)

}

#' @export
#' @keywords internal
nifti_to_voxel_df <- nifti_to_vbl


#' @title Read multiple NIfTI files into a merged vibble.
#' @description
#' Convert one or more NIfTI files into a single \link{vibble} by reading
#' all matching files, extracting variable names from filenames, and
#' merging the resulting vibbles using \code{join_vibbles()}.
#'
#' @details
#' The function operates on either:
#' \itemize{
#'   \item a directory (single-character string) containing NIfTI files, or
#'   \item a character vector of file paths.
#' }
#'
#' All files ending in \code{.nii.gz} are collected.
#' These paths can be filtered using a regular expression (\code{rgx_fp}), and
#' variable names used for each file are extracted from the filename using
#' \code{rgx_var}. Each extracted variable name must be unique.
#'
#' For each file, \code{nifti_to_vbl()} is called with \code{rm0} and
#' \code{verbose} forwarded. The resulting list of vibbles is then merged into
#' a single vibble using \code{purrr::reduce()} and \code{join_vibbles()} with
#' the join type specified in \code{join}.
#'
#' Errors are thrown if:
#' \itemize{
#'   \item no NIfTI files are found,
#'   \item file-path filtering removes all files,
#'   \item variable names cannot be extracted from some filenames,
#'   \item extracted variable names are duplicated.
#' }
#'
#' @param x
#' Character. Either a folder directory containing NIfTI files or a character vector
#' of file paths.
#'
#' @param rgx_fp
#' Character scalar. Regular expression applied to file paths to retain only
#' matching NIfTI files.
#'
#' @param rgx_var
#' Character scalar. Regular expression used to extract variable names from
#' the basename (without extension) of each NIfTI file.
#'
#' @param strip_var
#' Characte vector. A set of `pattern` inputs that are stripped from variable
#' names via \link{str_remove_all}().
#'
#' @param recursive
#' Logical. If \code{TRUE} and `x` is a directory it is searched recursively for NIfTI files.
#'
#' @inheritParams join_vibbles
#'
#' @return
#' A merged \link{vibble} containing one voxel variable per input file.
#' Variable names correspond to the values extracted via \code{rgx_var}.
#'
#' @examples
#' \dontrun{
#' # Read all NIfTI files in a directory
#' v <- niftis_to_vbl("data/images/", rgx_var = "^[A-Za-z]+")
#'
#' # From explicit file list
#' files <- c("t1.nii.gz", "flair.nii.gz", "mask.nii.gz")
#' v <- niftis_to_vbl(files, rgx_var = "^[A-Za-z]+")
#'
#' # Restrict to files with "_orig" in their name
#' v <- niftis_to_vbl("data/", rgx_fp = "_orig", rgx_var = "^[A-Za-z]+")
#' }
#'
#' @export
niftis_to_vbl <- function(x,
                          rgx_fp = ".*",
                          rgx_var = ".*",
                          strip_var = NULL,
                          recursive = FALSE,
                          join = "full",
                          .rfn = NULL,
                          rm0 = FALSE,
                          verbose = vbl_opts("verbose")){

  stopifnot(is.character(x))

  if(length(x) == 1){ # should be directory

    stopifnot(dir.exists(x))

    nii_paths <-
      list.files(path = x, full.names = TRUE, recursive = recursive) %>%
      stringr::str_subset(pattern = ".nii.gz$")

    if(length(nii_paths) == 0){ .glue_stop("No NIFTI files found in '{x}'.") }

  } else { # set of nifti file paths

    nii_paths <- stringr::str_subset(x, pattern = ".nii.gz$")

    if(length(nii_paths) == 0){ .glue_stop("No NIFTI files found in `x`.") }

  }

  # subset file names by regex
  stopifnot(is.character(rgx_fp) && length(rgx_fp) == 1)
  nii_paths <- stringr::str_subset(nii_paths, pattern = rgx_fp)

  if(length(nii_paths) == 0){ .glue_stop("No NIFTI files remain with rgx_fp = '{rgx_fp}'.") }

  # handle varnames
  nii_files <- stringr::str_remove_all(basename(nii_paths), pattern = ".nii.gz$")

  stopifnot(is.character(rgx_var) && length(rgx_var) == 1)
  var_names <- stringr::str_extract(nii_files, pattern = rgx_var)

  if(is.character(strip_var)){

    for(pattern in strip_var){

      var_names <- stringr::str_remove_all(var_names, pattern = pattern)

    }

  }

  if(any(is.na(var_names))){

    bad <- stringr::str_c(nii_files[is.na(var_names)], collapse = ", ")
    .glue_stop("Could not extract var_names with rgx_var = '{rgx_var}' from: {bad}")

  }

  duplicates <- names(table(var_names)[table(var_names) > 1])
  if(length(duplicates) != 0){

    duplicates <- stringr::str_c(duplicates, collapse = ", ")
    .glue_stop("Duplicated variable names with rgx_var = '{rgx_var}': {duplicates}")

  }

  # read and join files
  .glue_message("Reading files. n = {length(nii_files)}", verbose = verbose)

  vbl <-
    purrr::map2(
      .x = nii_paths,
      .y = var_names,
      .f = ~ nifti_to_vbl(x = .x, var = .y, rm0 = rm0, verbose = verbose)
    ) %>%
    purrr::reduce(.x = ., .f = join_vibbles, join = join, .rfn = .rfn)

  return(vbl)

}


#' @title Convert a vibble variable to a 3D array.
#' @description
#' Create a dense 3D numeric array from a voxel-wise variable stored in a
#' \link{vibble}, optionally filling missing voxels and enforcing a specific
#' output orientation.
#'
#' @details
#' The function reconstructs a full voxel grid from the coordinate limits
#' stored in \code{ccs_limits(vbl)} and fills it with the values of \code{var}.
#' Any voxel locations not present in \code{vbl} are set to \code{missing}.
#'
#' Before reshaping, the variable \code{var} is encoded as numeric:
#' \itemize{
#'   \item logical: converted to \code{0/1}
#'   \item factor / non-numeric: converted to factor (if needed) and then
#'         to integer level codes
#'   \item numeric: used as is
#' }
#'
#' The output orientation is controlled by the \code{orientation} argument, a
#' three-letter code (e.g. \code{"LIP"}) describing the anatomical direction
#' of the \code{x}, \code{y}, and \code{z} axes. If \code{orientation} is
#' \code{NULL}, the function falls back to the original orientation stored as
#' an attribute of the vibble. Using \code{ccs_limits()} and \code{ccs_mapping()},
#' axes are flipped as required so that the resulting array matches the desired
#' orientation. The dense voxel grid is then reshaped to a 3D array via
#' \code{reshape2::acast()}.
#'
#' The function assumes:
#' \itemize{
#'   \item \code{vbl} is a valid \link{vibble} with integer \code{x}, \code{y},
#'         \code{z} coordinates.
#'   \item \code{ccs_limits(vbl)} returns numeric ranges for all three axes.
#'   \item \code{orientation} passes \link{valid_orientation}().
#' }
#'
#' @param var
#' Name of the voxel-wise variable in \code{vbl} to be converted to an array.
#'
#' @param missing
#' Numeric value used to fill voxel locations that are not present in
#' \code{vbl}. Defaults to \code{0}.
#'
#' @param orientation
#' Optional character string specifying the desired output orientation
#' (e.g. \code{"LIP"}). If \code{NULL}, the original orientation attribute of
#' \code{vbl} is used.
#'
#' @inheritParams vbl_doc
#'
#' @return
#' A numeric 3D array containing the values of \code{var}, arranged according
#' to the specified orientation and with missing voxels filled by
#' \code{missing}.
#'
#' @examples
#' \dontrun{
#' # Convert a numeric variable to a 3D array in original orientation
#' arr <- vbl_to_array(vbl, var = "t1")
#'
#' # Convert a mask variable and enforce LIP orientation, using 0 as background
#' arr_mask <- vbl_to_array(vbl, var = "mask", missing = 0, orientation = "LIP")
#' }
#'
#' @export

vbl_to_array <- function(vbl,
                         var,
                         orientation,
                         missing = 0,
                         verbose = vbl_opts("verbose")
                         ){

  # get and check limits
  is_ccs_limits(ccs_limits(vbl))
  ccs_lim <- ccs_limits(vbl)

  valid_orientation(orientation, error = TRUE)
  .glue_message("Output orientation: {orientation}", verbose = verbose)

  # encode variable as numeric
  # lgl -> c(0,1)
  # fct -> level indices
  if(!is.numeric(vbl[[var]])){

    vbl[[var]] <- as.numeric(vbl[[var]])

  }

  # reconstruct full voxel data.frame
  full_vbl <-
    tidyr::expand_grid(
      x = seq_range(ccs_lim$x),
      y = seq_range(ccs_lim$y),
      z = seq_range(ccs_lim$z)
      ) %>%
    dplyr::left_join(x = ., y = vbl[,c(vbl_ccs_axes, var)], by = vbl_ccs_axes) %>%
    dplyr::rename(v = !!rlang::sym(var)) %>%
    dplyr::mutate(v = tidyr::replace_na(v, replace = {{ missing }}))

  # reorient voxel data.frame if required
  pointers <- stringr::str_split_1(orientation, pattern = "")

  form_prel <- vector("character", length = 0)
  ccs_map <- ccs_orientation_mapping
  ccs_map_fx <- ccs_orientation_mapping_fix

  # adjust the axes if required
  # for every pointer in the desired output orientation ...
  for(p in pointers){

    # ... extract the voxel orientation axis the pointer belongs to (e.g. R/L)
    axis <- purrr::keep(.x = ccs_map, .p = ~ p %in% .x)

    # ... get the ccs axis label (e.g. x)
    ccs_axis <- names(axis)

    # if the desired direction of the pointer does not correspond to the
    # fixed orientation mapping XYZ=LIP, flip
    if(p != ccs_map_fx[[ccs_axis]]){

      full_vbl[[ccs_axis]] <- max(ccs_lim[[ccs_axis]]) - full_vbl[[ccs_axis]] + 1

    }

    # add the required CCS axis to the formula in the desired order
    form_prel <- c(form_prel, ccs_axis)

  }

  form_final <- as.formula(paste0(form_prel, collapse = " ~ "))

  array_out <- reshape2::acast(data = full_vbl, formula = form_final, value.var = "v")

  dimnames(array_out) <- NULL

  return(array_out)

}



