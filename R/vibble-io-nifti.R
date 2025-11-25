# I/O and conversion between arrays and vibbles.



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
#' @param vbl A \link{vibble}.
#' @param var
#' Name of the variable in `vbl` to export as a NIfTI volume.
#' @param path_out
#' Optional character path to the output NIfTI file (`.nii.gz`). If `NULL`,
#' no file is written.
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
#' @param verbose
#' Logical. If `TRUE`, print messages about LUT handling, writing, and
#' external command execution.
#' @param ...
#' Additional arguments. Currently supports `path_ref`, a character path to
#' an explicit reference NIfTI that is used instead of `attr(vbl, "nifti")`.
#'
#' @return
#' Invisibly returns the NIfTI object whose data array has been replaced. If
#' `path_target` has been used to resample the NIfTI object the output
#' is read in from `path_out` and is returned.
#'
#' @importFrom glue glue
#'
#' @examples
#' \dontrun{
#' # Write a numeric variable back to the original reference space
#' make_nifti(vbl, var = "t1_score", path_out = "t1_score.nii.gz")
#'
#' # Resample into a target space using mri_vol2vol
#' make_nifti(
#'   vbl,
#'   var        = "risk",
#'   path_out   = "risk_mni.nii.gz",
#'   path_target = "MNI152_T1_1mm.nii.gz",
#'   path_fs    = "/Applications/freesurfer/7.4.0"
#' )
#' }
#'
#' @export

make_nifti <- function(vbl,
                       var,
                       path_out = NULL,
                       path_lut = NULL,
                       path_target = NULL,
                       dir_temp = getwd(),
                       path_fs = "/Applications/freesurfer/7.4.0",
                       interp = NULL,
                       missing = 0,
                       verbose = TRUE,
                       ...){

  type <- var_type(vbl[[var]])

  if(is.character(path_target)){ stopifnot(file.exists(path_target)) }

  path_ref = list(...)[["path_ref"]]
  if(is.character(path_ref)){

    nii <- oro.nifti::readNIfTI(fname = path_ref, reorient = FALSE)

  } else {

    nii <- attr(vbl, which = "nifti")

    if(!oro.nifti::is.nifti(nii)){

      stop("Requiring `path_ref` cause `attr(vbl, which = 'nifti')` is invalid for this vibble.")

    }

  }

  # exchange data array
  nii@.Data <-
    vbl_to_array(
      vbl = vbl,
      var = var,
      orientation = RNifti::orientation(nii),
      missing = missing
    )

  # write to disk if required
  if(is.character(path_out)){

    # handle LUT if required
    if(type == "label" & !is.na(path_lut)){

      lut <- make_lut(vbl, var = var)

      if(!is.character(path_lut)){

        path_lut <- stringr::str_replace(path_out, "\\.nii\\.gz$", "_lut.csv")

      }

      stopifnot(stringr::str_detect(path_lut, "_lut.csv$"))

      glue_message("Writing LUT to '{path_lut}'.", verbose)

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

      glue_message("Writing NIFTI to '{path_out}' with:\n {stringr::str_replace_all(cmd,';', ';\n')}")
      out <- system(cmd, show.output.on.console = verbose)

      unlink(path_nii_mov)

      nii <- oro.nifti::readNIfTI(fname = path_out, reorient = FALSE)

      # directly save as output
    } else {

      if(verbose){ message(glue("Writing NIFTI to '{path_out}'."))}

      oro.nifti::writeNIfTI(nii, filename = stringr::str_remove(path_out, ".nii.gz"))

    }

  }

  # return invisibly
  invisible(nii)

}


#' @title Convert a NIfTI image into a vibble.
#' @description
#' Create a \link{vibble} from a NIfTI object or file path by melting the voxel
#' data into long format, assigning coordinate axes, applying orientation
#' corrections, converting variable types, and optionally applying a lookup
#' table (LUT), removing zero-valued voxels, and adding voxel IDs.
#'
#' @details
#' The function accepts either a NIfTI object or a character file path. When a
#' path is supplied, the file is read without reorientation. If the original
#' NIfTI object has \code{@reoriented == TRUE}, a warning is issued because
#' voxel orientation may be unreliable.
#'
#' Coordinate directions are extracted from the NIfTI header via
#' \code{RNifti::orientation()}. Axes are renamed to \code{x}, \code{y},
#' \code{z} according to the package's canonical coordinate mapping
#' (\code{ccs_orientation_mapping}). Axes that point in opposite anatomical
#' directions relative to the desired LIP convention are flipped.
#'
#' The voxel values of \code{var} are inspected and converted:
#' \itemize{
#'   \item binary 0/1 values → logical
#'   \item any other non-numeric values → factor (if not already), then numeric
#'   (unless processed by a LUT)
#' }
#'
#' If \code{lut} is supplied and the variable appears to represent labeled
#' classes, the lookup table is applied via \code{map_lut()}. Otherwise the
#' variable is left numeric with a message (depending on \code{verbose}).
#'
#' The vibble is stored with class \code{"vbl"} and the following attributes:
#' \itemize{
#'   \item \code{orientation_orig}: orientation string extracted from the NIfTI object.
#'   \item \code{ccs_mapping}: canonical voxel-to-axis mapping (currently fixed as LIP).
#'   \item \code{ccs_limits}: coordinate limits for \code{x}, \code{y}, \code{z}.
#'   \item \code{nifti}: the NIfTI object with its data array emptied, retained
#'                     for metadata compatibility.
#'   \item \code{var_smr}: variable summary list created by \code{summarize_var()}.
#' }
#'
#' If \code{rm0 = TRUE}, voxels with value 0 are removed. If \code{add_id = TRUE},
#' a unique voxel ID column is added via \code{id_add()}.
#'
#' @param nifti
#' A NIfTI object or a character path to a \code{.nii} or \code{.nii.gz} file.
#'
#' @param var
#' Character. Name of the voxel-wise variable to extract and store as the main
#' variable in the vibble.
#'
#' @param ordered
#' Logical or \code{NULL}. If a lookup table is provided, controls whether the
#' resulting labeled factor is ordered. If \code{NULL}, ordering defaults to
#' \code{FALSE}.
#'
#' @param add_id
#' Logical. If \code{TRUE}, add a unique voxel ID column.
#'
#' @param rm0
#' Logical. If \code{TRUE}, remove voxels with value 0 from the resulting
#' vibble. Replaces the deprecated argument \code{black_rm}.
#'
#' @inherit vbl_doc params
#'
#' @param ...
#' Additional arguments; currently supports the deprecated argument
#' \code{black_rm}.
#'
#' @return
#' A \link{vibble} containing voxel coordinates, the extracted voxel variable,
#' and metadata attributes describing orientation, coordinate limits, lookup
#' table usage, and per-variable summaries.
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
nifti_to_vbl <- function(nifti,
                         var = "value",
                         lut = NULL,
                         ordered = FALSE,
                         add_id = TRUE,
                         rm0 = TRUE,
                         verbose = TRUE,
                         ...){

  if("black_rm" %in% names(list(...))){

    rm0 <- list(...)[["black_rm"]]
    warning("`black_rm` is deprecated in favor of `rm0`.")

  }

  if(is.character(nifti)){

    nifti <- oro.nifti::readNIfTI(fname = nifti, reorient = FALSE)

  }

  if(isTRUE(nifti@reoriented)){

    warning("Input object has @reoriented == TRUE. Output orientation is not reliable.")

  }

  msg <- "Created a vibble for variable '{var}'."

  # use to force XYZ==LIP orientation in the data.frame
  flip_check <- list(x = "R", y = "S", z = "A")

  pointers <-
    RNifti::orientation(nifti, useQuaternionFirst = TRUE) %>%
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

  # create vbl
  vbl <-
    reshape2::melt(nifti@.Data, varnames = unname(pointers), value.name = var) %>%
    tibble::as_tibble() %>%
    dplyr::select(!!!pointers[c(ccs_labels)], !!rlang::sym(var))

  # identify logical input
  if(is_mask_candidate(vbl[[var]])){

    vbl[[var]] <- as.logical(vbl[[var]])

  }

  # set attributes
  class(vbl) <- c("vbl", class(vbl))
  attr(vbl, which = "orientation_orig") <- RNifti::orientation(nifti, useQuaternionFirst = TRUE)
  attr(vbl, which = "ccs_mapping") <- list(x = "L", y = "I", z = "P")  # currently fixed XYZ = LIP
  attr(vbl, which = "ccs_limits") <- purrr::map(vbl[,c(ccs_labels)], .f = range)

  nifti@.Data <- array()
  attr(vbl, which = "nifti") <- nifti # required meta data for backwards compatibility

  # post process
  for(fa in flip_axes){

    vbl[[fa]] <- max(vbl[[fa]]) - vbl[[fa]] + 1

  }

  if(is_label_candidate(vbl[[var]])){

    if(is.null(lut)){

      msg <- paste0(msg, " Interpreting as numeric - no LUT provided.", collasep = "")

    } else {

      msg <- paste0(msg, " Interpreting as label with provided LUT.")

      vbl <- map_lut(vbl, var = var, lut = lut, ordered = isTRUE(ordered), verbose = verbose)

    }

  } else { # numeric or mask

    var_type <- ifelse(is_mask_var(vbl[[var]]), "mask", "numeric")
    msg <- paste0(msg, " Interpreting as {var_type}.")

  }

  glue_message(msg, verbose = verbose)

  var_smr <- list()
  var_smr[[var]] <- summarize_var(vbl[[var]])
  attr(vbl, which = "var_smr") <- var_smr

  if(isTRUE(rm0)){

    vbl <- vbl[vbl[[var]] != 0, ]

  }

  if(isTRUE(add_id)){ vbl <- id_add(vbl) }

  # output
  return(vbl)

}

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
#' @param input
#' Character. Either a directory containing NIfTI files or a character vector
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
#' @param recursive
#' Logical. If \code{TRUE} and `input` is a directory it is searched recursively for NIfTI files.
#'
#' @param rm0
#' Logical. Forwarded to \link{nifti_to_vbl()}. If \code{TRUE}, zero-valued
#' voxels are removed from each individual vibble.
#'
#' @param join
#' Join method passed to \link{join_vibbles()}. One of
#' \code{"full"}, \code{"inner"}, \code{"left"}, or \code{"right"}.
#'
#' @param verbose
#' Logical. If \code{TRUE}, print messages about file discovery and reading.
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
niftis_to_vbl <- function(input,
                          rgx_fp = ".*",
                          rgx_var = ".*",
                          recursive = FALSE,
                          rm0 = FALSE,
                          join = "full",
                          verbose = TRUE){

  stopifnot(is.character(input))

  if(length(input) == 1){ # should be directory

    stopifnot(dir.exists(input))

    nii_paths <-
      list.files(path = input, full.names = TRUE, recursive = recursive) %>%
      stringr::str_subset(pattern = ".nii.gz$")

    if(length(nii_paths) == 0){ glue_stop("No NIFTI files found in '{input}'.") }

  } else { # set of nifti file paths

    nii_paths <- stringr::str_subset(input, pattern = ".nii.gz$")

    if(length(nii_paths) == 0){ glue_stop("No NIFTI files found in `input`.") }

  }

  # subset file names by regex
  stopifnot(is.character(rgx_fp) && length(rgx_fp) == 1)
  nii_paths <- stringr::str_subset(nii_paths, pattern = rgx_fp)

  if(length(nii_paths) == 0){ glue_stop("No NIFTI files remain with rgx_fp = '{rgx_fp}'.") }

  # handle varnames
  nii_files <- stringr::str_remove_all(basename(nii_paths), pattern = ".nii.gz$")

  stopifnot(is.character(rgx_var) && length(rgx_var) == 1)
  var_names <- stringr::str_extract(nii_files, pattern = rgx_var)

  if(any(is.na(var_names))){

    bad <- stringr::str_c(nii_files[is.na(var_names)], collapse = ", ")
    glue_stop("Could not extract var_names with rgx_var = '{rgx_var}' from: {bad}")

  }

  duplicates <- names(table(var_names)[table(var_names) > 1])
  if(length(duplicates) != 0){

    duplicates <- stringr::str_c(duplicates, collapse = ", ")
    glue_stop("Duplicated variable names with rgx_var = '{rgx_var}': {duplicates}")

  }

  # read and join files
  glue_message("Reading files. n = {length(nii_files)}", verbose = verbose)

  vbl <-
    purrr::map2(
      .x = nii_paths,
      .y = var_names,
      .f = ~ nifti_to_vbl(nifti = .x, var = .y, rm0 = rm0, add_id = FALSE, verbose = verbose)
    ) %>%
    purrr::reduce(.x = ., .f = join_vibbles, join = join)

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
#' @param vbl
#' A \link{vibble} containing voxel coordinates and at least one voxel-wise
#' variable.
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
                         missing = 0,
                         orientation = NULL){

  cvars <- c("x", "y", "z")

  # get and check limits
  ccs_lim <- ccs_limits(vbl)

  stopifnot(all(cvars %in% names(ccs_lim)))
  stopifnot(all(purrr::map_lgl(ccs_lim, .f = is.numeric)))
  stopifnot(all(purrr::map_lgl(ccs_lim, .f = ~ length(.x)==2)))

  # get and check desired output orientation
  if(!is.character(orientation)){

    orientation <- attr(vdf, which = "orig_orientation")

  }

  valid_orientation(orientation, error = TRUE)
  message(glue::glue("Output orientation: {orientation}"))

  # encode variable as numeric
  if(is.logical(vbl[[var]])){

    vbl[[var]] <- as.numeric(vbl[[var]])

  } else if(!is.numeric(vbl[[var]])){

    if(!is.factor(vbl[[var]])){

      vbl[[var]] <- as.factor(vbl[[var]])

    }

    vbl[[var]] <- as.numeric(vbl[[var]])

  }

  # reconstruct full voxel data.frame
  lim_seq <- function(l){ min(l):max(l) } # helper

  full_vbl <-
    tidyr::expand_grid(x = lim_seq(limits$x), y = lim_seq(limits$y), z = lim_seq(limits$z)) %>%
    dplyr::left_join(x = ., y = vbl[,c(ccs_labels, var)], by = ccs_labels) %>%
    dplyr::rename(v = !!rlang::sym(var)) %>%
    dplyr::mutate(v = tidyr::replace_na(v, replace = {{missing}}))

  # reorient voxel data.frame if required
  pointers <- stringr::str_split_1(orientation, pattern = "")

  form_prel <- vector("character", length = 0)
  ccs_map <- ccs_mapping(vbl)

  # adjust the axes if required
  # for every pointer in the desired output orientation ...
  for(p in pointers){

    # ... extract the voxel orientation axis the pointer belongs to (e.g. R/L)
    axis <- purrr::keep(.x = ccs_orientation_mapping, .p = ~ p %in% .x)

    # ... get the ccs axis label (e.g. x)
    ccs_axis <- names(axis)

    # if wrong direction -> flip
    if(ccs_map[[ccs_axis]] != p){

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



