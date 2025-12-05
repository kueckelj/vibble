# Core data structure and constructors.

#' @keywords internal
#' @export
vbl_attr <- c("ccs_limits", "ccs_steps")

#' @keywords internal
#' @export
vbl_attr2D <- c(vbl_attr, c("screen_limits", "offset_col", "offset_row", "plane"))

# core constructors and tests ---------------------------------------------

#' @export
new_vbl <- function(data,
                    ccs_limits,
                    ccs_steps){

  stopifnot(is.data.frame(data))

  data$x <- as.integer(data$x)
  data$y <- as.integer(data$y)
  data$z <- as.integer(data$z)

  # set class: vbl on top of existing classes (e.g. tbl_df, data.frame)
  class(data) <- c("vbl", class(data))

  # attach attributes
  ccs_limits(data)  <- ccs_limits
  ccs_steps(data) <- ccs_steps

  # validate and return
  validate_vbl(data)

}

#' @export
validate_vbl <- function(x){

  # must be a data.frame
  if(!is.data.frame(x)){
    stop("A vibble must be based on a data frame.", call. = FALSE)
  }

  # required spatial variables
  req_spatial <- c("x", "y", "z")

  if(!all(req_spatial %in% colnames(x))){
    stop("A vibble must contain spatial variables `x`, `y`, and `z`.", call. = FALSE)
  }

  # spatial variables must be numeric (typically integer) and non-missing
  for(v in req_spatial){

    if(!is.numeric(x[[v]])){
      stop("Spatial variable `", v, "` must be numeric.", call. = FALSE)
    }

    if(any(is.na(x[[v]]))){
      stop("Spatial variable `", v, "` must not contain missing values.", call. = FALSE)
    }

  }

  # must have at least one non-spatial variable
  if(length(setdiff(colnames(x), req_spatial)) == 0L){
    stop("A vibble must contain at least one data variable besides `x`, `y`, and `z`.", call. = FALSE)
  }

  # required attributes: ccs_limits and ccs_mapping
  ccs_limits  <- attr(x, "ccs_limits", exact = TRUE)

  if(is.null(ccs_limits) || !is.list(ccs_limits) || !all(req_spatial %in% names(ccs_limits))){
    stop("Attribute `ccs_limits` must be a list with elements `x`, `y`, and `z`.", call. = FALSE)
  }

  # check that coordinates lie within ccs_limits
  for(v in req_spatial){

    lim <- range(ccs_limits[[v]])

    if(any(x[[v]] < lim[1L] | x[[v]] > lim[2L])){
      stop("Values of `", v, "` must lie within the declared `ccs_limits`.", call. = FALSE)
    }

  }

  # check that ccs_steps is provided
  ccs_steps  <- ccs_steps(x)

  if(is.null(ccs_steps) || !is.list(ccs_steps) || !all(req_spatial %in% names(ccs_steps))){
    stop("Attribute `ccs_steps` must be a list with elements `x`, `y`, and `z`.", call. = FALSE)
  }

  # check that steps are valid
  for(v in req_spatial){

    if(!is.double(ccs_steps[[v]]) || !ccs_steps[[v]] > 0){
      stop("CCS step of ", v, " isn't a double > 0.", call. = FALSE)
    }

  }

  x

}

#' @export
is_vbl <- function(x){ inherits(x, "vbl") }

#' @export
is_vbl2D <- function(x){ inherits(x, "vbl2D") }

# -------------------------------------------------------------------
# Print method
# -------------------------------------------------------------------

#' @importFrom purrr map_lgl
#' @export
print.vbl <- function(x, ...){

  # print as tibble first
  NextMethod()

  if (isTRUE(vbl_opts("verbose"))) {

    ccs_limits <- attr(x, "ccs_limits", exact = TRUE)
    ccs_steps  <- attr(x, "ccs_steps",  exact = TRUE)

    xl <- range(ccs_limits[["x"]])
    yl <- range(ccs_limits[["y"]])
    zl <- range(ccs_limits[["z"]])

    xr <- range(x$x, na.rm = TRUE)
    yr <- range(x$y, na.rm = TRUE)
    zr <- range(x$z, na.rm = TRUE)

    # collect all mins/max for alignment
    lim_min <- c(xl[1L], yl[1L], zl[1L])
    lim_max <- c(xl[2L], yl[2L], zl[2L])
    rng_min <- c(xr[1L], yr[1L], zr[1L])
    rng_max <- c(xr[2L], yr[2L], zr[2L])
    step_mm <- c(ccs_steps$x, ccs_steps$y, ccs_steps$z)

    # widths
    w_lim_min <- max(nchar(as.character(lim_min)))
    w_lim_max <- max(nchar(as.character(lim_max)))
    w_rng_min <- max(nchar(as.character(rng_min)))
    w_rng_max <- max(nchar(as.character(rng_max)))
    w_step    <- max(nchar(as.character(step_mm)))

    # formatters
    fmt_lim_min <- function(v) sprintf("%*d", w_lim_min, v)
    fmt_lim_max <- function(v) sprintf("%*d", w_lim_max, v)
    fmt_rng_min <- function(v) sprintf("%*d", w_rng_min, v)
    fmt_rng_max <- function(v) sprintf("%*d", w_rng_max, v)
    fmt_step    <- function(v) sprintf("%*s", w_step, v)

    # data-variable stats
    dvars <- vars_data(x)
    n_mask <- sum(purrr::map_lgl(x[dvars], is_mask_var))
    n_num  <- sum(purrr::map_lgl(x[dvars], is_numeric_var))
    n_lab  <- sum(purrr::map_lgl(x[dvars], is_label_var))

    info <- paste0(
      "<vibble> vbl object\n",
      "  axes (ccs_limits / data range / step)\n",
      sprintf(
        "    x -> %-9s: Lim[%s,%s], Rng[%s,%s], Step %s mm\n",
        "Left",
        fmt_lim_min(xl[1L]), fmt_lim_max(xl[2L]),
        fmt_rng_min(xr[1L]), fmt_rng_max(xr[2L]),
        fmt_step(step_mm[1])
      ),
      sprintf(
        "    y -> %-9s: Lim[%s,%s], Rng[%s,%s], Step %s mm\n",
        "Inferior",
        fmt_lim_min(yl[1L]), fmt_lim_max(yl[2L]),
        fmt_rng_min(yr[1L]), fmt_rng_max(yr[2L]),
        fmt_step(step_mm[2])
      ),
      sprintf(
        "    z -> %-9s: Lim[%s,%s], Rng[%s,%s], Step %s mm\n",
        "Posterior",
        fmt_lim_min(zl[1L]), fmt_lim_max(zl[2L]),
        fmt_rng_min(zr[1L]), fmt_rng_max(zr[2L]),
        fmt_step(step_mm[3])
      ),
      "  data variables: ", length(dvars),
      " (", n_lab, " label, ",
      n_mask, " mask, ",
      n_num, " numeric)\n"
    )

    cat(pillar::style_subtle(info))
  }

  invisible(x)
}

#' @export
print.vbl2D <- function(x, ...){

  n_slice <- dplyr::n_distinct(x$slice)

  ccs_limits <- attr(x, "ccs_limits", exact = TRUE)

  # spatial ranges
  xr <- range(ccs_limits[["x"]])
  yr <- range(ccs_limits[["y"]])
  zr <- range(ccs_limits[["z"]])

  # simple variable type counts
  dvars <- vars_data(x)

  n_mask <- sum(map_lgl(x[dvars], is_mask_var))
  n_num  <- sum(map_lgl(x[dvars], is_numeric_var))
  n_lab  <- sum(map_lgl(x[dvars], is_label_var))

  # first print as tibble / data.frame
  NextMethod()

  # optional vibble meta information in tibble-style grey
  if(isTRUE(vbl_opts("verbose"))){

    offset_string <- paste0("(", offset_col(x), "|", offset_row(x), ")")

    limits_string <-
      purrr::map_chr(
        .x = screen_limits(x),
        .f = function(l, a){

          ifelse(is.null(l), "none", paste0(as.integer(l[[1]]), ":", as.integer(l[[2]])))

        } )

    limits_string <- paste0("(",limits_string[1], "|", limits_string[2], ") <integer.truncated>")

    info <-
      paste0(
        "<vibble2D> vbl2D object\n",
        "  plane : ",  vbl_planes_pretty[plane(x)], " (", plane(x), ")\n",
        "  slices:           ", n_slice, "\n",
        "  screen (col|row): ", limits_string, "\n",
        "  offset (col|row): ", offset_string, "\n",
        "  data variables: ", length(dvars), " (",
        n_lab,  " label, ",
        n_mask, " mask, ",
        n_num,  " numeric)\n "
      )

    cat(pillar::style_subtle(info))

  }

  invisible(x)

}




# -------------------------------------------------------------------
# Protection of spatial coordinates and names in base R
# -------------------------------------------------------------------

# $<- : block modification of x, y, z via vbl$x <- value

#' @keywords internal
#' @export
`$<-.vbl` <- function(x, name, value){

  if(name %in% c("x", "y", "z")){

    stop("Spatial coordinates `x`, `y`, and `z` cannot be modified in a vibble.", call. = FALSE)

  }

  NextMethod()

}

# [[<- : block modification of x, y, z via vbl[["x"]] <- value
#' @keywords internal
#' @export
`[[<-.vbl` <- function(x, i, value){

  if(is.character(i) && i %in% ccs_labels){

    stop("Spatial coordinates `x`, `y`, and `z` cannot be modified in a vibble.", call. = FALSE)

  }

  if(is.numeric(i) && names(x)[i] %in% ccs_labels){

    stop("Spatial coordinates `x`, `y`, and `z` cannot be modified in a vibble.", call. = FALSE)

  }

  NextMethod()

}

# [<- : block modification of x, y, z via vbl[, "x"] <- value
#' @keywords internal
#' @export
`[<-.vbl` <- function(x, i, j, value){

  if(!is.character(j) && any(j %in% ccs_labels)){

    stop("Spatial coordinates `x`, `y`, and `z` cannot be modified in a vibble.", call. = FALSE)

  }

  if(!is.numeric(j) && any(names(x)[j] %in% ccs_labels)){

    stop("Spatial coordinates `x`, `y`, and `z` cannot be modified in a vibble.", call. = FALSE)

  }

  NextMethod()

}

# names<- : block renaming of x, y, z
#' @keywords internal
#' @export
`names<-.vbl` <- function(x, value){

  old <- names(x)

  if(length(old) == length(value)){

    changed <- old != value

    if(any(changed & old %in% ccs_labels)){

      stop("Spatial coordinates `x`, `y`, and `z` cannot be modified in a vibble.", call. = FALSE)

    }

  }

  NextMethod()

}


# -------------------------------------------------------------------
# Protection of spatial coordinates and names in dplyr
# -------------------------------------------------------------------

#' @keywords internal
.vbl_check_spatial_unchanged <- function(old, new){

  # must still be present
  if(!all(ccs_labels %in% colnames(new))){

    rlang::abort("Spatial coordinates `x`, `y`, and `z` must not be removed or renamed.")

  }

  # must not be modified
  for(v in ccs_labels){

    if(!identical(old[[v]], new[[v]])){

      rlang::abort(paste0("Spatial coordinate `", v, "` must not be modified."))

    }

  }

  invisible(TRUE)

}

#' @export
mutate.vbl <- function(.data, ...){

  old <- .data

  # let dplyr do the work on the underlying tibble
  new <- dplyr::mutate(tibble::as_tibble(.data), ...)

  .vbl_check_spatial_unchanged(old, new)

  new <- .vbl_reconstruct(old, new)

  return(new)

}

#' @export
transmute.vbl <- function(.data, ...){

  old <- .data

  new <- dplyr::transmute(tibble::as_tibble(.data), ...)

  .vbl_check_spatial_unchanged(old, new)

  new <- .vbl_reconstruct(old, new)

  return(new)

}

#' @export
rename.vbl <- function(.data, ...){

  old <- .data

  new <- dplyr::rename(tibble::as_tibble(.data), ...)

  .vbl_check_spatial_unchanged(old, new)

  new <- .vbl_reconstruct(old, new)

  return(new)

}

# -------------------------------------------------------------------
# Protection of spatial coordinates AND attributes in grouped vibbles
# -------------------------------------------------------------------

#' @keywords internal
.vbl_reconstruct <- function(old, new){

  # keep vibble-specific attributes
  keep_attrs <- vbl_attr

  for(a in keep_attrs){

    val <- attr(old, a, exact = TRUE)

    if(!is.null(val)){

      attr(new, a) <- val

    }

  }

  # prepend vibble classes to existing ones
  if(inherits(old, "grouped_vbl")){

    class(new) <- unique(c("grouped_vbl", "vbl", class(new)))

  } else {

    class(new) <- unique(c("vbl", class(new)))

  }

  return(new)

}

#' @export
group_by.vbl <- function(.data, ..., .add = FALSE, .drop = dplyr::group_by_drop_default(.data)){

  # start from tibble, apply dplyr grouping
  new <- dplyr::group_by(tibble::as_tibble(.data), ..., .add = .add, .drop = .drop)

  # re-attach vibble attributes and add grouped_vbl class
  new <- .vbl_reconstruct(.data, new)

  class(new) <- unique(c("grouped_vbl", class(new)))

  return(new)

}


#' @export
ungroup.grouped_vbl <- function(x, ...){

  new <- dplyr::ungroup(tibble::as_tibble(x), ...)

  # drop grouped_vbl, keep vbl and underlying classes
  new <- .vbl_reconstruct(x, new)

  class(new) <- class(new)[!class(new) == "grouped_vbl"]

  return(new)

}


#' @export
mutate.grouped_vbl <- function(.data, ...){

  old <- .data

  # temporarily strip vibble-specific classes so mutate() does not dispatch here again
  cls <- class(.data)
  class(.data) <- setdiff(cls, c("grouped_vbl", "vbl"))

  new <- dplyr::mutate(.data, ...)

  # restore class stack
  class(new) <- cls

  # check spatial coords unchanged
  old_ung <- dplyr::ungroup(tibble::as_tibble(old))
  new_ung <- dplyr::ungroup(tibble::as_tibble(new))
  .vbl_check_spatial_unchanged(old_ung, new_ung)

  # reattach attributes
  new <- .vbl_reconstruct(old, new)

  return(new)

}



#' @export
transmute.grouped_vbl <- function(.data, ...){

  old <- .data

  new <- dplyr::transmute(tibble::as_tibble(.data), ...)

  old_ung <- dplyr::ungroup(tibble::as_tibble(old))
  new_ung <- dplyr::ungroup(tibble::as_tibble(new))

  .vbl_check_spatial_unchanged(old_ung, new_ung)

  new <- .vbl_reconstruct(old, new)

  class(new) <- unique(c("grouped_vbl", class(new)))

  return(new)

}



# -------------------------------------------------------------------
# Protection of attributes in grouped vibble2Ds
# -------------------------------------------------------------------

# essentially the same concept as for vbl, but protection of spatial variables x,y,z
# is not required and not desired (cause vbl2D does not have them)

#' @keywords internal
.vbl2D_reconstruct <- function(old, new){

  # keep vibble-specific attributes
  for(a in vbl_attr2D){

    val <- attr(old, a, exact = TRUE)

    if(!is.null(val)){

      attr(new, a) <- val

    }

  }

  # prepend vibble classes to existing ones
  if(inherits(old, "grouped_vbl2D")){

    class(new) <- unique(c("grouped_vbl2D", "vbl2D", class(new)))

  } else {

    class(new) <- unique(c("vbl2D", class(new)))

  }

  return(new)

}

#' @export
group_by.vbl2D <- function(.data, ..., .add = FALSE, .drop = dplyr::group_by_drop_default(.data)){

  # start from tibble, apply dplyr grouping
  new <- dplyr::group_by(tibble::as_tibble(.data), ..., .add = .add, .drop = .drop)

  # re-attach vibble attributes and add grouped_vbl class
  new <- .vbl2D_reconstruct(.data, new)

  class(new) <- unique(c("grouped_vbl2D", class(new)))

  return(new)

}

#' @export
transmute.grouped_vbl2D <- function(.data, ...){

  old <- .data

  new <- dplyr::transmute(.data, ...)

  old_ung <- dplyr::ungroup(tibble::as_tibble(old))
  out_ung <- dplyr::ungroup(tibble::as_tibble(new))

  new <- .vbl2D_reconstruct(old, new)

  class(new) <- unique(c("grouped_vbl2D", class(new)))

  return(new)

}


#' @export
mutate.grouped_vbl2D <- function(.data, ...){

  old <- .data

  # temporarily strip vibble-specific classes so mutate() does not dispatch here again
  cls <- class(.data)
  class(.data) <- setdiff(cls, c("grouped_vbl2D", "vbl2D"))

  new <- dplyr::mutate(.data, ...)

  # restore class stack
  class(new) <- cls

  # reattach attributes
  new <- .vbl2D_reconstruct(old, new)

  class(new) <- unique(c("grouped_vbl2D", class(new)))

  return(new)

}

#' @export
ungroup.grouped_vbl2D <- function(x, ...){

  new <- dplyr::ungroup(tibble::as_tibble(x), ...)

  new <- .vbl2D_reconstruct(x, new)

  class(new) <- class(new)[!class(new) == "grouped_vbl2D"]

  return(new)

}



# core functions and utilities --------------------------------------------

#' @title Add or reconstruct voxel IDs based on spatial coordinates
#' @description Create a unique integer voxel identifier from zero-padded
#' `x`, `y`, `z` coordinates or reconstruct spatial coordinates from such an ID.
#' See section Voxel ID encoding.
#'
#' @param arrange Logical. If `TRUE`, rows are arranged by the newly created `id`.
#' @param rm_coords Logical. If `TRUE`, remove the original coordinate columns after creating the ID.
#' @inherit vbl_doc params
#'
#' @return
#' For `id_add()`: A tibble with a new integer column `id`.
#' For `id_split()`: A tibble in which `id` has been split back into `x`, `y`, and `z`.
#'
#' @details
#' The width of each coordinate block is defined by the maximum observed coordinate
#' in the respective axis using \link{ccs_limits}(). Zero-padding ensures fixed-width
#' encoding, enabling safe concatenation and later reversal.
#'
#' `id_add()`:
#' \itemize{
#'   \item Pads `x`, `y`, `z` to fixed widths.
#'   \item Concatenates them into a single string.
#'   \item Converts the result to an integer `id`.
#' }
#'
#' `id_split()`:
#' \itemize{
#'   \item Verifies that `id` has the expected padded length.
#'   \item Splits the ID into fixed-width substrings.
#'   \item Converts each back into integer coordinates.
#' }
#'
#' These functions provide a deterministic and reversible mapping useful for joins,
#' indexing, hashing, and storage of voxel-based data.
#'
#' @note
#' IDs are not required for standard operations because each voxel is already uniquely
#' identified by its `x`, `y`, `z` coordinates. The integer ID is only a
#' convenience key and becomes useful when transferring information between
#' a \link{vibble} and its corresponding \link{vibble2D}, storing intermediate results,
#' or performing joins that benefit from a single-column identifier.
#'
#' An integer ID column as created with zero-padding has the advantage of more
#' efficient storage usage. See examples.
#'
#' @section Voxel ID encoding:
#' IDs are encoded as `<pseudodigit><x><y><z>`, where each of `x`, `y`, and `z`
#' is zero-padded to a fixed number of digits. The padding widths are determined
#' by `ccs_limits()`, which provides the maximal coordinate range in each axis
#' and therefore the number of digits required to represent all voxels.
#'
#' IDs are stored as integers rather than character strings to reduce memory
#' footprint. The leading pseudodigit (1) ensures that the full padded width is
#' preserved even after converting the ID to an integer, because integers cannot
#' store leading zeros. When decoding, the pseudodigit is removed and the remaining
#' string is split according to the known padding lengths, making the mapping reversible.
#'
#' @seealso \link{ccs_limits}()
#'
#' @examples
#' library(dplyr)
#' library(pryr)
#'
#' vbl <- example_vbl()
#'
#' object_size(vbl) # with x,y,z
#'
#' # Add ID
#' vbl_id <- id_add(vbl)
#'
#' object_size(vbl_id) # with id, x, y, z
#'
#' vbl_no_ccs <- select(vbl_id, -x, -y, -z)
#'
#' object_size(vbl_no_ccs) # only id
#'
#' # Reconstruct coordinates
#' vbl_split <- id_split(vbl_id)
#'
#' identical(vbl_split, vbl)
#'
#' @rdname id_add
#' @export
id_add <- function(vbl, rm_coords = FALSE){

  lim <- ccs_limits(vbl)

  # Determine padding width for each axis based on max coordinate
  pad_x <- nchar(max(lim[["x"]]))
  pad_y <- nchar(max(lim[["y"]]))
  pad_z <- nchar(max(lim[["z"]]))

  # Create zero-padded strings for each coordinate
  sx <- sprintf(fmt = paste0("%0", pad_x, "d"), vbl$x)
  sy <- sprintf(fmt = paste0("%0", pad_y, "d"), vbl$y)
  sz <- sprintf(fmt = paste0("%0", pad_z, "d"), vbl$z)

  # Combine into one integer ID
  # add a pseudo 1 infront to prevent the drop of leading 0 during integer conversion
  vbl$id <- as.integer(paste0(1, sx, sy, sz))

  if(isTRUE(rm_coords)){

    # call mutate.data.frame explicitly, to allow x,y,z removal
    dplyr:::select.data.frame(vbl, id, dplyr::everything(), -x, -y, -z)

  } else {

    dplyr::select(vbl, id, dplyr::everything())

  }

}

#' @rdname id_add
#' @export
id_split <- function(vbl){

  lim <- ccs_limits(vbl)

  # Determine padding widths
  pad_x <- nchar(max(lim[["x"]]))
  pad_y <- nchar(max(lim[["y"]]))
  pad_z <- nchar(max(lim[["z"]]))

  # Convert IDs to character
  id_chr <- sprintf(fmt = "%s", vbl$id)

  # Total width of padded ID including pseudo digit
  total_width <- 1 + pad_x + pad_y + pad_z

  # Safety check
  if(any(nchar(id_chr) != total_width)){

    stop("IDs do not match expected padded length.")

  }

  # Split into substrings, start at 2 due to pseudodigit
  # call mutate.data.frame explicitly, to allow x,y,z manipulation
  vbl <-
    dplyr:::mutate.data.frame(
      .data = vbl,
      x = as.integer(substr(id_chr, start = 2, stop = pad_x + 1)),
      y = as.integer(substr(id_chr, start = pad_x + 2, stop = pad_x + pad_y + 1)),
      z = as.integer(substr(id_chr, start = pad_x + pad_y + 2, stop = total_width))
    )

  dplyr::select(vbl, x, y, z, dplyr::everything(), -id)

}




read_vbl <- function(path){

  stopifnot(length(path) == 1 & grepl(".RDS$", path))

  vbl <- readRDS(path)

  if("id" %in% colnames(vbl)){

    vbl <- id_split(vbl)

  }

  return(vbl)

}


