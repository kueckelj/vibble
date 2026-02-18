


#' @title Validate series metadata
#' @description
#' Checks whether an object represents valid metadata for a series variable.
#' Series metadata defines a regular, ordered axis (e.g. time) along which
#' vector-valued voxel data are measured.
#'
#' @param x A list to validate.
#'
#' @return
#' Logical scalar. `TRUE` if `x` is valid series metadata, otherwise `FALSE`.
#'
#' @details
#' A valid series metadata object must be a named list containing:
#' \itemize{
#'   \item \code{axis}: Character scalar naming the axis (e.g. `"time"`).
#'   \item \code{unit}: Character scalar giving the unit of \code{step}.
#'   \item \code{step}: Positive numeric scalar giving the spacing between samples.
#'   \item \code{start}: Numeric scalar giving the axis start value.
#'   \item \code{end}: Numeric scalar giving the axis end value.
#' }
#'
#' Additionally, the range defined by \code{start} and \code{end} must be
#' compatible with \code{step}, such that a regular sequence can be formed.
#'
#' @export
is_series_meta <- function(x){

  if(!is.list(x)){
    return(FALSE)
  }

  x <- purrr::discard(.x = x, .p = ~ length(dim(.x)) != 1)
  x <- purrr::discard(.x = x, .p = ~ is.na(.x) | length(.x) != 1)

  required <- c("axis", "unit", "step", "start", "end")

  if(!all(required %in% names(x))) return(FALSE)

  chr <- purrr::map(.x = x[c("axis", "unit")], .f = is.character)
  if(!all(chr)) return(FALSE)

  num <- purrr::map(.x = x[c("step", "start", "end")], .f = is.numeric)
  if(!all(num)) return(FALSE)

  int <- purrr::map(.x = x[c("start", "end")], .f = ~ is.integer(.x) && .x >= 0)
  if(!all(int)) return(FALSE)

  return(TRUE)

}


#' @title Check for series variable
#' @description
#' Tests whether an object represents a valid series variable, defined as a
#' collection of per-voxel data frames with a consistent structure across voxels.
#'
#' @param x An object to test.
#'
#' @return
#' Logical scalar. `TRUE` if `x` is a valid series variable, otherwise `FALSE`.
#'
#' @details
#' A valid series variable must satisfy all of the following:
#' \itemize{
#'   \item Be a list or a data.frame.
#'   \item Contain only data.frame elements.
#'   \item All elements have the same number of rows.
#'   \item All elements have the same number of columns.
#' }
#'
#' This structure represents vector-valued measurements per voxel that can be
#' unnested along a common series axis.
#'
#' @export
is_series_var <- function(x){

  if(!is.list(x) & !is.data.frame(x)) return(FALSE)

  if(!all(purrr::map_lgl(x, is.data.frame))) return(FALSE)

  nrows <- purrr::map_int(x, nrow)
  if(dplyr::n_distinct(nrows) != 1) return(FALSE)

  ncols <- purrr::map_int(x, ncol)
  if(dplyr::n_distinct(ncols) != 1) return(FALSE)

  return(TRUE)

}


#' @title Create series metadata from a NIfTI image
#' @description
#' Derives series metadata from a 4D NIfTI object read with
#' `oro.nifti::readNIfTI()`. The resulting metadata describes a regular
#' series axis.
#'
#' @param nifti A 4D NIfTI object of class `nifti`.
#'
#' @return
#' A named list containing series metadata.
#'
#' @details
#' The returned list contains:
#' \itemize{
#'   \item \code{axis}: The series axis name. Always `"time"`.
#'   \item \code{step}: Spacing between samples, taken from \code{pixdim(nifti)[5]}.
#'   \item \code{unit}: Unit of \code{step}, decoded from \code{xyzt_units}.
#'   \item \code{start}: Axis start value. Always 0.
#'   \item \code{end}: Axis end value, computed from the number of volumes.
#' }
#'
#' @keywords internal
.make_series_meta <- function(nifti){

  stopifnot(inherits(nifti, "nifti"))

  d <- dim(nifti)
  if(is.null(d) || length(d) < 4){

    stop("`nifti` must be 4D to derive series metadata.", call. = FALSE)

  }

  n_t <- d[4]
  if(is.na(n_t) || n_t < 1){

    stop("Invalid 4th dimension length in `nifti`.", call. = FALSE)

  }

  # oro.nifti follows the NIfTI header layout: pixdim[5] is dt
  step <- oro.nifti::pixdim(nifti)[5]
  if(!is.numeric(step) || length(step) != 1 || is.na(step) || step <= 0){

    stop("Could not infer a valid `step` from `pixdim(nifti)[5]`.", call. = FALSE)

  }

  xyzt_units <- nifti@xyzt_units
  if(is.null(xyzt_units) || length(xyzt_units) != 1 || is.na(xyzt_units)){

    stop("Could not infer time unit from `nifti@hdr$xyzt_units`.", call. = FALSE)

  }

  # NIfTI unit codes (time): 8=sec, 16=msec, 24=usec
  unit <- dplyr::case_when(
    bitwAnd(xyzt_units, 8) == 8 ~ "sec",
    bitwAnd(xyzt_units, 16) == 16 ~ "msec",
    bitwAnd(xyzt_units, 24) == 24 ~ "usec",
    TRUE ~ NA_character_
  )

  if(is.na(unit)){

    stop("`xyzt_units` does not encode a supported time unit (sec/msec/usec).", call. = FALSE)

  }

  start <- 0
  end <- (n_t - 1) * step

  list(
    axis = "time",
    step = step,
    unit = unit,
    start = as.integer(start),
    end = as.integer(end)
  )

}

