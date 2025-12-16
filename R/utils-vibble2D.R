


#' @title Expand limits of a 2D bounding box
#' @description
#' Expand a 2D bounding box (\code{bb2D}) according to an absolute, relative,
#' or logical \link[=vbl_doc_expand]{expand rule}. This operation adjusts the
#' lower and upper limits of both \code{col} and \code{row} while preserving the
#' structure of the bounding box.
#'
#' @param expand An \link[=is_expand]{expand} specification that determines how
#' the bounding box is expanded (absolute, relative, or logical).
#' @inheritParams vbl_doc
#'
#' @return
#' A modified \code{bb2D} object with updated limits for \code{col} and \code{row}.
#'
#' @examples
#' vbl2D <- vibble2D(example_vbl(), plane = "axi", slices = 60)
#' bb <- slice_bbox(vbl2D, slice = 60)
#'
#' # Absolute expansion: add 2 units on each side
#' expand_bb2D(bb, 2L)
#'
#' # Relative expansion: expand by 10% of the current range
#' expand_bb2D(bb, 0.1)
#'
#' @export
expand_bb2D <- function(bb2D, expand){

  .stop_if_not(is_expand(expand))
  .stop_if_not(is_bb2D(bb2D))

  if(is_expand_abs(expand)){

    bb2D <- purrr::map2(.x = bb2D, .y = expand, .f = .apply_expand_abs)

  } else if(is_expand_rel(expand)){

    bb2D <- purrr::map2(.x = bb2D, .y = expand, .f = .apply_expand_rel)

  } else if(is_expand_lgl(expand)){

    bb2D <- purrr::map2(.x = bb2D, .y = expand, .f = .apply_expand_lgl)

  }

  return(bb2D)
}


#' @keywords internal
.comp_offset <- function(x, idx, offset){

  # offset positive
  # -> move to right, if col
  # -> move to bottom, if row
  x + idx * offset

}


#' @keywords internal
.data_bb0 <- function(vbl2D){

  .vbl_attr(vbl2D, which = "data_bb") %>%
    expand_bb2D(bb2D = ., expand = vbl_opts("expand.data.bb"))
}

#' @rdname vbl_doc_ref_bb
#' @export
data_bb <- function(vbl2D, slice = NULL){

  stopifnot(is_vbl2D(vbl2D))

  dbb <- .data_bb0(vbl2D)

  if(is.numeric(slice)){

    dbb <- .offset_bb0(vbl2D, slice = slice, bb0 = dbb)

  }

  return(dbb)

}

#' @keywords internal
`data_bb<-` <- function(vbl2D, value){

  stopifnot(is_vbl2D(vbl2D))
  stopifnot(is_bb2D(value))

  attr(vbl2D, which = "data_bb") <- value

  return(vbl2D)

}

#' @rdname vbl_doc_ref_bb
#' @export
plot_bb <- function(vbl2D){

  stopifnot(is_vbl2D(vbl2D))

  lims <-
    purrr::map(
      .x = slice_range(vbl2D),
      .f = ~ screen_bb(vbl2D, .x)
    )

  list(
    col = range(purrr::list_c(purrr::map(lims, "col"))),
    row = range(purrr::list_c(purrr::map(lims, "row")))
  )

}

#' @keywords internal
.screen_bb0 <- function(vbl2D){

  .vbl_attr(vbl2D, which = "screen_bb")

}

#' @rdname vbl_doc_ref_bb
#' @export
screen_bb <- function(vbl2D, slice = NULL){

  .stop_if_not(is_vbl2D(vbl2D))

  sbb <- .screen_bb0(vbl2D)

  if(is.numeric(slice)){

    sbb <- .offset_bb0(vbl2D, slice = slice, bb0 = sbb)

  }

  return(sbb)

}

#' @keywords internal
`screen_bb<-` <- function(vbl2D, value){

  stopifnot(is_vbl2D(vbl2D))
  stopifnot(is_bb2D(value))

  attr(vbl2D, which = "screen_bb") <- value

  return(vbl2D)

}

#' @keywords internal
.slice_bb0 <- function(vbl2D){

  sbb0 <-
    slice_bb(vbl2D, slice = slices(vbl2D)[1]) %>%
    expand_bb2D(bb2D = ., expand = vbl_opts("expand.slice.bb"))

  return(sbb0)

}

#' @rdname vbl_doc_ref_bb
#' @export
slice_bb <- function(vbl2D, slice){

  .stop_if_not(is_vbl2D(vbl2D))
  stopifnot(slice %in% slices(vbl2D))

  sbb <-
    purrr::map(vbl2D[vbl2D$slice == slice, c("col", "row")], range) %>%
    expand_bb2D(bb2D = ., expand = vbl_opts("expand.refbb"))

  return(sbb)

}

#' @title Slice range and slice position utilities
#' @name vbl_doc_slice_utils
#' @description
#' Utilities to query slice limits, observed slice ranges, and representative
#' slice positions for voxel data.
#'
#' @details
#' These functions operate on the slice axis of a vibble (`vbl`) or a plane-specific
#' vibble2D (`vbl2D`) object.
#'
#' Two related but distinct concepts are exposed.
#'
#' \itemize{
#'   \item{Slice limits: } The full valid slice limits of the registered image
#'   space, independent of filtering.
#'   \item{Observed slice range: } The slice range currently represented by rows
#'   in the object, which changes after filtering.
#' }
#'
#' @section Functions:
#' \itemize{
#'   \item{slice_limits(): } Returns the full slice limits of the image space using
#'   `ccs_limits()`.
#'   \item{slice_range(): } Returns the observed slice range as
#'   `range(slices())`.
#'   \item{slice_min(): } Returns the minimum observed slice.
#'   \item{slice_max(): } Returns the maximum observed slice.
#'   \item{slice_mid(): } Returns the median observed slice, coerced to integer.
#' }
#'
#' For `vbl`, the slice axis is determined from `plane` via `plane_to_ccs()`.
#' For `vbl2D`, the plane is taken from `plane(x)` and slices are read from
#' `x$slice`.
#'
#' @param x A vibble (`vbl`) or vibble2D (`vbl2D`) object.
#' @param plane Character scalar. Anatomical plane used to determine the slice
#' axis for `vbl`. Valid options are *c('axi', 'cor', 'sag')*.
#' @param ... Additional arguments, currently unused.
#'
#' @return
#' \itemize{
#'   \item{slice_limits(): } A limit object describing the valid slice coordinates
#'   in the image space.
#'   \item{slice_range(): } Integer vector of length two giving the observed slice
#'   range.
#'   \item{slice_min(), slice_max(), slice_mid(): } Integer scalars derived from
#'   the observed slice range.
#' }
NULL


#' @rdname vbl_doc_slice_utils
#' @export
slice_limits <- function(x, ...){

  UseMethod("slice_limits")

}

#' @rdname vbl_doc_slice_utils
#' @export
slice_limits.vbl <- function(x, plane = vbl_opts("plane"),...){

  slice_axis <- plane_to_ccs(plane)
  ccs_limits(x)[[slice_axis]]

}

#' @rdname vbl_doc_slice_utils
#' @export
slice_limits.vbl2D <- function(x, ...){

  slice_axis <- plane_to_ccs(plane(x))
  ccs_limits(x)[[slice_axis]]

}

#' @rdname vbl_doc_slice_utils
#' @export
slice_max <- function(x, ...){

  UseMethod("slice_max")

}

#' @rdname vbl_doc_slice_utils
#' @export
slice_max.vbl <- function(x, plane = vbl_opts("plane"),...){

  max(slice_range(x, plane))

}

#' @rdname vbl_doc_slice_utils
#' @export
slice_max.vbl2D <- function(x, ....){

  max(slice_range(x))

}

#' @rdname vbl_doc_slice_utils
#' @export
slice_mid <- function(x, ...){

  UseMethod("slice_mid")

}

#' @rdname vbl_doc_slice_utils
#' @export
slice_mid.vbl <- function(x, plane = vbl_opts("plane"),...){

  as.integer(median(slices(x, plane)))

}

#' @rdname vbl_doc_slice_utils
#' @export
slice_mid.vbl2D <- function(x, ...){

  as.integer(median(slices(x)))

}

#' @rdname vbl_doc_slice_utils
#' @export
slice_min <- function(x, ...){

  UseMethod("slice_min")

}

#' @rdname vbl_doc_slice_utils
#' @export
slice_min.vbl <- function(x, plane = vbl_opts("plane"),...){

  min(slice_range(x, plane))

}

#' @rdname vbl_doc_slice_utils
#' @export
slice_min.vbl2D <- function(x, ...){

  min(slice_range(x))

}

#' @rdname vbl_doc_slice_utils
#' @export
slice_range <- function(x, ...){

  UseMethod("slice_range")

}

#' @rdname vbl_doc_slice_utils
#' @export
slice_range.vbl <- function(x, plane = vbl_opts("plane"),...){

  range(slices(x, plane = plane))

}

#' @rdname vbl_doc_slice_utils
#' @export
slice_range.vbl2D <- function(x, ...){

  range(slices(x))

}


#' @keywords internal
.slice_offset_indices <- function(vbl2D){
  seq_along(slices(vbl2D)) - 1
}

#' @keywords internal
.slice_offset_index <- function(vbl2D, slice){
  stopifnot(slice %in% slices(vbl2D))
  which(slice == slices(vbl2D)) - 1
}



#' @title Slice queries and slice selection sequences
#' @name vbl_doc_slice_queries
#' @description
#' Utilities to query represented slices and to construct representative slice
#' sequences for quick plotting.
#'
#' @details
#' The `slice_*` family returns single-value or range summaries of slice position
#' such as limits, range, min, max, or mid.
#'
#' The `slices_*` family returns integer vectors of slice indices intended for
#' plotting and iteration. These functions produce slice sequences or selections
#' that can be passed directly to `ggplane(..., slices = )`.
#'
#' For `vbl`, the slice axis is determined from `plane` via `plane_to_ccs()`.
#' For `vbl2D`, slices are taken from `x$slice`.
#'
#' @section Functions:
#' \itemize{
#'   \item{slices(): } Returns the unique slice indices represented by the object.
#'   \item{slices_seq(): } Returns a contiguous integer sequence `from:to` within
#'   the observed slice range. If `from` or `to` are `NULL`, bounds default to
#'   `slice_min()` and `slice_max()`.
#'   \item{slices_cond(): } Returns a representative subset of slices where a
#'   per-voxel condition `.cond` holds. The resulting slice indices are downsampled
#'   to at most `n` values via `.downsample()`.
#'   \item{slices_mid(): } Returns a representative subset of slices centered on
#'   the median slice. `radius` can be an absolute integer radius or a proportion
#'   of the observed slice range if `radius < 1`. The resulting slice indices are
#'   downsampled to at most `n` values via `.downsample()`.
#' }
#'
#' @param x A vibble (`vbl`) or vibble2D (`vbl2D`) object.
#' @param plane Character scalar. Anatomical plane used to determine the slice
#' axis for `vbl`. Valid options are *c('axi', 'cor', 'sag')*.
#' @param from,to Optional numeric scalars. Lower and upper slice bounds for
#' `slices_seq()`. If `NULL`, bounds default to `slice_min()` and `slice_max()`.
#' @param .cond A logical expression evaluated per voxel row using data-masking.
#' @param radius Numeric scalar. If `radius < 1`, it is interpreted as a fraction
#' of the observed slice range. Otherwise it is interpreted as an absolute integer
#' radius around the median slice.
#' @param n Integer. Maximum number of slices returned by `slices_cond()` and
#' `slices_mid()`.
#' @param ... Additional arguments, currently unused.
#'
#' @return
#' \itemize{
#'   \item{slices(): } Integer vector of unique slice indices.
#'   \item{slices_seq(): } Integer vector of contiguous slices.
#'   \item{slices_cond(): } Integer vector of selected slices, downsampled to at
#'   most `n`.
#'   \item{slices_mid(): } Integer vector of selected slices, downsampled to at
#'   most `n`.
#' }
#'
#' @examples
#' vbl <- example_vbl()
#'
#' # query represented slices
#' slices(vbl, plane = "axi")
#'
#' # create a contiguous sequence within the observed range
#' slices_seq(vbl, plane = "axi", from = 90, to = 110)
#'
#' # select slices by condition (then downsample)
#' slices_cond(vbl, "axi", tumor, n = 6)
#'
#' # select slices around the mid slice
#' slices_mid(vbl, "axi", radius = 0.2, n = 6)
#'
NULL


#' @rdname vbl_doc_slice_queries
#' @export
slices_seq <- function(x, ...){

  UseMethod("slice_seq")

}

#' @rdname vbl_doc_slice_queries
#' @export
slices_seq.vbl <- function(x, plane = vbl_opts("plane"), from = NULL, to = NULL){

  if(is.numeric(from)){

    stopifnot(from >= slice_min(x, plane))

  } else {

    from <- slice_min(x, plane)

  }

  if(is.numeric(to)){

    stopifnot(to <= slice_max(x, plane))

  } else {

    to <- slice_max(x, plane)

  }

  from:to

}

#' @rdname vbl_doc_slice_queries
#' @export
slices_seq.vbl2D <- function(x, from = NULL, to = NULL){

  if(is.numeric(from)){

    stopifnot(from >= slice_min(x))

  } else {

    from <- slice_min(x)

  }

  if(is.numeric(to)){

    stopifnot(to <= slice_max(x))

  } else {

    to <- slice_max(x)

  }

  from:to

}

#' @rdname vbl_doc_slice_queries
#' @export
slices <- function(x, ...){

  UseMethod("slices")

}

#' @rdname vbl_doc_slice_queries
#' @export
slices.vbl <- function(x, plane = vbl_opts("plane"), ...){

  unique(x[[plane_to_ccs(plane)]])

}

#' @rdname vbl_doc_slice_queries
#' @export
slices.vbl2D <- function(x, ...){

  unique(x$slice)

}

#' @rdname vbl_doc_slice_queries
#' @export
slices.ggvibble <- function(x, ...){

  slices(x$vbl2D)

}


#' @rdname vbl_doc_slice_queries
#' @export
slices_cond <- function(x, ...){

  UseMethod("slices_cond")

}

#' @rdname vbl_doc_slice_queries
#' @export
slices_cond.vbl <- function(x, plane = vbl_opts("plane"), .cond, n = 6){

  slice_axis <- plane_to_ccs(plane)

  .cond_quo <- rlang::enquo(.cond)

  s <-
    dplyr::filter(.data = x, !!.cond_quo, .by = {{ slice_axis }}) %>%
    dplyr::pull(var = {{ slice_axis }}) %>%
    sort()

  .downsample(s, n = pmin(length(s), n))

}

#' @rdname vbl_doc_slice_queries
#' @export
slices_cond.vbl2D <- function(x, .cond, n = 6){

  .cond_quo <- rlang::enquo(.cond)

  s <-
    dplyr::filter(.data = x, !!.cond_quo, .by = "slice") %>%
    dplyr::pull(var = "slice") %>%
    sort()

  .downsample(s, n = pmin(length(s), n))

}

#' @rdname vbl_doc_slice_queries
#' @export
slices_mid <- function(x, ...){

  UseMethod("slices_mid")

}


#' @rdname vbl_doc_slice_queries
#' @export
slices_mid.vbl <- function(x, plane = vbl_opts("plane"), radius = 0.2, n = 6){

  stopifnot(radius > 0)

  if(radius < 1){

    radius <- as.integer(diff(slice_range(x, plane)) * radius)

  }

  mid <- slice_mid(x, plane)

  mn <- mid - radius
  mx <- mid + radius

  s <- mn:mx

  .downsample(s, n = pmin(length(s), n))

}

#' @rdname vbl_doc_slice_queries
#' @export
slices_mid.vbl2D <- function(x, radius = 0.2, n = 6){

  stopifnot(radius > 0)

  if(radius < 1){

    radius <- as.integer(diff(slice_range(x)) * radius)

  }

  mid <- slice_mid(x)

  mn <- mid - radius
  mx <- mid + radius

  s <- mn:mx

  .downsample(s, n = pmin(length(s), n))

}





#' @title Offset utilities for 2D vibbles
#' @name vbl_doc_offset_utils
#' @description
#' Utilities to query and manipulate slice-wise spatial offsets in `vbl2D`
#' objects. Offsets shift slices relative to each other in 2D layouts, allowing
#' for staggered slice arrangements. Offsets operate separately along the `col`
#' and `row` axes and are stored as integer attributes of the `vbl2D` object.
#'
#' The functions provided here support:
#' \itemize{
#'   \item retrieving per-axis offsets (\code{offset_col()}, \code{offset_row()});
#'   \item setting offsets as integer values;
#'   \item computing the effective offset applied to a specific slice;
#'   \item deriving combined axis offsets used for layout transformations.
#' }
#'
#' Offsets are interpreted in slice index order, where slices are assigned
#' zero-based offset indices. For slice \eqn{n}, the spatial shift is computed as:
#'
#' \preformatted{
#'   col_new = col + offset_col * n
#'   row_new = row - offset_row * n
#' }
#'
#' Signs are chosen to preserve visual consistency across sagittal, axial,
#' and coronal layouts.
#'
#' @section Functions:
#'
#' \describe{
#'
#'   \item{\code{is_offset(vbl2D)}}{
#'   Logical test whether col and/or row is offset.
#'   }
#'
#'   \item{\code{offset_axes(vbl2D)}}{
#'   Returns a named list containing the `col` and `row` offsets of a `vbl2D`.
#'   }
#'
#'   \item{\code{offset_col(vbl2D)}, \code{offset_row(vbl2D)}}{
#'   Accessors for the respective offset attributes.
#'   }
#'
#'   \item{\code{offset_col<-()}, \code{offset_row<-()}}{
#'   Setters for updating the per-axis offsets. Values must be integer.
#'   }
#'
#' }
#'
#' @return
#' Offset accessors return integer values. \code{offset_axes()} returns a
#' named list. Internal helpers return shifted limit structures suitable for
#' downstream layout operations.
#'
#' @keywords internal
NULL

#' @rdname vbl_doc_offset_utils
#' @export
offset_axes <- function(vbl2D){

  stopifnot(is_vbl2D(vbl2D))

  list(
    col = offset_col(vbl2D),
    row = offset_row(vbl2D)
  )

}

#' @keywords internal
.offset_bb0 <- function(vbl2D, slice, bb0){

  sidx <- .slice_offset_index(vbl2D, slice = slice)

  purrr::imap(
    .x = bb0,
    .f = ~ .x + offset_axes(vbl2D)[[.y]] * sidx
  )

}

#' @rdname vbl_doc_offset_utils
#' @export
offset_col <- function(vbl2D){

  .vbl_attr(vbl2D, which = "offset_col")

}

#' @rdname vbl_doc_offset_utils
#' @export
`offset_col<-` <- function(vbl2D, value){

  stopifnot(is.integer(value))

  attr(vbl2D, which = "offset_col") <- value

  return(vbl2D)

}

#' @rdname vbl_doc_offset_utils
#' @export
offset_row <- function(vbl2D){

  .vbl_attr(vbl2D, which = "offset_row")

}

#' @rdname vbl_doc_offset_utils
#' @export
`offset_row<-` <- function(vbl2D, value){

  stopifnot(is.integer(value))

  attr(vbl2D, which = "offset_row") <- value

  return(vbl2D)

}


#' @export
plane <- function(vbl2D){

  .vbl_attr(vbl2D, which = "plane")

}

#' @export
`plane<-` <- function(vbl2D, value){

  attr(vbl2D, which = "plane") <- value

  return(vbl2D)

}



#' @title Plane-wise spatial resolutions
#' @description
#' Compute the voxel resolution of a \code{vbl} object for a given anatomical
#' plane, or for all supported planes. The resolution is defined as the product
#' of the maximum index extents along the two in-plane axes.
#'
#' @param vbl A \code{vbl} object.
#' @param plane Character scalar specifying the anatomical plane for which the
#'   resolution is computed. Must match one of \code{vbl_planes}.
#'
#' @details
#' For a given \code{plane}, the corresponding 2D axes are obtained via
#' \code{req_axes_2D()}. The function retrieves the Cartesian-coordinate-system
#' limits (\code{ccs_limits()}) of these axes and returns:
#'
#' \deqn{\max(\mathrm{col}) \times \max(\mathrm{row})}
#'
#' This value approximates the number of potential in-plane voxel positions
#' before filtering or masking.
#'
#' \code{plane_resolutions()} applies \code{plane_resolution()} to all
#' anatomical planes and returns a named numeric vector sorted in decreasing
#' order.
#'
#' @return
#' \itemize{
#'   \item \code{plane_resolution()}: A numeric scalar.
#'   \item \code{plane_resolutions()}: A named numeric vector of plane
#'   resolutions.
#' }
#'
#' @seealso \code{\link{ccs_limits}}, \code{\link{req_axes_2D}}
#'
#' @export
plane_resolution <- function(vbl, plane){

  req_axes <- req_axes_2D(plane)

  ccs_lim <- ccs_limits(vbl)

  max(ccs_lim[[req_axes["col"]]]) * max(ccs_lim[[req_axes["row"]]])

}

#' @rdname plane_resolution
#' @export
plane_resolutions <- function(vbl){

  res <-
    purrr::map_dbl(
      .x = unname(vbl_planes),
      .f = ~ plane_resolution(vbl, plane = .x)
    )

  names(res) <- unname(vbl_planes)

  rev(sort(res))

}


