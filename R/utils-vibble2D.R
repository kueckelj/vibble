


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
      .x = slices_range(vbl2D),
      .f = ~ screen_bb(vbl2D, .x)
    )

  list(
    col = range(purrr::list_c(purrr::map(lims, "col"))),
    row = range(purrr::list_c(purrr::map(lims, "row")))
  )

}

#' @keywords internal
.ref_bb <- function(vbl2D, type, slice = NULL){

  type <- .match_arg(type, choices = c("data", "plot", "screen", "slice"))

  if(type == "data"){

    data_bb(vbl2D, slice = slice)

  } else if(type == "plot"){

    plot_bb(vbl2D)

  } else if(type == "screen"){

    screen_bb(vbl2D, slice = slice)

  } else if(type == "slice"){

    slice_bb(vbl2D, slice = slice)
  }

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
    expand_bb2D(bb2D = ., expand = vbl_opts("expand.slice.bb"))

  return(sbb)

}


#' @keywords internal
.slice_offset_indices <- function(vbl2D){

  sort(unique(vbl2D$slice_idx))

}

#' @keywords internal
.slice_offset_index <- function(vbl2D, slice){

  stopifnot(slice %in% slices(vbl2D))

  out <-
    dplyr::distinct(vbl2D, slice, slice_idx) %>%
    dplyr::filter(slice == {{ slice }}) %>%
    dplyr::pull(var = "slice_idx")

  return(out)

}



#' @rdname vbl_doc_slice_utils
#' @export
slices_limits <- function(x, ...){

  UseMethod("slices_limits")

}

#' @rdname vbl_doc_slice_utils
#' @export
slices_limits.vbl <- function(x, plane = vbl_def(),...){

  plane <- .resolve_plane(plane = plane)
  slice_axis <- plane_to_ccs(plane)
  ccs_limits(x)[[slice_axis]]

}

#' @rdname vbl_doc_slice_utils
#' @export
slices_limits.vbl2D <- function(x, ...){

  slice_axis <- plane_to_ccs(plane(x))
  ccs_limits(x)[[slice_axis]]

}

#' @rdname vbl_doc_slice_utils
#' @export
slices_max <- function(x, ...){

  UseMethod("slices_max")

}

#' @rdname vbl_doc_slice_utils
#' @export
slices_max.vbl <- function(x, plane = vbl_def(),...){

  plane <- .resolve_plane(plane = plane)
  max(slices_range(x, plane))

}

#' @rdname vbl_doc_slice_utils
#' @export
slices_max.vbl2D <- function(x, ....){

  max(slices_range(x))

}

#' @rdname vbl_doc_slice_utils
#' @export
slices_mid1 <- function(x, ...){

  UseMethod("slices_mid1")

}

#' @rdname vbl_doc_slice_utils
#' @export
slices_mid1.vbl <- function(x, plane = vbl_def(),...){

  plane <- .resolve_plane(plane)
  as.integer(median(slices(x, plane)))

}

#' @rdname vbl_doc_slice_utils
#' @export
slices_mid1.vbl2D <- function(x, ...){

  as.integer(median(slices(x)))

}

#' @rdname vbl_doc_slice_utils
#' @export
slices_min <- function(x, ...){

  UseMethod("slices_min")

}

#' @rdname vbl_doc_slice_utils
#' @export
slices_min.vbl <- function(x, plane = vbl_opts("plane"),...){

  min(slices_range(x, plane))

}

#' @rdname vbl_doc_slice_utils
#' @export
slices_min.vbl2D <- function(x, ...){

  min(slices_range(x))

}

#' @rdname vbl_doc_slice_utils
#' @export
slices_range <- function(x, ...){

  UseMethod("slices_range")

}

#' @rdname vbl_doc_slice_utils
#' @export
slices_range.vbl <- function(x, plane = vbl_opts("plane"),...){

  range(slices(x, plane = plane))

}

#' @rdname vbl_doc_slice_utils
#' @export
slices_range.vbl2D <- function(x, ...){

  range(slices(x))

}


#' @rdname vbl_doc_slice_queries
#' @export
slices <- function(x, ...){

  UseMethod("slices")

}

#' @rdname vbl_doc_slice_queries
#' @export
slices.vbl <- function(x, plane = vbl_def(), ...){

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
slices_cond.vbl <- function(x, .cond, n = vbl_def(), plane = vbl_def()){

  n <- .resolve_n(n = n, opt = "slices")

  plane <- .resolve_plane(plane = plane)
  slice_axis <- plane_to_ccs(plane)

  .cond_quo <- rlang::enquo(.cond)

  s <-
    dplyr::filter(.data = x, !!.cond_quo, .by = {{ slice_axis }}) %>%
    dplyr::pull(var = {{ slice_axis }}) %>%
    sort() %>%
    unique()

  .downsample(s, n = n)

}

#' @rdname vbl_doc_slice_queries
#' @export
slices_cond.vbl2D <- function(x, .cond, n = vbl_def()){

  n <- .resolve_n(n = n, opt = "slices")

  .cond_quo <- rlang::enquo(.cond)

  s <-
    dplyr::filter(.data = x, !!.cond_quo, .by = "slice") %>%
    dplyr::pull(var = "slice") %>%
    sort() %>%
    unique()

  .downsample(s, n = n)

}

#' @rdname vbl_doc_slice_queries
#' @export
slices_mid <- function(x, ...){

  UseMethod("slices_mid")

}

#' @rdname vbl_doc_slice_queries
#' @export
slices_mid.vbl <- function(x, n = vbl_def(), radius = 0.2, plane = vbl_def()){

  n <- .resolve_n(n = n, opt = "slices")
  plane <- .resolve_plane(plane = plane)

  stopifnot(radius > 0)

  if(radius < 1){

    radius <- as.integer(diff(slices_range(x, plane)) * radius)

  }

  mid <- slices_mid1(x, plane)

  mn <- mid - radius
  mx <- mid + radius

  s <- mn:mx

  .downsample(s, n = pmin(length(s), n))

}

#' @rdname vbl_doc_slice_queries
#' @export
slices_mid.vbl2D <- function(x, n = vbl_def(), radius = 0.2){

  n <- .resolve_n(n = n, opt = "slices")

  stopifnot(radius > 0)

  if(radius < 1){

    radius <- as.integer(diff(slices_range(x)) * radius)

  }

  mid <- slices_mid1(x)

  mn <- mid - radius
  mx <- mid + radius

  s <- mn:mx

  .downsample(s, n = pmin(length(s), n))

}

#' @rdname vbl_doc_slice_queries
#' @export
slices_max_area <- function(x, ...){

  UseMethod("slices_max_area")

}

#' @rdname vbl_doc_slice_queries
#' @export
slices_max_area.vbl <- function(x, .cond, n = vbl_def(), plane = vbl_def()){

  plane <- .resolve_plane(plane)
  n <- .resolve_n(n, opt = "slices")

  .cond_quo <- rlang::enquo(.cond)
  qassign(.cond_quo)

  slice_axis <- unname(req_axes_2D(plane)["slice"])

  out <-
    dplyr::filter(x, !!.cond_quo, .by = {{ slice_axis }}) %>%
    dplyr::group_by(!!rlang::sym(slice_axis)) %>%
    dplyr::summarise(count = dplyr::n()) %>%
    dplyr::arrange(dplyr::desc(count)) %>%
    dplyr::pull(var = {{ slice_axis }}) %>%
    head(x = ., n = n)

  return(out)

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


