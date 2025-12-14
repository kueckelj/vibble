


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

  stopifnot(is_expand(expand))
  stopifnot(is_bb2D(bb2D))

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
.comp_offset_col <- function(x, idx, offset){

  # offset positive -> move to right
  x + idx * offset

}

#' @keywords internal
.comp_offset_row <- function(x, idx, offset){

  # offset positive -> move to top (image inverted y axis)
  x - idx * offset

}


#' @keywords internal
.data_bb0 <- function(vbl2D){

  .vbl_attr(vbl2D, which = "data_bb")

}

#' @rdname vbl_doc_limits_2D
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

#' @rdname vbl_doc_limits_2D
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

#' @rdname vbl_doc_limits_2D
#' @export
screen_bb <- function(vbl2D, slice = NULL){

  stopifnot(is_vbl2D(vbl2D))

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

#' @title Slice utilities for 2D vibbles
#' @description
#' Helpers to query slice values and derive slice-wise spatial or layout
#' indices in a \code{vbl2D} object.
#'
#' @inheritParams vbl_doc
#'
#' @return
#' \itemize{
#'   \item \code{slices()}: Unique slice values.
#'   \item \code{slice_range()}: Minimum and maximum slice values.
#'   \item \code{slice_offset_indices()}: Zero-based offset index for each slice.
#'   \item \code{slice_offset_index()}: Zero-based offset index of a specific slice.
#' }
#'
#' @export

slices <- function(vbl2D){
  sort(unique(vbl2D$slice))
}


#' @keywords internal
.slice_bb0 <- function(vbl2D){

  slice <- slices(vbl2D)[1]
  slice_bb(vbl2D, slice = slice)

}

#' @rdname vbl_doc_limits_2D
#' @export
slice_bb <- function(vbl2D, slice){

  stopifnot(is_vbl2D(vbl2D))
  stopifnot(slice %in% slices(vbl2D))
  purrr::map(vbl2D[vbl2D$slice == slice, c("col", "row")], range)

}


#' @rdname slices
#' @export
slice_range <- function(vbl2D){
  range(slices(vbl2D))
}

#' @rdname slices
#' @export
slice_offset_indices <- function(vbl2D){
  seq_along(slices(vbl2D)) - 1
}

#' @rdname slices
#' @export
slice_offset_index <- function(vbl2D, slice){
  stopifnot(slice %in% slices(vbl2D))
  which(slice == slices(vbl2D)) - 1
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
#' zero-based offset indices via \code{slice_offset_index()}. For slice \eqn{n},
#' the spatial shift is computed as:
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

  sidx <- slice_offset_index(vbl2D, slice = slice)

  purrr::imap(
    .x = bb0,
    .f = function(lim, axis){

      if(axis == "col"){

        lim + offset_axes(vbl2D)[[axis]] * sidx

      } else if(axis == "row"){

        lim - offset_axes(vbl2D)[[axis]] * sidx

      }

    }
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

  vbl2D

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


