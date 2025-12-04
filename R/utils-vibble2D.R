


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

#' @title Limits utilities for 2D vibbles
#' @name vbl2D_limits
#' @description
#' Helpers to get and set 2D screen limits of a \code{vbl2D} object and to derive
#' overall plot limits across all slices.
#'
#' @inheritParams vbl_doc
#' @param slice Integer slice value. If \code{NULL}, the first slice in
#'   \code{slices(vbl2D)} is used.
#' @param value A \link[=is_bb2D]{bb2D} object giving \code{col} and \code{row}
#'   limits for the reference slice (slice 0).
#'
#' @return
#' \itemize{
#'   \item \code{screen_limits()}: A \link[=is_bb2D]{bb2D} object (list with
#'   elements \code{col} and \code{row}) giving the screen-space limits for the
#'   requested slice, after applying slice offsets.
#'
#'   \item \code{screen_limits<-}: The modified \code{vbl2D} with updated
#'   \code{screen_limits} attribute.
#'
#'   \item \code{plot_limits()}: A named list with elements \code{col} and
#'   \code{row} giving the global screen-space limits required to display all
#'   slices in the current offset layout.
#' }
#'
#' @export
screen_limits <- function(vbl2D, slice = NULL){

  stopifnot(is_vbl2D(vbl2D))

  screen_lim <- .vbl_attr(vbl2D, which = "screen_limits")

  if(is.numeric(slice)){

    screen_lim <- .offset_lim0(vbl2D, slice = slice, lim0 = screen_lim)

  }

  return(screen_lim)

}

#' @keywords internal
.screen_limits0 <- function(vbl2D){

  slice <- slices(vbl2D)[1]
  screen_limits(vbl2D, slice = slice)

}

#' @rdname vbl2D_limits
#' @export
`screen_limits<-` <- function(vbl2D, value){

  stopifnot(is_vbl2D(vbl2D))
  stopifnot(is_bb2D(value))

  attr(vbl2D, which = "screen_limits") <- value

  return(vbl2D)

}

#' @rdname vbl2D_limits
#' @export
plot_limits <- function(vbl2D){

  stopifnot(is_vbl2D(vbl2D))

  lims <-
    purrr::map(
      .x = slice_range(vbl2D),
      .f = ~ screen_limits(vbl2D, .x)
    )

  list(
    col = range(purrr::list_c(purrr::map(lims, "col"))),
    row = range(purrr::list_c(purrr::map(lims, "row")))
  )

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
#'   \item \code{slice_limits()}: 2D bounding box (col/row ranges) of one slice.
#'   \item \code{slice_range()}: Minimum and maximum slice values.
#'   \item \code{slice_offset_indices()}: Zero-based offset index for each slice.
#'   \item \code{slice_offset_index()}: Zero-based offset index of a specific slice.
#' }
#'
#' @export
slices <- function(vbl2D){
  sort(unique(vbl2D$slice))
}

#' @rdname slices
#' @export
slice_limits <- function(vbl2D, slice){
  stopifnot(is_vbl2D(vbl2D))
  stopifnot(slice %in% slices(vbl2D))
  purrr::map(vbl2D[vbl2D$slice == slice, c("col", "row")], range)
}

#' @keywords internal
.slice_limits0 <- function(vbl2D){

  slice <- slices(vbl2D)[1]
  slice_limits(vbl2D, slice = slice)

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





#' @rdname vbl_doc_offset_utilities
#' @export
offset_axes <- function(vbl2D){

  stopifnot(is_vbl2D(vbl2D))

  list(
    col = offset_col(vbl2D),
    row = offset_row(vbl2D)
  )

}

#' @keywords internal
.offset_lim0 <- function(vbl2D, slice, lim0){

  sidx <- slice_offset_index(vbl2D, slice = slice)

  purrr::imap(
    .x = lim0,
    .f = function(lim, axis){

      if(axis == "col"){

        lim + offset_axes(vbl2D)[[axis]] * sidx

      } else if(axis == "row"){

        lim - offset_axes(vbl2D)[[axis]] * sidx

      }

    }
  )

}

#' @rdname vbl_doc_offset_utilities
#' @export
offset_col <- function(vbl2D){

  .vbl_attr(vbl2D, which = "offset_col")

}

#' @rdname vbl_doc_offset_utilities
#' @export
`offset_col<-` <- function(vbl2D, value){

  stopifnot(is.integer(value))

  attr(vbl2D, which = "offset_col") <- value

  return(vbl2D)

}

#' @rdname vbl_doc_offset_utilities
#' @export
offset_row <- function(vbl2D){

  .vbl_attr(vbl2D, which = "offset_row")

}

#' @rdname vbl_doc_offset_utilities
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


