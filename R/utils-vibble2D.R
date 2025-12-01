


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

  slim <- .vbl_attr(vbl2D, which = "screen_limits")

  if(is_offset(vbl2D)){

    if(is.null(slice)){ slice <- slices(vbl2D)[1] }

    sidx <- slice_offset_index(vbl2D, slice = slice)

    slim <-
      purrr::imap(
        .x = slim,
        .f = function(lim, axis){

          if(axis == "col"){

            lim + offset_both(vbl2D)[[axis]] * sidx

          } else if(axis == "row"){

            lim - offset_both(vbl2D)[[axis]] * sidx

          }

        }
      )

  }

  return(slim)

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
  unique(vbl2D$slice)
}

#' @rdname slices
#' @export
slice_limits <- function(vbl2D, slice){
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




#' @title vbl2D attributes
#' @description
#' Access and modify core attributes of a `vbl2D` object.
#' These helpers provide a stable interface to metadata used for 2D visualization and offset handling.
#'
#' @param which Character scalar for `offset_attr()` selecting the offset attribute.
#'   Valid values are `c("dir", "dist")`.
#' @param value New value assigned in the corresponding replacement functions.
#'
#' @return
#' For accessor functions, the requested attribute value.
#' For replacement functions, the modified `vbl2D` object.
#'
#' @details
#' The following attributes are attached to `vbl2D` objects and can be accessed or modified via these helpers.
#' They are used by plotting and slicing functions to control display, offsets, and orientation in 2D space.
#'
#' The `lim` attribute is a list with optional entries `col` and `row`.
#' Each entry is either `NULL` or a valid limit vector tested via `is_limit()`.
#' These limits can be used by higher level functions to restrict the visible extent in column and row directions.
#'
#' Offset attributes are created when `vibble2D()` is called with `offset_dist > 0`.
#' The direction is stored in `offset_dir` and the distance in `offset_dist`.
#' `is_offset()` tests whether a `vbl2D` object currently has a positive offset distance.
#'
#' The `plane` attribute stores the anatomical plane used to generate the `vbl2D` object.
#' Valid options are `c("axi", "cor", "sag")` depending on the implementation of `vibble2D()`.
#'
#' @note
#' The `lim` attribute affects 2D display or computational subsetting only and does not change the underlying 3D voxel space of the vibble.
#' Offsets and plane metadata should be kept consistent with how `vibble2D()` generated the object to avoid misaligned visualizations.
#'
#' @section Functions:
#' \itemize{
#'   \item `lim()`, `lim<-()`: Get and set the `lim` attribute, a list with optional `col` and `row` limits.
#'   \item `offset_attr()`: Low level accessor for offset attributes, selecting `offset_dir` or `offset_dist` via `which`.
#'   \item `offset_dir()`, `offset_dir<-()`: Get and set the offset direction.
#'   \item `offset_dist()`, `offset_dist<-()`: Get and set the offset distance.
#'   \item `plane()`, `plane<-()`: Get and set the anatomical plane of a `vbl2D` object.
#'   \item `is_offset()`: Logical test, evaluates to `TRUE` if `offset_dist()` is greater than zero.
#' }
#'
#' @seealso
#' \link{vibble2D}() for constructing `vbl2D` objects.
#' \link{is_limit}() for validating limit vectors used in `lim`.
#'
#' @name vbl2D_attr
NULL

#' @rdname vbl2D_attr
#' @export
offset_both <- function(vbl2D){

  stopifnot(is_vbl2D(vbl2D))

  list(
    col = offset_col(vbl2D),
    row = offset_row(vbl2D)
  )

}

#' @rdname vbl2D_attr
#' @export
offset_col <- function(vbl2D){

  .vbl_attr(vbl2D, which = "offset_col")

}

#' @rdname vbl2D_attr
#' @export
`offset_col<-` <- function(vbl2D, value){

  stopifnot(is.integer(value))

  attr(vbl2D, which = "offset_col") <- value

  return(vbl2D)

}

#' @rdname vbl2D_attr
#' @export
offset_row <- function(vbl2D){

  .vbl_attr(vbl2D, which = "offset_row")

}

#' @rdname vbl2D_attr
#' @export
`offset_row<-` <- function(vbl2D, value){

  stopifnot(is.integer(value))

  attr(vbl2D, which = "offset_row") <- value

  return(vbl2D)

}


#' @rdname vbl2D_attr
#' @export
plane <- function(vbl2D){

  .vbl_attr(vbl2D, which = "plane")

}

#' @rdname vbl2D_attr
#' @export
`plane<-` <- function(vbl2D, value){

  attr(vbl2D, which = "plane") <- value

  vbl2D

}

#' @keywords internal
.offset_add <- function(limit, value){

  limit[1] <- limit[1] + value
  limit[2] <- limit[2] + value

  sort(limit)

}

#' @keywords internal
.offset_axis <- function(vbl2D){

  if(!is_offset(vbl2D)){

    warning("vbl2D is not offset.")
    out <- NULL

  } else {

    out <- ifelse(grepl("left|right", offset_dir(vbl2D)), "col", "row")

  }

  return(out)

}

#' @keywords internal
.offset_comp_fn <- function(offset_dir){

  ifelse(grepl("right|bottom", offset_dir), .offset_add, .offset_subtract)

}

#' @keywords internal
.offset_subtract <- function(limit, value){

  limit[1] <- limit[1] - value
  limit[2] <- limit[2] - value

  sort(limit)

}

