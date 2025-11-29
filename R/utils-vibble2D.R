


#' @title Expand limits of a 2D vibble
#' @description Expand the 2D spatial limits (`lim`) of a `vbl2D` object according to an absolute or relative
#' expand specification. Supports logical, absolute, and relative \link[=vbl_doc_expand]{expand rules}.
#'
#' @param expand An \link[=is_expand]{expand} value used to expand the limits of the output 2D vibble.
#' @inheritParams vbl_doc
#'
#' @return
#' A modified `vbl2D` with updated `lim` values for `col` and `row`.
#'
#' @examples
#' vbl2D <- vibble2D(example_vbl(), plane = "axi", slices = 60)
#'
#' # Absolute expansion: add 2 units to each side
#' expand_lim2D(vbl2D, 2L)
#'
#' # Relative expansion: add 10% of the range
#' expand_lim2D(vbl2D, 0.1)
#'
#' @export
expand_lim2D <- function(vbl2D, expand){

  stopifnot(is_expand(expand))

  lim2D <- lim(vbl2D)

  if(is_expand_abs(expand)){

    lim2D <- purrr::map2(.x = lim2D, .y = expand, .f = .apply_expand_abs)

  } else if(is_expand_rel(expand)){

    lim2D <- purrr::map2(.x = lim2D, .y = expand, .f = .apply_expand_rel)

  } else if(is_expand_lgl(expand)){

    lim2D <- purrr::map2(.x = lim2D, .y = expand, .f = .apply_expand_lgl)

  }

  lim(vbl2D) <- lim2D

  return(vbl2D)

}


#' @title Offset computation helpers
#' @description
#' Internal helpers that apply slice-wise positional offsets to 2D coordinates.
#' Offsets are computed using the slice index `idx`, which encodes the relative order of slices
#' (see \link{slice_offset_index}()).
#'
#' @param col,row Numeric vectors of `col` or `row` positions.
#' @param idx Integer slice-offset index used to scale the shift for each slice.
#' @param dir Direction string indicating which axis is offset.
#' @param dist Absolute offset distance applied per index step.
#'
#' @return Updated coordinate vectors.
#' @keywords internal
#' @name vbl_doc_comp_offset
NULL

#' @rdname vbl_doc_comp_offset
.comp_offset_col <- function(col, idx, dir, dist){

  if(grepl("left|right", dir)){

    fct <- ifelse(grepl("left", dir), -1, 1)
    dist <- dist * fct * idx
    col <- col + dist

  }

  return(col)

}

#' @rdname vbl_doc_comp_offset
.comp_offset_row <- function(row, idx, dir, dist){

  if(grepl("top|bottom", dir)){

    fct <- ifelse(grepl("top", dir), -1, 1)
    dist <- dist * fct * idx
    row <- row + dist

  }

  return(row)

}



#' @title Limits utilities for 2D vibbles
#' @description
#' Accesses and manipulates 2D limits for `vbl2D` objects. These limits define
#' the bounding box of the space in `col`/`row` coordinates used for plotting.
#'
#' @param value For `lim<-()`, a 2D bounding box object as validated by
#'   \code{is_bb2D()}. Typically a list with components `col` and `row`.
#' @param slice Integer scalar. For \code{lim_slice()}, the slice index for
#'   which the 2D limits should be returned.
#'
#' @inheritParams vbl_doc
#'
#' @return
#' \itemize{
#'   \item \code{lim()}: The stored 2D limits of a `vbl2D` object.
#'   \item \code{lim<-()}: The modified `vbl2D` object with updated limits.
#'   \item \code{lim_plot()}: A 2D limits object adapted for plotting, taking
#'     offsets into account across all slices.
#'   \item \code{lim_slice()}: A 2D limits object for a single slice, adapted
#'     for the slice-specific offset position.
#' }
#'
#' @details
#' The \code{lim()} accessor and its replacement form operate on the 2D limits
#' stored in the `lim` attribute of a `vbl2D` object. These limits describe the
#' visible extent in `col`/`row` coordinates and may be changed to crop or
#' expand the displayed region in 2D.
#'
#' The 3D coordinate system limits (`ccs_limits`) of the original 3D vibble are not
#' affected by these functions and remain the primary identifiers of space and voxels for
#' merging and alignment purposes!
#'
#' \subsection{Offset handling}{
#' \itemize{
#'   \item \code{lim_plot()} returns limits adjusted for the cumulative offset
#'   applied when slices are staggered using `offset_dist` and `offset_dir`.
#'   This is typically used internally when computing plotting ranges.
#'   \item \code{lim_slice()} returns limits for a single slice. If the
#'   `vbl2D` object is offset, the limits are shifted according to the slice
#'   index so that they match the displayed position of that slice.
#' }
#' }
#'
#' @export
lim <- function(vbl2D){

  stopifnot(is_vbl2D(vbl2D))

  lim2D <- .vbl_attr(vbl2D, which = "lim")

  return(lim2D)

}

#' @rdname lim
#' @export
`lim<-` <- function(vbl2D, value){

  stopifnot(is_vbl2D(vbl2D))
  stopifnot(is_bb2D(value))

  attr(vbl2D, which = "lim") <- value

  return(vbl2D)

}

#' @rdname lim
#' @export
lim_plot <- function(vbl2D){

  stopifnot(is_vbl2D(vbl2D))

  lim2D <- lim(vbl2D)

  if(is_offset(vbl2D)){

    axis <- .offset_axis(vbl2D)
    dir <- offset_dir(vbl2D)
    dist <- offset_dist(vbl2D)
    n <- max(slice_offset_index(vbl2D))

    idx <- ifelse(grepl("right|bottom", dir), 2, 1)

    lim2D[[axis]][idx] <- .offset_comp_fn(dir)(lim2D[[axis]], value = dist*n)[idx]

  }

  return(lim2D)

}

#' @rdname lim
#' @export
lim_slice <- function(vbl2D, slice){

  stopifnot(is_vbl2D(vbl2D))
  stopifnot(slice %in% slices(vbl2D))

  lim2D <- lim(vbl2D)
  idx <- which(slices(vbl2D) == slice)-1

  if(is_offset(vbl2D) && idx != 0){

    axis <- .offset_axis(vbl2D)
    dir <- offset_dir(vbl2D)
    dist <- offset_dist(vbl2D)

    lim2D[[axis]] <- .offset_comp_fn(dir)(lim2D[[axis]], value = dist*idx)

  }

  return(lim2D)

}

#' @title Slices utilities for 2D vibbles
#' @description
#' Provides helpers to access slice values of a `vbl2D` object and to derive
#' zero-based offset indices used for staggered slice visualizations.
#'
#' @inheritParams vbl_doc
#'
#' @return
#' \itemize{
#'   \item `slices()`: An integer vector of slice values in the order they
#'   appear in the `vbl2D` object.
#'   \item `slice_bb()`: The \link[=is_bb2D]{2D bounding-box} of the slice.
#'   \item `slice_offset_index()`: An integer vector of the same length as
#'   `slices(vbl2D)` starting at 0, where the first slice gets index 0 and
#'   subsequent slices get increasing indices used as offset multipliers.
#' }
#'
#' @details
#' `slices()` returns the distinct slice values stored in a `vbl2D` object.
#' These values correspond to the original slice axis extracted in
#' `vibble2D()` and identify which slices are present in 2D space.
#'
#' `slice_offset_index()` does not represent slice numbers.
#' Instead, it provides zero-based indices that encode the relative position
#' of each slice for offset computation when `offset_dist` and `offset_dir`
#' are applied during visualization.
#'
#' @export
slices <- function(vbl2D){

  unique(vbl2D$slice)

}

#' @rdname slices
#' @export
slice_bb <- function(vbl2D, slice){

  stopifnot(is_vbl2D(vbl2D))
  stopifnot(slice %in% slices(vbl2D))

  purrr::map(
    .x = vbl2D[vbl2D$slice == slice, c("col", "row")],
    .f = range
  )

}

#' @rdname slices
#' @export
slice_offset_index <- function(vbl2D){

  seq_along(slices(vbl2D)) - 1

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
offset_attr <- function(vbl2D, which){

  which <- match.arg(which, choices = c("dir", "dist"))

  .vbl_attr(vbl2D, which = paste0("offset_", which))

}

#' @rdname vbl2D_attr
#' @export
offset_dir <- function(vbl2D){

  if(!is_offset(vbl2D)){

    warning("Input vbl2D is not offset.")

  }

  offset_attr(vbl2D, which = "dir")

}

#' @rdname vbl2D_attr
#' @export
`offset_dir<-` <- function(vbl2D, value){

  attr(vbl2D, which = "offset_dir") <- value

  vbl2D

}

#' @rdname vbl2D_attr
#' @export
offset_dist <- function(vbl2D){

  if(!is_offset(vbl2D)){

    warning("Input vbl2D is not offset.")

  }

  offset_attr(vbl2D, which = "dist")

}

#' @rdname vbl2D_attr
#' @export
`offset_dist<-` <- function(vbl2D, value){

  attr(vbl2D, which = "offset_dist") <- value

  vbl2D

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

