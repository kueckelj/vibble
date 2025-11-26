# Spatial reference and coordinate-system utilities.



#' Apply slice-wise offsets to a 2D vibble
#'
#' Shift `col` and `row` coordinates of a `vbl2D` object by slice-specific offsets.
#' This is used to arrange multiple slices next to each other for 2D plotting.
#'
#'
#' @param offset_dist Integer distance between adjacent slices in coordinate units.
#' This is typically derived from a relative distance specified in \link{vibble2D}().
#'
#' @inherit vbl_doc params
#'
#' @return
#' The input `vbl2D` tibble with updated `col` and `row` coordinates that include the slice-wise offsets.
#' All attributes of the original object are preserved where possible.
#'
#' @details
#' The function computes a slice index `idx` based on the unique values of `slice` and the requested `offset_dir`.
#' Coordinates are shifted by calling \link{comp_offset_col}() and \link{comp_offset_row}() inside grouped `dplyr` operations so that offsets are consistent within plane, sequence, slice, and mask combinations.
#'
#' @seealso
#' \link{vibble2D}(), \link{comp_offset_col}(), \link{comp_offset_row}()
#'
#' @keywords internal
.apply_offset <- function(vbl2D, offset_dir, offset_dist){

  idx_levels <- sort(unique(vbl2D$slice))
  idx_levels <- paste0("slice", idx_levels)

  assign("vbl2D", vbl2D, envir = .GlobalEnv)

  vbl2D <-
    dplyr::mutate(
      .data = vbl2D,
      idx = as.numeric(factor(paste0("slice", slice), levels = idx_levels))-1
    ) %>%
    dplyr::group_by(
      dplyr::across(
        .cols = dplyr::any_of(c("plane", "sequence", "slice", "mask"))
      )
    ) %>%
    dplyr::mutate(
      col = .comp_offset_col(col, idx = unique(idx), dir = {{offset_dir}}, dist = {{offset_dist}}),
      row = .comp_offset_row(row, idx = unique(idx), dir = {{offset_dir}}, dist = {{offset_dist}})
    ) %>%
    dplyr::ungroup()

  if(grepl("-flip", offset_dir)){

    vbl2D <- dplyr::arrange(vbl2D, dplyr::desc(idx))

  } else {

    vbl2D <- dplyr::arrange(vbl2D, idx)

  }

  return(vbl2D)

}


#' @title Compute 2D bounding boxes per slice
#' @description
#' Compute per-slice 2D bounding boxes from a `vbl2D` object based on its `col` and `row` coordinates.
#' Optionally filter voxels, restrict to selected slices, and expand boxes by a buffer.
#'
#' @param cond Optional filter condition evaluated in the context of `vbl2D`.
#'   Use an unquoted expression, for example `cond = tumor == TRUE`.
#'   If omitted, all rows are considered.
#' @param slices Optional numeric vector of slice indices to include.
#'   If not numeric or `NULL`, all slices present in `vbl2D$slice` are used.
#' @param buffer Numeric vector of length one or two defining the non-negative buffer added to the
#'   column and row ranges.
#'   If a single value is supplied, it is recycled to both axes.
#'   Values are interpreted in the units of `col` and `row`.
#' @param mask Character scalar stored in the output column `mask` to label the bounding boxes.
#'
#' @inheritParams vbl_doc
#'
#' @return
#' A tibble with one row per slice and the following columns.
#' \itemize{
#'   \item `mask`: The mask label passed via `mask`.
#'   \item `plane`: The plane of the input `vbl2D` as returned by `plane(vbl2D)`.
#'   \item `slice`: Slice index.
#'   \item `cmin`, `cmax`: Minimum and maximum column coordinates after buffering.
#'   \item `rmin`, `rmax`: Minimum and maximum row coordinates after buffering.
#' }
#'
#' @details
#' The function first filters `vbl2D` using `cond` if provided.
#' If `slices` is not numeric, the set of slices is derived from the unique values in `vbl2D$slice`.
#' For each slice, the range of `col` and `row` is computed and expanded symmetrically by the given buffer.
#'
#' The buffer must be non-negative and of length one or two.
#' If a single value is provided, it is used for both the column and row directions.
#' The resulting bounding boxes are returned as a tibble that can be used, for example, by plotting helpers
#' such as `layer_bb()`.
#'
#' @seealso
#' \link{bb3D}() for 3D bounding boxes.
#' \link{plane}() for accessing the plane attribute of a `vbl2D` object.
#' \link{layer_bb}() for drawing bounding boxes in 2D plots.
#'
#' @importFrom rlang enquo
#'
#' @examples
#' vbl <- example_vbl()
#' vbl2D <- vibble2D(vbl, plane = "axi", slices = 60:62)
#'
#' # Example 1: Bounding boxes per slice for all voxels
#' bb_all <- bb2D(vbl2D)
#'
#' # Example 2: Bounding boxes for brain mask with a buffer of 2 units
#' bb_brain <- bb2D(vbl2D, cond = brain == TRUE, buffer = 2, mask = "brain")
#'
#' @export
bb2D <- function(vbl2D, cond = enquo(), slices = NULL, buffer = 0, mask = "cond"){

  vbl2D <- dplyr::filter(vbl2D, !!cond)

  stopifnot(length(buffer) %in% c(1,2) && all(buffer >= 0))
  if(length(buffer) == 1){ buffer <- rep(buffer, 2) }

  if(!is.numeric(slices)){ slices <- sort(unique(vbl2D$slice)) }

  purrr::map_df(
    .x = slices,
    .f = function(slice){

      slice_df <- dplyr::filter(vbl2D, slice == {{slice}})

      bcol <- if(buffer[1] < 0){ diff(range(slice_df$col))*buffer[1] } else { buffer[1] }

      brow <- if(buffer[2] < 0){ diff(range(slice_df$row))*buffer[2] } else { buffer[2] }

      tibble(
        mask = mask,
        plane = plane(vbl2D),
        slice = slice,
        cmin = min(slice_df$col) - bcol,
        cmax = max(slice_df$col) + bcol,
        rmin = min(slice_df$row) - brow,
        rmax = max(slice_df$row) + brow
      )

    }
  )

}



#' @title Compute 3D bounding box
#' @description
#' Compute a 3D bounding box (`bb3D`) from a vibble by extracting coordinate ranges for the
#' spatial axes `x`, `y`, and `z`.
#'
#' @param cond Optional tidy-selection condition used to filter the vibble before computing the bounding box.
#' Use unquoted expressions, e.g. `brain == TRUE`. If omitted, all rows are considered.
#' @param buffer Numeric scalar defining how much to expand the bounding box. If `buffer >= 1`, it
#' is interpreted as an absolute number of voxels.If `0 < buffer < 1`, it is interpreted as a
#' proportion of the axis-specific range.
#'
#' @inheritParams vbl_doc
#'
#' @return
#' A named list of length three (for `x`, `y`, `z`) where each element is a numeric
#' vector of length two defining the lower and upper limits of the bounding box.
#'
#' @details
#' The function first filters the vibble using `cond` if provided.
#' For each spatial axis, the range is computed from the voxel coordinates.
#'
#' If a buffer is supplied, the bounding box is expanded outward.
#' Proportional buffers (`buffer < 1`) are converted to absolute sizes based on the axis-specific range.
#'
#' Expanded limits are always trimmed to the global coordinate-space limits
#' defined in the vibble attribute `ccs_limits()`.
#' This ensures bounding boxes never exceed the valid spatial space of the vibble.
#'
#' @note
#' Bounding boxes are compatible with \link{filter_bb3D}() and other functions that
#' require valid 3D limit vectors.
#' They follow the same structural rules as objects validated by \link{is_bb3D}().
#'
#' @seealso
#' \link{filter_bb3D}() for subsetting using bounding boxes.
#' \link{is_bb3D}() for structure validation.
#' \link{ccs_limits}() for accessing the global coordinate limits of a vibble.
#'
#' @importFrom rlang enquo
#'
#' @examples
#' # Example 1: Bounding box around all voxels
#' vbl <- example_vbl()
#' bb <- bb3D(vbl)
#'
#' # Example 2: Bounding box around tumor region with a proportional buffer
#' bb_tumor <- bb3D(vbl, cond = tumor == TRUE, buffer = 0.1)
#'
#' @export
bb3D <- function(vbl, cond = enquo(), buffer = 0){

  vbl <- dplyr::filter(vbl, !!cond)

  purrr::imap(
    .x = vbl[, ccs_labels],
    .f = function(avar, axis){

      alim <- ccs_limits(vbl)[[axis]]
      r <- range(avar)

      if(buffer != 0){

        if(buffer < 1){

          buffer <- diff(r)*buffer

        }

        r[1] <- r[1]-buffer
        r[2] <- r[2]+buffer

        r[1] <- ifelse(r[1] < min(alim), min(alim), r[1])
        r[2] <- ifelse(r[2] > max(alim), max(alim), r[2])

      }

      return(r)

    })

}


#' @title Compute slice-wise coordinate offsets
#' @description
#' Helper functions used by \link{.apply_offset}() to shift `col` and `row`
#' coordinates for slice-wise layout of 2D vibbles.
#'
#' @details
#' `comp_offset_col()` and `comp_offset_row()` apply linear offsets of the
#' form `dist * idx` to the respective coordinate:
#' \itemize{
#'   \item `comp_offset_col()` shifts `col` when `dir` contains `"left"` or
#'   `"right"`. `"left"` yields a negative shift, `"right"` a positive shift
#'   \item `comp_offset_row()` shifts `row` when `dir` contains `"top"` or
#'   `"bottom"`. `"top"` yields a negative shift, `"bottom"` a positive shift
#' }
#'
#' The index `idx` is typically derived from slice indices in
#' \link{apply_offset}() and controls how far each slice is moved relative
#' to the others.
#'
#' @param col,row
#' Numeric vectors of column or row coordinates to be shifted.
#'
#' @param idx
#' Numeric scalar or vector giving the per-slice index used to scale `dist`.
#'
#' @param dir
#' Character string indicating the direction of the shift. For
#' `.comp_offset_col()` it should contain `"left"` or `"right"`. For
#' `.comp_offset_row()` it should contain `"top"` or `"bottom"`.
#'
#' @param dist
#' Numeric base distance by which coordinates are shifted per unit of `idx`.
#'
#' @return
#' A numeric vector of shifted coordinates (`col` or `row`).
#'
#' @examples
#' comp_offset_col(col = 1:5, idx = 1, dir = "right", dist = 10)
#' comp_offset_row(row = 1:5, idx = 2, dir = "top", dist = 5)
#'
#' @name comp_offset
NULL

#' @export
#' @keywords internal
#' @rdname comp_offset
.comp_offset_col <- function(col, idx, dir, dist){

  if(grepl("left|right", dir)){

    fct <- ifelse(grepl("left", dir), 1, -1)
    dist <- dist * fct * idx

    col <- col + dist

  }

  return(col)

}

#' @export
#' @keywords internal
#' @rdname comp_offset
.comp_offset_row <- function(row, idx, dir, dist){

  if(grepl("top|bottom", dir)){

    fct <- ifelse(grepl("top", dir), -1, 1)
    dist <- dist * fct * idx

    row <- row + dist

  }

  return(row)

}




identify_voxels_in_poly <- function(vbl2D,
                                    poly,
                                    strictly,
                                    opt = "keep"){

  res <-
    sp::point.in.polygon(
      point.x = vbl2D[["col"]],
      point.y = vbl2D[["row"]],
      pol.x = poly[["col"]],
      pol.y = poly[["row"]]
    )

  inside <- if(isTRUE(strictly)){ 1 } else { c(1,2,3) }

  if(opt == "keep"){

    vbl2D <- vbl2D[res %in% inside, ]

  } else if(opt == "remove"){

    vbl2D <- vbl2D[!res %in% inside, ]

  } else {

    vbl2D[[opt]] <- res %in% inside

  }

  return(vbl2D)

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
lim <- function(vbl2D){

  .vbl_attr(vbl2D, which = "lim")

}

#' @rdname vbl2D_attr
#' @export
`lim<-` <- function(vbl2D, value){

  stopifnot(is.list(value))
  stopifnot(is.null(value$col) | is_limit(value$col))
  stopifnot(is.null(value$row) | is_limit(value$row))

  attr(vbl2D, which = "lim") <- value

  return(vbl2D)

}

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












