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
#' @importFrom dplyr desc
#'
#' @keywords internal
apply_offset <- function(vbl2D, offset_dist, offset_dir){

  idx_levels <- sort(unique(vbl2D$slice))
  idx_levels <- paste0("slice", idx_levels)

  vbl2D_offset <-
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
      col = comp_offset_col(col, idx = unique(idx), dir = {{offset_dir}}, dist = {{offset_dist}}),
      row = comp_offset_row(row, idx = unique(idx), dir = {{offset_dir}}, dist = {{offset_dist}})
    ) %>%
    dplyr::ungroup()

  if(grepl("-flip", offset_dir)){

    vbl2D_offset <- dplyr::arrange(vbl2D_offset, desc(idx))

  } else {

    vbl2D_offset <- dplyr::arrange(vbl2D_offset, idx)

  }

  attr_orig <- attributes(vbl2D)
  attr_new <- attributes(vbl2D_offset)
  for(nm in names(attr_orig)){

    if(!nm %in% names(attr_new)){

      attr(vbl2D_offset, which = nm) <- attr_orig[[nm]]

    }

  }

  return(vbl2D_offset)

}


bb2D <- function(vbl2D, var, slices = NULL, buffer = 0.05){

  is_vartype(vbl2D, var, "mask")
  stopifnot(any(vbl2D[[var]]))

  stopifnot(length(buffer) %in% c(1,2) && all(buffer >= 0))
  if(length(buffer) == 1){ buffer <- rep(buffer, 2) }

  vbl_wm <- vbl2D[vbl2D[[var]], ]

  if(!is.numeric(slices)){ slices <- sort(unique(vbl_wm$slice)) }

  purrr::map_df(
    .x = slices,
    .f = function(slice){

      slice_df <- dplyr::filter(vbl_wm, slice == {{slice}})

      bcol <- if(buffer[1] < 0){ diff(range(slice_df$col))*buffer[1] } else { buffer[1] }

      brow <- if(buffer[2] < 0){ diff(range(slice_df$row))*buffer[2] } else { buffer[2] }

      tibble(
        mask = var,
        plane = attr(vbl_wm, "plane"),
        slice = slice,
        cmin = min(slice_df$col) - bcol,
        cmax = max(slice_df$col) + bcol,
        rmin = min(slice_df$row) - brow,
        rmax = max(slice_df$row) + brow
      )

    }
  )

}

bb3D <- function(vbl, var, buffer = 0){

  is_vartype(vbl, var, "mask")

  purrr::imap(
    .x = vbl[vbl[[var]], ccs_labels],
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
#' Helper functions used by \link{apply_offset}() to shift `col` and `row`
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
#' `comp_offset_col()` it should contain `"left"` or `"right"`. For
#' `comp_offset_row()` it should contain `"top"` or `"bottom"`.
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
comp_offset_col <- function(col, idx, dir, dist){

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
comp_offset_row <- function(row, idx, dir, dist){

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

  inside <- if(base::isTRUE(strictly)){ 1 } else { c(1,2,3) }

  if(opt == "keep"){

    vbl2D <- vbl2D[res %in% inside, ]

  } else if(opt == "remove"){

    vbl2D <- vbl2D[!res %in% inside, ]

  } else {

    vbl2D[[opt]] <- res %in% inside

  }

  return(vbl2D)

}

#' @title Access offset-related attributes of a vbl2D object
#' @description Retrieve offset metadata attached to a `vbl2D` object.
#'
#' @param which Name of the offset attribute to extract. Valid options are *c('offset_dir', 'offset_dist', 'offset_plane')*.
#' @inherit vbl_doc params
#'
#' @return The stored offset attribute value, or `NULL` if not present.
#'
#' @details Offsets are applied in `\link{vibble2D}()` when `offset_dist > 0`.
#' The following attributes can then be accessed:
#' \itemize{
#'   \item `offset_dir`: Direction of voxel shift.
#'   \item `offset_dist`: Distance of the applied offset.
#'   \item `offset_plane`: Plane with respect to which the offset was defined.
#' }
#'
#' If the input object is not offset, a warning is issued
#' but the function still attempts to return the attribute.
#'
#' @seealso \link{vibble2D}(), \link{is_offset}()
#'
#' @rdname offset_attr
#' @export
offset_attr <- function(vbl2D, which){
  attr(vbl2D, which = which)
}

#' @rdname offset_attr
#' @export
offset_dir <- function(vbl2D){
  if(!is_offset(vbl2D)){ warning("Input vbl2D is not offset.") }
  offset_attr(vbl2D, which = "offset_dir")
}

#' @rdname offset_attr
#' @export
offset_dist <- function(vbl2D){
  if(!is_offset(vbl2D)){ warning("Input vbl2D is not offset.") }
  offset_attr(vbl2D, which = "offset_dist")
}


plane <- function(vbl2D){

  offset_attr(vbl2D, which = "plane")

}



