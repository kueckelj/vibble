# Spatial reference and coordinate-system utilities.


#' Apply slice-wise offsets to a 2D vibble
#'
#' Shift `col` and `row` coordinates of a `vbl2D` object by slice-specific offsets.
#' This is used to arrange multiple slices next to each other for 2D plotting.
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
#' @export
.apply_offset <- function(vbl2D, offset_col, offset_row){

  stopif(is_offset(vbl2D))

  slim <- screen_limits(vbl2D)

  # convert relative offsets
  if(is_rel(offset_col)){

    offset_col <- diff(slim$col) * offset_col

  }

  if(is_rel(offset_row)){

    offset_row <- diff(slim$row) * offset_row

  }

  # truncate to integer
  offset_col <- as.integer(offset_col)
  offset_row <- as.integer(offset_row)

  # save idx in case the var exists
  vbl_names <- names(vbl2D)
  if("idx" %in% vbl_names){ idx_var <- vbl2D[["idx"]] }

  # apply offset
  idx_levels <- slices(vbl2D)
  idx_levels <- paste0("slice", idx_levels)

  vbl2D <-
    dplyr::mutate(
      .data = vbl2D,
      idx = as.numeric(factor(paste0("slice", slice), levels = idx_levels))-1
    ) %>%
    dplyr::group_by(slice) %>%
    dplyr::mutate(
      col = .comp_offset_col(x = col, idx = unique(idx), offset = {{offset_col}}),
      row = .comp_offset_row(x = row, idx = unique(idx), offset = {{offset_row}}),
      idx = NULL
    ) %>%
    dplyr::ungroup()

  offset_col(vbl2D) <- offset_col
  offset_row(vbl2D) <- offset_row

  # restore idx in case the var exists  - and col ordering
  if("idx" %in% vbl_names){

    vbl2D[["idx"]] <- idx_var
    vbl2D <- vbl2D[,vbl_names]

  }

  return(vbl2D)

}


#' @title Convert bb2D to data frame
#' @description Convert a 2D bounding box (`bb2D`) into a one-row tibble with
#' column and row minima and maxima.
#'
#' @param bb2D A 2D bounding box as validated by \link{is_bb2D}().
#'
#' @return A tibble with columns `cmin`, `cmax`, `rmin`, and `rmax`.
#'
#' @export
as_bb2D_df <- function(bb2D){

  tibble::tibble(
    cmin = min(bb2D$col),
    cmax = max(bb2D$col),
    rmin = min(bb2D$row),
    rmax = max(bb2D$row)
  )

}

#' @title Average 2D bounding boxes
#' @description
#' Compute the element-wise mean of two 2D bounding boxes (`bb2D`).
#'
#' @param a,b Two `bb2D` objects as validated by \link{is_bb2D}().
#'
#' @return
#' A `bb2D` object (list with elements `col` and `row`) where each limit is the
#' arithmetic mean of the corresponding limits in `bb1` and `bb2`.
#'
#' @details
#' Both inputs must have the same named axes (typically `col` and `row`). The
#' lower and upper bounds of each axis are averaged separately.
#'
#' @examples
#' vbl <- example_vbl()
#' vbl2D <- vibble2D(vbl, plane = "axi", slices = 60)
#' bb_slice <- slice_bb(vbl2D, slice = 60)
#' bb_screen <- screen_limits(vbl2D, slice = 60)
#' avg_bb <- avg_bb2D(bb_slice, bb_screen)
#'
#' @export
avg_bb2D <- function(a, b){

  stopifnot(is_bb2D(a))
  stopifnot(is_bb2D(b))

  purrr::map2(.x = a, .y = b, .f = ~ (.x + .y) / 2)

}


#' @title Compute 2D bounding boxes
#' @description
#' Compute 2D bounding boxes per slice for `vbl2D` objects.
#' Two variants are provided.
#' \itemize{
#'   \item `bb2D_df()`: tibble with one row per slice, convenient for plotting and joins.
#'   \item `bb2D_lst()`: list indexed by slice, convenient for passing bounding boxes to other helpers.
#' }
#'
#' @param .cond Optional logical filter expression evaluated on `vbl2D` before computing bounding boxes.
#' The expression is evaluated with \link[rlang:args_data_masking]{data-masking} semantics.
#' @param slices Optional numeric vector of slice indices to include. Defaults to all \link{slices}.
#' @param expand Optional. If provided, the \link[=is_expand]{expand} value is
#' used to expand the 2D limits of each slice-wise bounding box.
#'
#' @inheritParams vbl_doc
#'
#' @return
#' `bb2D_df()` returns a tibble with one row per slice and columns.
#' \itemize{
#'   \item `plane`: Plane attribute of `vbl2D`.
#'   \item `slice`: Slice index.
#'   \item `cmin`, `cmax`: Column limits with optional buffer applied.
#'   \item `rmin`, `rmax`: Row limits with optional buffer applied.
#' }
#'
#' `bb2D_lst()` returns a list of length `max(slices)`.
#' \itemize{
#'   \item Entries at positions corresponding to `slices` contain a list with elements
#'     `col` and `row`, each a length-two numeric vector giving the buffered limits.
#'   \item Positions for slices not requested remain `NULL`, making direct indexing by slice number possible.
#' }
#'
#' The two output formats are designed for different downstream uses.
#' The data frame variant is well suited for plotting layers such as rectangle overlays.
#' The list variant matches interfaces that expect per-slice bounding boxes in `list(col = ..., row = ...)` form.
#'
#' @note
#' For a \link[=is_bb2D]{2D bounding box}, a column- and a row-\link{limit} must be computed,
#' which requires at least two different column- and two different row values per slice. If
#' a slice does not match these requirements the functions silently skip these slices.
#'
#' @seealso
#' \link{vbl_doc_abs_rel} for absolute and relative specifications.
#' \link{plane}() for accessing the plane of a `vbl2D` object.
#'
#' @examples
#' vbl <- example_vbl()
#' vbl2D <- vibble2D(vbl, plane = "axi", slices = 60:62)
#'
#' # Tibble output
#' bb_df <- bb2D_df(vbl2D, .cond = brain == TRUE, buffer = c(0.1, 0.1))
#'
#' # List output indexed by slice
#' bb_lst <- bb2D_lst(vbl2D, .cond = brain == TRUE, buffer = as_abs(2))
#' bb_lst[[60]]  # bounding box for slice 60
#'
#' @name vbl_doc_bb2D
NULL

#' @rdname vbl_doc_bb2D
#' @export
bb2D_df <- function(vbl2D,
                    .cond = NULL,
                    .by = NULL,
                    slices = NULL,
                    expand = FALSE){

  stopifnot(is_vbl2D(vbl2D))
  stopifnot(is_expand(expand))

  .by_quo <- rlang::enquo(.by)
  .cond_quo <- rlang::enquo(.cond)

  if(!rlang::quo_is_null(.cond_quo)){

    vbl2D <- dplyr::filter(vbl2D, !!.cond_quo, .by = {{ .by }})

  }

  stopifnot(is.null(slices) | is.numeric(slices))
  slices <- if(!is.numeric(slices)){ slices(vbl2D) }

  purrr::map_df(
    .x = slices,
    .f = function(slice){

      slice_df <- vbl2D[vbl2D$slice %in% slice, c("col", "row")]

      if(!.bb2D_possible(slice_df)) return(NULL)

      bb <-
        purrr::map2(
          .x = slice_df,
          .y = expand,
          .f = ~ .apply_expand(range(.x), expand = .y)
        )

      tibble::tibble(
        plane = plane(vbl2D),
        slice = slice,
        cmin = min(bb$col),
        cmax = max(bb$col),
        rmin = min(bb$row),
        rmax = max(bb$row)
      )

    }
  )

}


#' @rdname vbl_doc_bb2D
#' @export
bb2D_lst <- function(vbl2D,
                     .cond = NULL,
                     .by = NULL,
                     slices = NULL,
                     expand = FALSE){

  stopifnot(is_vbl2D(vbl2D))
  stopifnot(is_expand(expand))

  .by_quo <- rlang::enquo(.by)
  .cond_quo <- rlang::enquo(.cond)

  if(!rlang::quo_is_null(.cond_quo)){

    vbl2D <- dplyr::filter(vbl2D, !!.cond_quo, .by = {{ .by }})

  }

  stopifnot(is.null(slices) | is.numeric(slices))
  slices <- if(!is.numeric(slices)){ slices(vbl2D)}
  out <- vector(mode = "list", length = max(slices))

  out[slices] <-
    purrr::map(
      .x = slices,
      .f = function(slice){

        slice_df <- vbl2D[vbl2D$slice %in% slice, c("col", "row")]

        if(!.bb2D_possible(slice_df)) return(NULL)

        purrr::map2(
          .x = slice_df,
          .y = expand,
          .f = ~ .apply_expand(range(.x), expand = .y)
        )

      }
    )

  return(out)

}

#' @keywords internal
.bb2D_possible <- function(slice_df){

  dplyr::n_distinct(slice_df$col) >= 2 &
  dplyr::n_distinct(slice_df$row) >= 2

}


#' @title Compute a 3D bounding box
#' @description
#' Compute a 3D bounding box (`bb3D`) from a vibble by extracting coordinate ranges for the
#' spatial axes `x`, `y`, and `z`.
#'
#' @inheritParams bb2D
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
#' bb_tumor <- bb3D(vbl, .cond = tumor == TRUE, buffer = 0.1)
#'
#' @export
bb3D <- function(vbl, .cond, buffer = 0){

  vbl <- dplyr::filter(vbl, !!.cond)

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












