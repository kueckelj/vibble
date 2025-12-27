#' @title Create a 2D vibble from 3D voxel data
#' @description Convert a 3D `vbl` object into a 2D representation for a given
#' anatomical plane and selected slices. Optionally apply filtering conditions.
#'
#' @param slices Optional. Integer vector of slice positions in the selected `plane`
#' to keep. If `NULL`, all slices are retained.
#' @param crop Defines the \link[=vbl_doc_ref_bb]{data bounding box}
#' applied to all slices in native (pre-offset) coordinates.
#'
#' \itemize{
#'   \item{`NULL`: }{ No cropping.}
#'   \item{`limit`: }{ A single spatial \link[=is_limit]{limit} recycled for both `col` and `row`.}
#'   \item{`bb2D`: }{ A \link[=is_bb2D]{2D bounding box} with separate limits for `col` and `row`.}
#' }
#'
#' When the supplied bounding box is smaller than the slice extents, only voxels
#' inside this region are retained, giving `crop` a filtering effect.
#'
#' @param expand An \link[=is_expand]{expand} specification applied to the col/row limits
#' of `crop` **after** interpretation and potential filtering to set the \link[=vbl_doc_ref_bb]{screen bounding box}.
#' @param .cond An expression evaluated with \link[rlang:args_data_masking]{data-masking}
#' to filter voxels. If `NULL`, all voxels are retained.
#' @param .by A \link[dplyr:dplyr_tidy_select]{tidy-selection} of columns to
#' group by before applying the filtering logic of `.cond`.
#'
#' @inherit vbl_doc params
#'
#' @return A \link[=vbl_doc_vbl2D]{2D vibble}.
#'
#' @export

vibble2D <- function(vbl,
                     plane,
                     slices = NULL,
                     crop = NULL,
                     expand = FALSE,
                     .cond = NULL,
                     .by = NULL){

  # sanity checks and prep
  plane <- .match_arg(plane, choices = vbl_planes)

  req_axes <- req_axes_2D(plane = plane)
  slice_axis <- req_axes["slice"]

  # change class name HERE to allow x,y,z manipulation/renaming
  class(vbl) <- stringr::str_replace(class(vbl), "vbl", "vbl2D")

  # 3D to 2D -> col, row, slice, slice_idx
  vbl2D <-
    dplyr::rename(vbl, !!!req_axes) %>%
    dplyr::select(col, row, slice, dplyr::everything())

  if(!is.null(slices)){

    .stop_if_not(is_slice_set(slices))

    vbl2D <- dplyr::filter(vbl2D, slice %in% {{ slices }})

  }

  idx_levels <- sort(slices(vbl2D))
  idx_levels <- paste0("slice", idx_levels)

  vbl2D <-
    dplyr::mutate(
      .data = vbl2D,
      slice_idx = as.numeric(factor(paste0("slice", slice), levels = idx_levels))-1,
      slice_idx = as.integer(slice_idx)
    )

  offset_col(vbl2D) <- 0L
  offset_row(vbl2D) <- 0L
  plane(vbl2D) <- plane

  # apply condition
  .cond_quo <- rlang::enquo(.cond)
  if(!rlang::quo_is_null(.cond_quo)){

    vbl2D <- dplyr::filter(vbl2D, !!.cond_quo, .by = {{ .by }})

  }

  # crop - interpret
  if(is_limit(crop)){

    crop <- list(col = crop, row = crop)

    .stop_if_not(is_bb2D(crop))

  } else if(is_bb2D(crop)){

    # pass, already valid input

  } else {

    crop <- list(col = range(vbl2D$col), row = range(vbl2D$row))

  }

  # crop - apply
  slices_before <- slices(vbl2D)

  vbl2D <-
    dplyr::filter(
      .data = vbl2D,
      within_limits(col, l = crop$col) &
      within_limits(row, l = crop$row)
    )

  slices_after <- slices(vbl2D)

  # crop - feedback
  if(length(slices_before) != length(slices_after)){

    if(length(slices_after) == 0){

      crop_ref <- rlang::quo_text(rlang::enquo(crop))
      msg <- c(glue::glue("Cropping with crop = {crop_ref} removed all voxel-level data."))
      rlang::abort(msg)

    } else if(length(slices_after) != length(slices_before)){

      slices_missing <-
        setdiff(slices_before, slices_after) %>%
        .slice_collapse(last = " and ")

      msg <- c(
        glue::glue("Cropping with crop = {crop_ref} removed all voxel-level data of some slices completely."),
        i = "Removed slices: {slices_missing}"
      )

      rlang::warn(msg)

    }

  }

  # set ref bbs
  data_bb(vbl2D) <- crop
  screen_bb(vbl2D) <- expand_bb2D(bb2D = crop, expand = expand)

  # sort cols and return
  dplyr::select(vbl2D, col, row, slice, slice_idx, dplyr::everything())

}
