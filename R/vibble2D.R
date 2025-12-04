#' @title Create a 2D vibble from 3D voxel data
#' @description Convert a 3D `vbl` object into a 2D representation for a given anatomical plane and selected slices. Optionally apply filtering conditions and spatial offsets for visualization.
#'
#' @param slices Optional. Integer vector of slice positions in the selected `plane`
#' to keep. If `NULL`, all slices are retained.
#' @param lim Defines the 2D spatial limits of the resulting `vbl2D`. By default, the
#' \link[=ccs_limits]{coordinate-space} limits of the input vibble are and mapped to
#' col and rowdepending on `plane`. User can adjust that by supplying:
#'
#'   \itemize{
#'     \item  a valid \link[=is_limit]{limit}: a numeric vector of length two which is recycled for col and row.
#'     \item a list with `col` and `row` elements, each being a \link[=is_limit]{limit} for the respective axis.
#'   }
#'
#' When the supplied \link[=is_bb2D]{2D bounding-box} is smaller than the slice extents,
#' only voxels inside this region are retained, giving `lim` a filtering effect.
#'
#' @param expand An \link[=is_expand]{expand} specification applied to the col/row limits **after**
#' the interpretation and potential filtering of `lim`.
#' @param .cond Optional. An expression evaluated with \link[rlang:args_data_masking]{data-masking}
#' to filter voxels. If `NULL`, all voxels are retained.
#' @param .by Optional. A \link[dplyr:dplyr_tidy_select]{tidy-selection} of columns to
#' group by before applying the filtering logic of `.cond`.
#'
#' @inherit vbl_doc params
#'
#' @return A `tibble` with class `vbl2D` containing columns `col`, `row`, `slice`,
#' and all remaining variables from `vbl`. Attributes store the used plane and any
#' applied offsets.
#'
#' @details The function derives the required axes using \link{req_axes_2D}() and
#' renames the corresponding coordinate columns to `col`, `row`, and `slice`,
#' and optionally filters by `slices`, `lim` and `.cond`.
#'
#' \itemize{
#'   \item `col` and `row` store the 2D coordinates in voxel space.
#'   \item `slice` stores the position along the third axis in the selected `plane`.
#'   \item Attributes `"offset_dist"`, `"offset_dir"`, and `"plane"`
#'   describe the applied spatial offset and the original plane.
#' }
#'
#' @export

vibble2D <- function(vbl,
                     plane,
                     slices = NULL,
                     lim = NULL,
                     expand = FALSE,
                     offset_col = 0,
                     offset_row = 0,
                     .cond = NULL,
                     .by = NULL){

  # sanity checks and prep
  plane <- match.arg(plane, choices = vbl_planes)

  req_axes <- req_axes_2D(plane = plane)
  slice_axis <- req_axes["slice"]

  if(!is.numeric(slices)){ slices <- unique(vbl[[slice_axis]]) }

  # change class name HERE to allow x,y,z manipulation/renaming
  class(vbl) <- stringr::str_replace(class(vbl), "vbl", "vbl2D")

  # 3D to 2D
  vbl2D <-
    dplyr::rename(vbl, !!!req_axes) %>%
    dplyr::filter(slice %in% {{slices}}) %>%
    dplyr::select(col, row, slice, dplyr::everything())

  # apply lim
  if(is.numeric(lim)){

    stopifnot(is.numeric(lim) & length(lim) == 2)

    lim <- list(col = lim, row = lim)

  } else if(is.list(lim)){

    stopifnot(is_bb2D(lim))

  } else {

    lim <- list()
    lim[c("col", "row")] <- ccs_limits(vbl)[req_axes[c("col", "row")]]

  }

  vbl2D <-
    dplyr::filter(
      .data = vbl2D,
      within_limits(col, l = lim$col) &
      within_limits(row, l = lim$row)
      )

  screen_limits(vbl2D) <- expand_bb2D(bb2D = lim, expand = expand)

  # apply condition
  .by_quo <- rlang::enquo(.by)
  .cond_quo <- rlang::enquo(.cond)
  if(!rlang::quo_is_null(.cond_quo)){

    vbl2D <- dplyr::filter(vbl2D, !!.cond_quo, .by = {{ .by_quo }})

  }

  # apply offset
  offset_col(vbl2D) <- 0L
  offset_row(vbl2D) <- 0L
  vbl2D <- .apply_offset(vbl2D, offset_col = offset_col, offset_row = offset_row)

  # set vbl2D attributes
  plane(vbl2D) <- plane

  return(vbl2D)

}
