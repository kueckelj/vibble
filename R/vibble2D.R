#' @title Create a 2D vibble from 3D voxel data
#' @description Convert a 3D `vbl` object into a 2D representation for a given anatomical plane and selected slices. Optionally apply filtering conditions and spatial offsets for visualization.
#'
#' @param slices Optional numeric vector of slice positions in the selected `plane`
#' to keep. If `NULL`, all slices are retained.
#' @param cond An expression evaluated with \link[rlang:args_data_masking]{tidyverse's data-masking}
#' to filter voxels. If `NULL`, all voxels are retained.
#'
#' @inherit vbl_doc params
#'
#' @return A `tibble` with class `vbl2D` containing columns `col`, `row`, `slice`,
#' and all remaining variables from `vbl`. Attributes store the used plane and any
#' applied offsets.
#'
#' @details The function derives the required axes using \link{req_axes_2d}() and
#' \link{switch_axis_label}(). It then selects the corresponding coordinate columns,
#' renames them to `col`, `row`, and `slice`, and optionally filters by `slices` and `cond`.
#'
#' If `offset_dist > 0`, voxels are shifted along the specified `offset_dir`,
#' and offset-related attributes are attached.
#'
#' Internally, summary statistics for `col`, `row`, and `slice` are updated via \link{update_var_smr}().
#'
#' \itemize{
#'   \item `col` and `row` store the 2D coordinates in voxel space.
#'   \item `slice` stores the position along the third axis in the selected `plane`.
#'   \item Attributes `"offset"`, `"offset_dist"`, `"offset_dir"`, and `"plane"`
#'   describe the applied spatial offset and the original plane.
#' }
#'
#' @seealso \link{ggplane}(), \link{vbl_layer}(), \link{offset_attr}()
#'
#' @importFrom rlang enquo
#'
#' @examples
#' vbl <- example_vbl()
#'
#' vbl2D <- vibble2D(
#'   vbl = vbl,
#'   plane = "axi",
#'   slices = 90:100,
#'   offset_dist = 0.1,
#'   offset_dir = "left"
#' )
#'
#' dplyr::glimpse(vbl2D)
#'
#' @export

vibble2D <- function(vbl,
                     plane,
                     slices = NULL,
                     lim = NULL,
                     offset_dist = 0,
                     offset_dir = "left",
                     cond = enquo()){

  req_axes <- req_axes_2d(plane = plane)

  col_axis <- unname(req_axes["col"])
  row_axis <- unname(req_axes["row"])
  slice_axis <- switch_axis_label(plane)

  class(vbl) <- stringr::str_replace(class(vbl), "vbl", "vbl2D")

  # 3D to 2D
  vbl2D_all <-
    dplyr:::select(
      .data = vbl,
      !!!req_axes,
      slice := {{slice_axis}},
      dplyr::everything()
      )

  if(is.numeric(slices)){

    vbl2D <- dplyr::filter(vbl2D_all, slice %in% {{slices}})

  } else {

    vbl2D <- vbl2D_all

  }

  # apply limits
  lim_col <- NULL
  lim_row <- NULL
  if(is.numeric(lim)){

    stopifnot(is.numeric(lim) & length(lim) == 2)

    lim_col <- lim
    lim_row <- lim

  } else if(is.list(lim)){

    lim_col <- if(is.numeric(lim[["col"]]) && length(lim[["col"]]) > 1){ range(lim[["col"]])}
    lim_row <- if(is.numeric(lim[["row"]]) && length(lim[["row"]]) > 1){ range(lim[["row"]])}

  }

  vbl2D <-
    dplyr::filter(
      .data = vbl2D,
      within_limits(col, lim_col, null_ok = TRUE) &
      within_limits(row, lim_row, null_ok = TRUE)
      )

  # apply condition
  if(!rlang::quo_is_missing(cond) && !rlang::quo_is_null(cond)){

    vbl2D <- dplyr::filter(vbl2D, !!cond)

  }

  # offset voxels
  if(offset_dist > 0){

    offset_dir <- match.arg(offset_dir, choices = offset_dir_choices)

    offset_axis <- ifelse(stringr::str_detect(offset_dir, "left|right"), "col", "row")

    if(offset_dist < 1){

      mx <- max(ccs_limits(vbl)[[req_axes[[offset_axis]]]])
      offset_dist <- mx*offset_dist

    }

    # integer positions required for geom_raster
    offset_dist <- as.integer(offset_dist)

    vbl2D <- .apply_offset(vbl2D, offset_dist = offset_dist, offset_dir = offset_dir)

  }

  # set vbl2D attributes
  lim(vbl2D) <- list(col = lim_col, row = lim_row)
  plane(vbl2D) <- plane
  offset_dist(vbl2D) <- offset_dist #
  offset_dir(vbl2D) <- if(offset_dist > 0) offset_dir # else NULL

  vbl2D <- update_var_smr(vbl2D, vars = c("col", "row", "slice"))

  return(vbl2D)

}
