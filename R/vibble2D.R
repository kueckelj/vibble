#' @title Create a 2D vibble from 3D voxel data
#' @description Convert a 3D `vbl` object into a 2D representation for a given anatomical plane and selected slices. Optionally apply filtering conditions and spatial offsets for visualization.
#'
#' @param slices Optional numeric vector of slice positions in the selected `plane`
#' to keep. If `NULL`, all slices are retained.
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
                     offset_dist = 0,
                     offset_dir = "left",
                     cond = enquo()){

  req_axes <- req_axes_2d(plane = plane)

  col_axis <- unname(req_axes["col"])
  row_axis <- unname(req_axes["row"])
  slice_axis <- switch_axis_label(plane)

  ccs_limits <- attr(vbl, which = "ccs_limits")

  # 3D to 2D
  vbl2D_all <- dplyr::select(vbl, !!!req_axes, slice := {{slice_axis}}, dplyr::everything())
  attr(vbl2D_all, which = "plane") <- plane

  if(is.numeric(slices)){

    vbl2D <- dplyr::filter(vbl2D_all, slice %in% {{slices}})

  } else {

    vbl2D <- vbl2D_all

  }

  # apply condition
  if(!rlang::quo_is_missing(cond) && !rlang::quo_is_null(cond)){

    vbl2D <- dplyr::filter(vbl2D, !!cond)

  }


  # offset voxels
  if(offset_dist > 0){

    offset_dir_choices <-
      c("left", "right", "top", "bottom") %>%
      c(., paste0(., "-flip"))

    offset_dir <- match.arg(offset_dir, offset_dir_choices)

    offset_axis <- ifelse(stringr::str_detect(offset_dir, "left|right"), "col", "row")

    if(offset_dist < 1){

      mx <- max(ccs_limits(vbl)[[req_axes[[offset_axis]]]])
      offset_dist <- mx*offset_dist

    }

    # integer positions required for geom_raster
    offset_dist <- as.integer(offset_dist)

    vbl2D <- apply_offset(vbl2D, offset_dist = offset_dist, offset_dir = offset_dir)

    attr(vbl2D, which = "offset") <- TRUE
    attr(vbl2D, which = "offset_dist") <- offset_dist
    attr(vbl2D, which = "offset_dir") <- offset_dir

  }

  vbl2D <- update_var_smr(vbl2D, vars = c("col", "row", "slice"))

  class(vbl2D) <- c("vbl2D", class(vbl2D))

  return(vbl2D)

}
