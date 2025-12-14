#' @title Create a 2D vibble from 3D voxel data
#' @description Convert a 3D `vbl` object into a 2D representation for a given
#' anatomical plane and selected slices. Optionally apply filtering conditions
#' and spatial offsets for visualization.
#'
#' @param slices Optional. Integer vector of slice positions in the selected `plane`
#' to keep. If `NULL`, all slices are retained.
#' @param screen_bb `bb2D`, `limit`, or `NULL`. Bounding box defining the common
#' \link[=vbl_doc_limits_2D]{screen window} applied to all slices in native (pre-offset) coordinates.
#'
#' \itemize{
#'   \item{`NULL`: }{ Screen window defaults to the global data extent.}
#'   \item{`limit`: }{ A single spatial \link[=is_limit]{limit} recycled for both `col` and `row`.}
#'   \item{`bb2D`: }{ A \link[=is_bb2D]{2D bounding box} with separate limits for `col` and `row`.}
#' }
#'
#' When the supplied bounding box is smaller than the slice extents, only voxels
#' inside this region are retained, giving `screen_bb` a filtering effect.
#'
#' @param expand An \link[=is_expand]{expand} specification applied to the col/row limits
#' of `screen_bb` **after** interpretation and potential filtering.
#' @param .cond An expression evaluated with \link[rlang:args_data_masking]{data-masking}
#' to filter voxels. If `NULL`, all voxels are retained.
#' @param .by A \link[dplyr:dplyr_tidy_select]{tidy-selection} of columns to
#' group by before applying the filtering logic of `.cond`.
#'
#' @inherit vbl_doc params
#'
#' @return A \link[=vbl_doc_vbl2D]{2D vibble}.
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
                     screen_bb = NULL,
                     expand = FALSE,
                     offset_col = 0,
                     offset_row = 0,
                     zstack = "asc",
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

  data_bb(vbl2D) <- list(col = range(vbl2D$col), row = range(vbl2D$row))

  offset_col(vbl2D) <- 0L
  offset_row(vbl2D) <- 0L

  plane(vbl2D) <- plane

  # apply condition
  .by_quo <- rlang::enquo(.by)
  .cond_quo <- rlang::enquo(.cond)
  if(!rlang::quo_is_null(.cond_quo)){

    vbl2D <- dplyr::filter(vbl2D, !!.cond_quo, .by = {{ .by_quo }})

  }

  # apply screen_bb
  if(is_limit(screen_bb)){

    screen_bb <- list(col = screen_bb, row = screen_bb)

  } else if(is_bb2D(screen_bb)){

    # pass, already valid input

  } else if(is.null(screen_bb)) {

    screen_bb <- data_bb(vbl2D)

  } else {

    stop("Invalid input for `screen_bb`.")

  }

  # apply (potential) filtering, then set
  vbl2D <-
    dplyr::filter(
      .data = vbl2D,
      within_limits(col, l = screen_bb$col) &
      within_limits(row, l = screen_bb$row)
    )

  screen_bb(vbl2D) <- expand_bb2D(bb2D = screen_bb, expand = expand)

  # apply offset
  vbl2D <- apply_offset(vbl2D, offset_col = offset_col, offset_row = offset_row)

  # manage z-stack
  if(is.character(zstack)){

    zstack <- match.arg(zstack, choices = c("asc", "desc"))

    if(zstack == "asc"){

      vbl2D <- dplyr::arrange(vbl2D, slice)

    } else {

      vbl2D <- dplyr::arrange(vbl2D, dplyr::desc(slice))

    }

  }

  return(vbl2D)

}
