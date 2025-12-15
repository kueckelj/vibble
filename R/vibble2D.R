#' @title Create a 2D vibble from 3D voxel data
#' @description Convert a 3D `vbl` object into a 2D representation for a given
#' anatomical plane and selected slices. Optionally apply filtering conditions
#' and spatial offsets for visualization.
#'
#' @param slices Optional. Integer vector of slice positions in the selected `plane`
#' to keep. If `NULL`, all slices are retained.
#' @param crop Defines the \link[=vbl_doc_ref_bb]{data bounding box}
#' applied to all slices in native (pre-offset) coordinates.
#'
#' \itemize{
#'   \item{`NULL`: }{ Defaults to the global data extent.}
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
                     crop = NULL,
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

  if(!is.numeric(slices)){

    slices <- unique(vbl[[slice_axis]])

  } else {

    slices_missing <- slices[!slices %in% unique(vbl[[slice_axis]])]
    n_missing <- length(slices_missing)

    if(n_missing != 0){

      slice_ref <- ifelse(n_missing == 1, "slice", "slices")
      slices_missing <- stringr::str_c(slices_missing, collapse = "', '")
      .glue_warning("No data for {slice_ref} '{slices_missing}' in plane {plane}. Ignoring.")

    }

  }

  # change class name HERE to allow x,y,z manipulation/renaming
  class(vbl) <- stringr::str_replace(class(vbl), "vbl", "vbl2D")

  # 3D to 2D
  vbl2D <-
    dplyr::rename(vbl, !!!req_axes) %>%
    dplyr::filter(slice %in% {{slices}}) %>%
    dplyr::select(col, row, slice, dplyr::everything())

  offset_col(vbl2D) <- 0L
  offset_row(vbl2D) <- 0L

  plane(vbl2D) <- plane

  # apply condition
  .by_quo <- rlang::enquo(.by)
  .cond_quo <- rlang::enquo(.cond)
  if(!rlang::quo_is_null(.cond_quo)){

    vbl2D <- dplyr::filter(vbl2D, !!.cond_quo, .by = {{ .by_quo }})

  }

  # apply crop
  if(is_limit(crop)){

    crop <- list(col = crop, row = crop)

  } else if(is_bb2D(crop)){

    # pass, already valid input

  } else {

    crop <- list(col = range(vbl2D$col), row = range(vbl2D$row))

  }

  crop <- expand_bb2D(bb2D = crop, expand = vbl_opts("expand.refbb"))

  # crop
  vbl2D <-
    dplyr::filter(
      .data = vbl2D,
      within_limits(col, l = crop$col) &
      within_limits(row, l = crop$row)
    )

  data_bb(vbl2D) <- crop

  # set screen
  screen_bb(vbl2D) <- expand_bb2D(bb2D = crop, expand = expand)

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
