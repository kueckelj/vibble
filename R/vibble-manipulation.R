# Verbs and manipulation helpers, all attribute-safe.

#' @importFrom rlang sym
add_var_head_mask <- function(vbl, var, var_out = "head", seed = 123, fn = max, verbose = vbl_opts("verbose")){

  stopifnot(is.numeric(vbl[[var]]))
  stopifnot(is.numeric(seed))

  # ID for subsetting later on
  id_exists <- "id" %in% colnames(vbl)
  if(!id_exists){ vbl <- id_add(vbl) }

  # kmeans cluster to separate based on intensity
  set.seed(seed)
  vbl[[var_out]] <- as.character(x = kmeans(x = vbl[[var]], centers = 2)$cluster)

  # foreground based on intensity only
  fg <-
    dplyr::group_by(vbl, !!sym(var_out)) %>%
    dplyr::summarise(avg = mean(!!sym(var), na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(avg == fn(avg)) %>%
    dplyr::pull(var = {{var_out}})

  plane <- names(plane_resolutions(vbl))[1]

  vbl2D <- vibble2D(vbl, plane = plane)
  vbl2D_fg <- vbl2D[vbl2D[[var_out]] == fg, ]

  # include everything 'inside' the foreground
  slice_iter <- slices(vbl2D_fg)

  pb <- set_up_progress_bar(length(slice_iter))

  vbl_fg_ids <-
    purrr::map(
      .x = slice_iter,
      .f = function(si){

        if(verbose){ pb$tick() }

        mask <- vbl2D_fg[vbl2D_fg$slice == si, ]

        if(nrow(mask) <= 3){

          out <- mask$id

        } else {

          # get mask poly
          poly <-
            concaveman::concaveman(
              points = as.matrix(mask[,c("col", "row")]),
              concavity = 1
            ) %>%
            as.data.frame() %>%
            magrittr::set_colnames(value = c("col", "row"))

          # identify every voxel of the complete vbl2D inside the mask poly
          out <-
            identify_voxels_in_poly(
              vbl2D = dplyr::filter(vbl2D, slice == {{si}}),
              poly = poly,
              strictly = TRUE,
              opt = "keep"
            )[["id"]]

        }

        return(unique(out, mask$id))

      }
    )

  vbl_fg_ids <- purrr::flatten_int(vbl_fg_ids)

  # overwrite var_out to logial mask
  vbl[[var_out]] <- vbl[["id"]] %in% vbl_fg_ids

  if(!id_exists){ vbl$id <- NULL }

  return(vbl)

}

clean_mask <- function(vbl, var, var_clean = var, min_size = 5000, verbose = TRUE, ...){

  stopifnot(is.logical(vbl[[var]]))

  before <- sum(vbl[[var]])

  dbscan_out <- dbscan3D(vbl, var = var, pref_out = NULL, min_size = min_size, ...)

  vbl[[var_clean]] <- dbscan_out$dbscan3D != "0"

  after <- sum(vbl[[var_clean]])

  perc <- (after/before)*100

  if(verbose){ message(glue::glue("{round(perc, 5)}% of voxels remain in '{var_clean}'."))}

  return(vbl)

}

#' @importFrom rlang sym

dbscan2D <- function(slice_df,
                     var = NULL,
                     var_out = "dbscan2D",
                     pref_out = var,
                     eps = 1.5,
                     minPts = 3,
                     min_size = NULL,
                     rm_outlier = FALSE,
                     ...){

  stopifnot(dplyr::n_distinct(slice_df$slice)==1)

  if(is.null(var)){

    var <- "pseudo."
    slice_df[[var]] <- TRUE

  }

  stopifnot(length(var)==1)
  stopifnot(var %in% colnames(slice_df))
  stopifnot(is.logical(slice_df[[var]]))

  vox_apply <- slice_df[[var]]

  set.seed(123)

  dbscan_out <-
    slice_df[vox_apply, c("col", "row")] %>%
    dbscan::dbscan(x = ., eps = eps, minPts  = minPts)

  slice_df[[var_out]] <- NA
  slice_df[[var_out]][vox_apply] <- dbscan_out$cluster

  if(is.numeric(min_size)){

    stopifnot(min_size > 0)

    if(min_size < 1){ min_size <- min_size * length(dbscan_out$cluster) }

    rm_size <-
      dplyr::group_by(slice_df, !!rlang::sym(var_out)) %>%
      dplyr::mutate(rm_size = dplyr::n() < {{min_size}}) %>%
      dplyr::pull("rm_size")

    slice_df[[var_out]][rm_size] <- 0

  }

  # remove outliers if desired and factorize
  if(isTRUE(rm_outlier)){

    slice_df <- dplyr::filter(slice_df, !!sym(var_out) != 0)

  }

  slice_df[[var_out]] <-
    factor(
      x = paste0(pref_out, slice_df[[var_out]]),
      levels = paste0(pref_out, sort(unique(dbscan_out$cluster)))
    ) %>%
    droplevels()

  return(slice_df)

}

dbscan3D <- function(vbl,
                     var,
                     var_out = "dbscan3D",
                     pref_out = var,
                     eps = 1.5,
                     minPts = 1,
                     min_size = NULL,
                     rm0 = FALSE){

  stopifnot(var %in% colnames(vbl))
  stopifnot(is.logical(vbl[[var]]))
  stopifnot(sum(vbl[[var]]) > 1 & sum(vbl[[var]]) < nrow(vbl))

  vox_apply <- vbl[[var]]

  dbscan_out <-
    vbl[vox_apply, vbl_ccs_axes] %>%
    dbscan::dbscan(x = ., eps = eps, minPts  = minPts)

  vbl[[var_out]] <- 0
  vbl[[var_out]][vox_apply] <- dbscan_out$cluster

  if(is.numeric(min_size)){

    if(min_size < 1){

      max_size <-
        purrr::map_dbl(.x = attr(vbl, "ccs_limits"), .f = max) %>%
        prod()

      min_size <- ceiling(max_size * min_size)

    }

    rm_size <-
      dplyr::group_by(vbl, !!rlang::sym(var_out)) %>%
      dplyr::mutate(rm_size = dplyr::n() < {{min_size}}) %>%
      dplyr::pull("rm_size")

    vbl[[var_out]][rm_size] <- 0

  }

  if(isTRUE(rm0)){

    vbl <- vbl[vox_apply & vbl[[var_out]] != 0, ]

  }

  vbl[[var_out]] <-
    factor(
      x = paste0(pref_out, vbl[[var_out]]),
      levels = paste0(pref_out, sort(unique(c(0, dbscan_out$cluster))))
    )

  return(vbl)

}


#' @title Filter a vbl2D by a 2D bounding box.
#'
#' @description
#' Filters a `vbl2D` object to rows whose `col` and `row` coordinates fall within
#' a given 2D bounding box.
#'
#' @param vbl2D A `vbl2D` object.
#' @param bb2D A valid 2D bounding box as checked by `is_bb2D()`.
#' @param null_ok Logical. Whether `NULL` limits in `bb2D` are treated as open
#' bounds.
#'
#' @return
#' A filtered `vbl2D` object.
#'
#' @seealso
#' is_vbl2D(),
#' is_bb2D(),
#' within_limits()
filter_bb2D <- function(vbl2D, bb2D, null_ok = TRUE){

  stopif(is_offset(vbl2D))

  .stop_if_not(is_vbl2D(vbl2D))
  .stop_if_not(is_bb2D(bb2D))

  dplyr::filter(
    .data = vbl2D,
    within_limits(col, l = bb2D$col, null_ok = null_ok) &
    within_limits(row, l = bb2D$row, null_ok = null_ok)
  )

}

#' @title Filter a vbl by a 3D bounding box.
#'
#' @description
#' Filters a `vbl` object to rows whose spatial coordinates fall within a given
#' 3D bounding box.
#'
#' @param vbl A `vbl` object.
#' @param bb3D A valid 3D bounding box as checked by `is_bb3D()`.
#' @param null_ok Logical. Whether `NULL` limits in `bb3D` are treated as open
#' bounds.
#'
#' @return
#' A filtered `vbl` object.
#'
#' @seealso
#' is_bb3D(),
#' within_limits()
filter_bb3D <- function(vbl, bb3D, null_ok = TRUE){

  .stop_if_not(is_bb3D(bb3D))

  dplyr::filter(
    .data = vbl,
    within_limits(x, l = bb3D$x, null_ok = null_ok) &
      within_limits(y, l = bb3D$y, null_ok = null_ok) &
      within_limits(z, l = bb3D$z, null_ok = null_ok)
  )

}


#' @export
filter_mask <- function(vbl, var){

  stopifnot(is.logical(vbl[[var]]))

  vbl <- vbl[vbl[[var]], ]
  vbl[[var]] <- NULL

  return(vbl)

}


impute_scores <- function(vbl, var_mask, var_score, mx_dst = Inf, verbose = TRUE){

  require(rlang)

  id_exists <- "id" %in% colnames(vbl)
  if(!id_exists){ vbl <- id_add(vbl) }

  bb <- bb3D(vbl, .cond = !!rlang::sym(var_mask), buffer = 0.01)

  bb_id <- filter_bb3D(vbl, bb = bb)[["id"]]

  # candidates from which to draw score
  vbl_data <- dplyr::filter(vbl, id %in% {{bb_id}} & !is.na(!!sym(var_score)))

  # mask voxels that require a score
  vbl_query <- dplyr::filter(vbl, !!sym(var_mask) & is.na(!!sym(var_score)))

  n_missing <- nrow(vbl_query)

  if(n_missing == 0){

    if(verbose){ message(glue::glue("No missing '{var_score}'-values  in mask '{var_mask}'.")) }

  } else {

    if(verbose){ message(glue::glue("{n_missing} voxels lack '{var_score}'-values."))}

    # add NN information to query
    vbl_query <-
      identify_nearest_voxel(vbl_query = vbl_query, vbl_data = vbl_data) %>%
      dplyr::filter(nn_dist < {{mx_dst}})

    if(nrow(vbl_query) == 0){

      if(verbose){ message(glue::glue("No voxels with neighbors at distance below {round(mx_dst, 5)}.")) }

    } else {

      score_data <- vbl_data[[var_score]]
      names(score_data) <- vbl_data$id

      vbl_query[[var_score]] <- unname(score_data[vbl_query$nn_id])

      idx <- match(vbl_query$id, vbl$id)
      vbl[idx, var_score] <- unname(vbl_query[[var_score]])

      if(verbose){

        perc <- (nrow(vbl_query)/n_missing)*100
        message(glue::glue("Imputed scores for {round(perc, 5)}% of missing mask voxels."))

      }

    }

  }

  if(!id_exists){ vbl$id <- NULL }

  return(vbl)

}



#' @title Join two vibbles by spatial coordinates
#' @description
#' Joins two `vbl` objects of the same reference space by their Cartesian coordinate
#' columns (`x`, `y`, `z`).
#'
#' @param a,b `vbl` objects to be joined. Their `ccs_limits` must match exactly.
#' @param .rfn Optional renaming function or formula applied to all non-coordinate
#' variables of `b` before joining (e.g., `~ paste0(.x, "_b")`).
#' @param join Type of join to perform. One of `"full"`, `"inner"`, `"left"`,
#' or `"right"`. Determines how voxel coordinates absent in one input are treated.
#' @param ... Reserved for future extensions.
#'
#' @details
#' The join is performed strictly on the three spatial coordinate columns
#' (`x`, `y`, `z`). Before joining, overlapping variable names between `a` and `b`
#' (excluding the coordinate columns) are prohibited unless `.rfn` is used to
#' disambiguate them. This prevents silent overwriting of voxelwise variables.
#'
#' Matching `ccs_limits` between the two vibbles is required to ensure that both
#' objects reside in the same voxel grid and share a valid spatial reference
#' system. If the limits differ, the function aborts with an informative error.
#'
#' @return
#' A tibble containing all coordinate columns and the combined voxelwise
#' variables from both inputs, according to the selected join strategy.
#'
#' @export

join_vibbles <- function(a, b, .rfn = NULL, join = "full", ...){

  require(dplyr)

  join <- match.arg(join, choices = c("full", "inner", "left", "right"))

  test_ccs_limits(
    lim_a = ccs_limits(a),
    lim_b = ccs_limits(b),
    fdb = TRUE
  )

  a <- dplyr::select(a, {{vbl_ccs_axes}}, dplyr::everything())
  b <- dplyr::select(b, {{vbl_ccs_axes}}, dplyr::everything())

  if(is.function(.rfn) | purrr::is_formula(.rfn)){

    b <- dplyr::rename_with(b, .cols = -dplyr::all_of(vbl_ccs_axes), .fn = .rfn)

  }

  ovlp_vars <- intersect(colnames(a[,-c(1:3)]), colnames(b[,-c(1:3)]))

  if(length(ovlp_vars) != 0){

    ref <- stringr::str_c(ovlp_vars, collapse = ", ")
    stop(glue::glue("Overlapping variable names: {ref}"))

  }

  # join columns
  if(join == "full"){

    out <- dplyr::full_join(x = a, y = b, by = vbl_ccs_axes)

  } else if(join == "inner"){

    out <- dplyr::inner_join(x = a, y = b, by = vbl_ccs_axes)

  } else if(join == "left"){

    out <- dplyr::left_join(x = a, y = b, by = vbl_ccs_axes)

  } else if(join == "right"){

    out <- dplyr::right_join(x = a, y = b, by = vbl_ccs_axes)

  }

  return(out)

}
