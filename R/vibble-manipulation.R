# Verbs and manipulation helpers, all attribute-safe.

#' @importFrom rlang sym
add_var_head_mask <- function(vbl, var, var_out = "head", seed = 123, verbose = TRUE){

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
    dplyr::filter(avg == max(avg)) %>%
    dplyr::pull(var = {{var_out}})

  vbl2D_fg <- vibble2D(vbl[vbl[[var_out]] == fg, ], plane = "axi")

  # include everything 'inside' the foreground
  slice_iter <- sort(unique(vbl2D_fg$slice)) # slice seq

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

          poly <-
            concaveman::concaveman(
              points = as.matrix(mask[,c("col", "row")]),
              concavity = 1
            ) %>%
            as.data.frame() %>%
            magrittr::set_colnames(value = c("col", "row"))

          out <-
            identify_voxels_in_poly(
              vbl2D = vibble2D(vbl, plane = "axi", slices = si),
              poly = poly,
              strictly = TRUE,
              opt = "keep"
            )[["id"]]

        }

        return(unique(out, mask$id))

      }
    )

  vbl_fg_ids <- purrr::flatten_chr(vbl_fg_ids)

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

dbscan2D <- function(slice_df,
                     var,
                     var_out = "dbscan2D",
                     pref_out = var,
                     eps = 1.5,
                     minPts = 3,
                     min_size = NULL,
                     rm0 = FALSE,
                     ...){

  stopifnot(dplyr::n_distinct(slice_df$slice)==1)

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

    if(min_size < 1){

      ccs_limits <- attr(slice_df, "ccs_limits")
      ccs_axes <- req_axes_2d(attr(slice_df, "plane"))

      max_size <-
        purrr::map_dbl(ccs_limits[ccs_axes], .f = max) %>%
        prod()

      min_size <- ceiling(max_size * min_size)

    }

    rm_size <-
      dplyr::group_by(slice_df, !!rlang::sym(var_out)) %>%
      dplyr::mutate(rm_size = dplyr::n() < {{min_size}}) %>%
      dplyr::pull("rm_size")

    slice_df[[var_out]][rm_size] <- 0

  }

  if(isTRUE(rm0)){

    slice_df <- slice_df[vox_apply & slice_df[[var_out]] != 0, ]

  }

  slice_df[[var_out]] <-
    factor(
      x = paste0(pref_out, slice_df[[var_out]]),
      levels = paste0(pref_out, sort(unique(dbscan_out$cluster)))
    )

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


#' @title Filter vibble by 3D bounding box
#' @description Filter voxels that lie inside a 3D bounding box.
#' Returns a subset of `vbl` restricted to the specified coordinate limits.
#'
#' @inheritParams vbl_doc
#'
#' @return A `vibble` (or tibble) containing only rows whose spatial coordinates fall within the limits given by `bb3D`.
#'
#' @details The function checks that `bb3D` is a valid 3D bounding box via \link{is_bb3D}().
#' It then filters rows where all three coordinates lie within the specified limits using \link{within_limits}`(..., null_ok = TRUE)`.
#'
#' @note The \link{ccs_limits} remain untouched. While the output vibble contains
#' only voxels within the bounding box they still remain in the same space.
#'
#' @seealso \link{is_bb3D}(), \link{ccs_limits}()
#'
#' @importFrom dplyr filter
#'
#' @examples
#' # Example 1: Filter to a manually defined bounding box
#' vbl <- example_vbl()
#' bb3D <- list(
#'   x = c(10, 50),
#'   y = c(20, 60),
#'   z = c( 5, 40)
#' )
#' vbl_sub <- filter_bb3D(vbl, bb3D)
#'
#' # Example 2: Use the bounding box of a mask region
#' vbl <- example_vbl()
#' bb_tumor <- bb3D(vbl, var = "tumor")
#' vbl_tumor_area <- filter_bb3D(vbl, bb_tumor)
#'
#' @export
filter_bb3D <- function(vbl, bb3D){

  stopifnot(is_bb3D(bb3D))

  dplyr::filter(
    .data = vbl,
      within_limits(x, bb3D$x) &
      within_limits(y, bb3D$y) &
      within_limits(z, bb3D$z)
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

  bb <- bb3D(vbl, var = var_mask, buffer = 0.01)

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
