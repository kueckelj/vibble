# Verbs and manipulation helpers, all attribute-safe.

#' @importFrom rlang sym
add_var_head_mask <- function(vbl, var, var_out = "head", seed = 123, verbose = TRUE){

  stopifnot(is.numeric(vbl[[var]]))
  stopifnot(is.numeric(seed))

  # ID for subsetting later on
  id_exists <- "id" %in% colnames(vbl)
  if(!id_exists){ vbl <- id_add(vbl, arrange = FALSE) }

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
  vbl <- update_var_smr(vbl, vars = var_out)

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
    vbl[vox_apply, ccs_labels] %>%
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



#' @title Join vibbles
#' @description
#' Join two vibble objects by their voxel coordinates, optionally renaming
#' variables in the second input and creating it from file if needed.
#'
#' @details
#' The function checks that both inputs share the same spatial space, coordinate
#' limits, and coordinate mapping. If `b` is a character path, it is converted
#' into a vibble via `make_vibble()`. An optional renaming function `.rfn` is
#' applied to all non-coordinate variables of `b` before the join, if specified.
#' Variable name collisions are not allowed. The final merge is performed using a full, inner,
#' left, or right join on `ccs_labels` where `x` of the input for `<type>_join()`
#' is the input for `a` and `y` of the join corresponds to resulting vibble from
#' input for `b`.
#'
#' @param a A vibble data.frame.
#' @param b A vibble data.frame or a character path to a NIfTI file or folder.
#' @param .rfn Optional renaming function or formula applied to non-coordinate
#' variables of `b`.
#' @param join Type of join to perform: `"full"`, `"inner"`, `"left"`, `"right"`.
#' @param ... Additional arguments passed to `make_vibble()` when `b` is a
#' character path.
#'
#' @return A vibble.
#'
#' @examples
#' \dontrun{
#' join_vibbles(a = vibble_a, b = vibble_b)
#' join_vibbles(a, "path/to/folder", space = "orig")
#' join_vibbles(a, b, .rfn = ~ paste0("b_", .x))
#' }
#'
#' @export

join_vibbles <- function(a, b, .rfn = NULL, join = "full", ...){

  require(dplyr)

  join <- match.arg(join, choices = c("full", "inner", "left", "right"))

  space_a <- space(a)

  test_ccs_limits(
    lim_a = ccs_limits(a),
    lim_b = ccs_limits(b),
    fdb = TRUE
  )

  test_ccs_mapping(
    map_a = ccs_mapping(a),
    map_b = ccs_mapping(b),
    fdb = TRUE
  )

  a <- dplyr::select(a, {{ccs_labels}}, dplyr::everything())
  b <- dplyr::select(b, {{ccs_labels}}, dplyr::everything())

  if(is.function(.rfn) | purrr::is_formula(.rfn)){

    b <- dplyr::rename_with(b, .cols = -dplyr::all_of(ccs_labels), .fn = .rfn)

  }

  ovlp_vars <- intersect(colnames(a[,-c(1:3)]), colnames(b[,-c(1:3)]))

  if(length(ovlp_vars) != 0){

    ref <- stringr::str_c(ovlp_vars, collapse = ", ")
    stop(glue::glue("Overlapping variable names: {ref}"))

  }

  # join columns
  if(join == "full"){

    out <- dplyr::full_join(x = a, y = b, by = ccs_labels)

  } else if(join == "inner"){

    out <- dplyr::inner_join(x = a, y = b, by = ccs_labels)

  } else if(join == "left"){

    out <- dplyr::left_join(x = a, y = b, by = ccs_labels)

  } else if(join == "right"){

    out <- dplyr::right_join(x = a, y = b, by = ccs_labels)

  }

  # no NAs in logical variables
  out <-
    mutate(
      .data = out,
      across(
        .cols = where(is.logical),
        .fns = ~ tidyr::replace_na(.x, replace = FALSE))
    )

  # join meta data
  attr(out, which = "var_smr") <- c(var_smr(a), var_smr(b))

  return(out)

}
