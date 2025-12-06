#' @title Add a color layer for categorical variables
#' @description Overlay a categorical label variable on a `ggplane()` plot by
#' filling voxels with discrete colors.
#'
#' @param var Character. The name of a factor-variable with categorical labels.
#' If a logical (mask) variable is specified, it is treated as a categorical one
#' with labels = c('TRUE', 'FALSE').
#' @param labels Optional character vector of label values to keep. If supplied,
#' only voxels whose label is in `labels` are rendered.
#' @param labels_rm Optional character vector of label values to remove. If
#' supplied, voxels whose label is in `labels_rm` are excluded.
#' @param ... Additional arguments passed to \link{scale_fill_manual}().
#'
#' @inherit vbl_doc_layer params return
#'
#' @inheritParams vbl_doc_var_label
#' @inheritParams vbl_doc
#' @export
layer_categorical <- function(var,
                              clrp = "default",
                              clrp_adjust = NULL,
                              opacity = 0.45,
                              .cond = NULL,
                              .by = NULL,
                              ...){

  opacity_quo <- rlang::enquo(opacity)
  .cond_quo <- rlang::enquo(.cond)

  vbl_layer(
    fun = function(vbl2D){

      layer <- glue::glue("layer_categorical(var = '{var}', ...")
      vbl2D <- .filter_layer(vbl2D, .cond = .cond_quo, .by = .by, layer = layer)

      .layer_categorical_impl(
        vbl2D = vbl2D,
        var = var,
        clrp = clrp,
        clrp_adjust = clrp_adjust,
        opacity = opacity_quo,
        ...
      )

    }
  )

}


#' @keywords internal
.layer_categorical_impl <- function(vbl2D,
                                    var,
                                    clrp = "default",
                                    clrp_adjust = NULL,
                                    opacity = 0.45,
                                    ...){

  if(is_mask_var(vbl2D[[var]])){

    vbl2D[[var]] <- factor(as.character(vbl2D[[var]]), levels = c("TRUE", "FALSE"))

  }

  is_vartype(vbl2D, var = var, type = "label")

  vbl2D <- vbl2D[!is.na(vbl2D[[var]]), ]

  if(is_offset(vbl2D)){ vbl2D <- .remove_overlap(vbl2D) }

  if(is.character(vbl2D[[var]])){ vbl2D[[var]] <- as.factor(vbl2D[[var]]) }

  cvec <- color_vector(clrp, names = levels(vbl2D[[var]]), clrp_adjust = clrp_adjust)

  alpha_use <- .eval_tidy_opacity(vbl2D, opacity = opacity, var = var)
  color_use <- alpha(cvec[vbl2D[[var]]], alpha_use)

  list(
    ggnewscale::new_scale_fill(),
    ggplot2::geom_tile(
      data = vbl2D,
      mapping = ggplot2::aes(x = col, y = row, fill = .data[[var]]),
      alpha = alpha_use,
      color = color_use
    ),
    scale_fill_categorical(
      clrp = clrp,
      clrp_adjust = clrp_adjust,
      names = levels(vbl2D[[var]]),
      ...
    )
  )

}

#' @title Add a color layer for mask
#' @description Overlay a logical mask on a \link{ggplane}() plot by filling
#' voxels and/or drawing polygon outlines.
#'
#' @param color Color used for the mask fill and outline.
#' @param fill Logical. If `TRUE`, draw a filled raster layer over mask voxels.
#' @param outline Logical. If `TRUE`, draw polygon outlines around connected
#' mask regions.
#' @param concavity Numeric passed to \link{concaveman}() to control the shape
#' of the concave hull used for outlines.
#' @param nv Integer. Number of vertices used by `densify_poly()` to densify each
#' polygon outline.
#' @param var Character or `NULL`. If character, specifies a logical variables
#' in the vibble based on which the masks are created.
#' @param ... Additional arguments passed to \link{geom_polygon}() for the outline.
#'
#' @note
#' If neither `var` nor `.cond` is specified, the masks mask every voxel in every slice
#' of the 2D vibble passed to this layer by `ggplane()`.
#'
#' @inherit vbl_doc_layer params return
#' @inheritParams vbl_doc_var_mask
#' @inheritParams vbl_doc
#' @inheritSection vbl_doc Opacity options
#'
#' @export
layer_mask <- function(color,
                       opacity = 0.25,
                       fill = TRUE,
                       outline = FALSE,
                       concavity = 2.5,
                       nv = 50,
                       var = NULL,
                       .cond = NULL,
                       .by = NULL,
                       ...){

  opacity_quo <- rlang::enquo(opacity)
  .cond_quo <- rlang::enquo(.cond)

  vbl_layer(
    fun = function(vbl2D){

      layer <- glue::glue("layer_mask(color = '{color}', ...)")
      vbl2D <- .filter_layer(vbl2D, .cond = .cond_quo, .by = .by, layer = layer)

      .layer_mask_impl(
        vbl2D = vbl2D,
        var = var,
        color = color,
        opacity = opacity_quo,
        fill = fill,
        outline = outline,
        concavity = concavity,
        nv = nv,
        ...
      )

    }
  )

}

#' @keywords internal
.layer_mask_impl <- function(vbl2D,
                             var,
                             color,
                             opacity = 0.45,
                             fill = TRUE,
                             outline = TRUE,
                             concavity = 2.5,
                             nv = 50,
                             ...){

  if(is.null(var)){ vbl2D[["pseudo."]] <- TRUE; var <- "pseudo." }

  is_vartype(vbl2D, var = var, type = "mask")

  if(is_offset(vbl2D)){ vbl2D <- .remove_overlap(vbl2D) }

  data <- vbl2D[vbl2D[[var]], ]

  layer_lst <- list()

  if(isTRUE(fill)){

    layer_lst$fill <-
      ggplot2::geom_tile(
        data = data,
        mapping = ggplot2::aes(x = col, y = row),
        alpha = .eval_tidy_opacity(data, opacity = opacity, var = var),
        color = color,
        fill = color
      )

  }

  if(isTRUE(outline)){

    outline_df <-
      dplyr::group_by(data, slice) %>%
      dplyr::group_split() %>%
      purrr::map_dfr(.f = ~ dbscan2D(.x, var = var, rm0 = TRUE)) %>%
      dplyr::group_by(slice, !!rlang::sym(var)) %>%
      dplyr::group_split() %>%
      purrr::imap_dfr(.f = function(df, idx){

        concaveman::concaveman(
          points = as.matrix(df[,c("col", "row")]),
          concavity = concavity
        ) %>%
          tibble::as_tibble() %>%
          magrittr::set_colnames(value = c("col", "row")) %>%
          .densify_poly(n = nv) %>%
          dplyr::mutate(outline_idx = idx, slice = unique(df$slice))

      })

    layer_lst$outline <-
      ggplot2::geom_polygon(
        data = outline_df,
        mapping = ggplot2::aes(x = col, y = row, group = outline_idx),
        color = color,
        fill = NA,
        ...
      )

  }

  return(layer_lst)

}



#' @title Add a color layer for numeric variables
#' @description Overlay a numeric variable on a \link{ggplane}() plot by mapping its
#' values to a continuous fill scale.
#'
#' @param ... Additional arguments passed to \link{scale_fill_numeric}().
#'
#' @inherit vbl_doc_layer params return
#' @inheritParams vbl_doc_var_numeric
#' @inheritParams vbl_doc
#' @inheritSection vbl_doc Opacity options
#' @export
layer_numeric <- function(var,
                          clrsp,
                          opacity = c(0.2, 0.45),
                          interpolate = vbl_opts("interpolate"),
                          .cond = NULL,
                          .by = NULL,
                          ...){

  .by_quo <- rlang::enquo(.by)
  .cond_quo <- rlang::enquo(.cond)
  opacity_quo <- rlang::enquo(opacity)

  vbl_layer(
    fun = function(vbl2D){

      layer <- glue::glue("layer_numeric(var = '{var}', ...)")

      vbl2D <- .filter_layer(vbl2D, .cond = .cond_quo, .by = .by_quo, layer = layer)

      .layer_numeric_impl(
        vbl2D = vbl2D,
        var = var,
        clrsp = clrsp,
        opacity = opacity_quo,
        interpolate = interpolate,
        ...
      )

    }
  )

}

#' @keywords internal
.layer_numeric_impl <- function(vbl2D,
                                var,
                                clrsp,
                                opacity = c(0.2, 0.45),
                                interpolate = vbl_opts("interpolate"),
                                ...){

  is_vartype(vbl2D, var = var, type = "numeric")

  if(is_offset(vbl2D)){ vbl2D <- .remove_overlap(vbl2D) }

  list(
    ggnewscale::new_scale_fill(),
    ggplot2::geom_tile(
      data = vbl2D,
      mapping = ggplot2::aes(x = col, y = row, fill = .data[[var]]),
      alpha = .eval_tidy_opacity(vbl2D, opacity = opacity, var = var)
    ),
    scale_fill_numeric(
      clrsp,
      limits = var_limits(vbl2D, var),
      ...
    )
  )

}




layer_outline <- function(color,
                          alpha = 0.9,
                          linetype = "solid",
                          linewidth = 0.5,
                          use_dbscan = TRUE,
                          concavity = 2.5,
                          nv = 50,
                          .cond = NULL,
                          .by = NULL,
                          ...){

  .cond_quo <- rlang::enquo(.cond)

  vbl_layer(
    fun = function(vbl2D){

      outlines_slice <- NULL
      if(is_offset(vbl2D)){

        outlines_slice <-
          .comp_outlines(
            vbl2D = vbl2D,
            var = NULL,
            concavity = concavity,
            use_dbscan = FALSE
            )

      }

      layer <- glue::glue("layer_outline(color = '{color}', ...)")
      vbl2D <- .filter_layer(vbl2D, .cond = .cond_quo, .by = .by, layer = layer)

      .layer_outline_impl(
        vbl2D = vbl2D,
        alpha = alpha,
        color = color,
        linetype = linetype,
        linewidth = linewidth,
        use_dbscan = use_dbscan,
        concavity = concavity,
        outlines_slice = outlines_slice,
        ...
      )

    }
  )

}

#' @keywords internal
.layer_outline_impl <- function(vbl2D,
                                color,
                                alpha,
                                linetype,
                                linewidth,
                                concavity,
                                use_dbscan,
                                outlines_slice,
                                ...){


  # create raw outlines on the input vbl2D
  # (which was filtered by .cond/.by in layer_outline())
  outlines <-
    .comp_outlines(
      vbl2D = vbl2D,
      var = NULL,
      concavity = concavity,
      use_dbscan = use_dbscan,
      ...)

  # handle offset overlaps
  if(is_offset(vbl2D)){

    slices_main <- unique(vbl2D$slice)
    slices_lead <- dplyr::lead(slices_main)

    outlines <-
      purrr::map_df(
        .x = seq_along(slices_main),
        .f = function(i){

          sm <- slices_main[i]
          sl <- slices_lead[i]

          om <- dplyr::filter(outlines, slice == {{sm}})

          # BREAK, if no leading slice for the last main slice
          if(i == length(slices_main)){ return(om) }

          # BREAK, if no outline available
          if(nrow(om) == 0){ return(NULL) }

          purrr::map_df(
            .x = unique(om$outline),
            .f = function(outline_use){

              # split the outline according to the slice outline of the next slice
              .split_outline(
                outline = dplyr::filter(om, outline == {{outline_use}}),
                outline_ref = dplyr::filter(outlines_slice, slice == {{sl}})
              ) %>%
                dplyr::filter(pos_rel == "outside")

            }
          )

        }
      )

    outlines$split <- tidyr::replace_na(outlines$split, FALSE)
    outlines$part <- tidyr::replace_na(outlines$part, 1)

  } else {

    outlines$part <- 1
    outlines$split <- FALSE

  }

  assign("outlines", outlines, envir = .GlobalEnv)

  # output
  layer_lst <- list()

  if(any(outlines$split)){

    data <-
      dplyr::filter(outlines, split) %>%
      dplyr::mutate(outline = stringr::str_c(outline, slice, part, sep = "."))

    layer_lst$path <-
      ggplot2::geom_path(
        data = data,
        mapping = ggplot2::aes(x = col, y = row, group = outline),
        alpha = alpha,
        color = color,
        linetype = linetype,
        linewidth = linewidth
      )

  }

  if(any(!outlines$split)){

    data <-
      dplyr::filter(outlines, !split) %>%
      dplyr::mutate(outline = stringr::str_c(outline, slice, part, sep = "."))

    layer_lst$polygon <-
      ggplot2::geom_polygon(
        data = data,
        mapping = ggplot2::aes(x = col, y = row, group = outline),
        color = ggplot2::alpha(color, alpha),
        linetype = linetype,
        linewidth = linewidth,
        fill = NA,
        ...
      )

  }

  return(layer_lst)

}



