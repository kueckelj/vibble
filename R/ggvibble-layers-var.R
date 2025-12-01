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
                              labels = NULL,
                              labels_rm = NULL,
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
        labels = labels,
        labels_rm = labels_rm,
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
                                    labels = NULL,
                                    labels_rm = NULL,
                                    clrp = "default",
                                    clrp_adjust = NULL,
                                    opacity = 0.45,
                                    ...){

  if(is_mask_var(vbl2D[[var]])){

    vbl2D[[var]] <- factor(as.character(vbl2D[[var]]), levels = c("TRUE", "FALSE"))

  }

  is_vartype(vbl2D, var = var, type = "label")

  vbl2D <- vbl2D[!is.na(vbl2D[[var]]), ]

  if(is.character(labels)){ vbl2D <- vbl2D[vbl2D[[var]] %in% labels, ] }

  if(is.character(labels_rm)){ vbl2D <- vbl2D[!vbl2D[[var]] %in% labels, ] }

  if(is.character(vbl2D[[var]])){ vbl2D[[var]] <- as.factor(vbl2D[[var]]) }

  list(
    ggnewscale::new_scale_fill(),
    ggplot2::geom_raster(
      data = vbl2D,
      mapping = ggplot2::aes(x = col, y = row, fill = .data[[var]]),
      alpha = .eval_tidy_opacity(vbl2D, opacity = opacity, var = var)
    ),
    scale_fill_categorical(
      clrp = clrp,
      clrp_adjust = clrp_adjust,
      names = levels(vbl2D[[var]])
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

      layer <- glue::glue("layer_mask(var = '{var}', ...")
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

  if(is.null(var)){ vbl2D$pseudo <- TRUE; var <- "pseudo" }

  is_vartype(vbl2D, var = var, type = "mask")

  data <- vbl2D[vbl2D[[var]], ]

  layer_lst <- list()

  if(isTRUE(fill)){

    layer_lst$fill <-
      ggplot2::geom_raster(
        mapping = ggplot2::aes(x = col, y = row),
        data = data,
        alpha = .eval_tidy_opacity(data, opacity = opacity, var = var),
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

  .cond_quo <- rlang::enquo(.cond)
  opacity_quo <- rlang::enquo(opacity)

  vbl_layer(
    fun = function(vbl2D){

      layer <- glue::glue("layer_numeric(var = '{var}', ...)")
      vbl2D <- .filter_layer(vbl2D, .cond = .cond_quo, .by = .by, layer = layer)

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

  list(
    ggnewscale::new_scale_fill(),
    ggplot2::geom_raster(
      data = vbl2D,
      mapping = ggplot2::aes(x = col, y = row, fill = .data[[var]]),
      alpha = .eval_tidy_opacity(vbl2D, opacity = opacity, var = var),
      interpolate = interpolate
    ),
    scale_fill_numeric(
      clrsp,
      limits = var_smr(vbl, var)$limits,
      ...
    )
  )

}
