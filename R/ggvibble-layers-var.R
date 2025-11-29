#' @title Add a color layer for label variables
#' @description Overlay a categorical label variable on a `ggplane()` plot by
#' filling voxels with discrete colors. The layer is designed for variables of
#' vartype `"label"` and can be restricted to selected labels, slices, or
#' conditions.
#'
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

layer_label_var <- function(var,
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

      layer <- glue::glue("layer_label_var(var = '{var}', ...")
      vbl2D <- .filter_layer(vbl2D, .cond = .cond_quo, .by = .by, layer = layer)

      .layer_label_var_impl(
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
.layer_label_var_impl <- function(vbl2D,
                                  var,
                                  labels = NULL,
                                  labels_rm = NULL,
                                  clrp = "default",
                                  clrp_adjust = NULL,
                                  opacity = 0.45,
                                  ...){

  is_vartype(vbl2D, var = var, type = "label")

  vbl2D <- vbl2D[!is.na(vbl2D[[var]]), ]

  if(is.character(labels)){ vbl2D <- vbl2D[vbl2D[[var]] %in% labels, ] }

  if(is.character(labels_rm)){ vbl2D <- vbl2D[!vbl2D[[var]] %in% labels, ] }

  if(is.character(vbl2D[[var]])){ vbl2D[[var]] <- as.factor(vbl2D[[var]]) }

  color_mapping <-
    confuns::color_vector(
      clrp = clrp,
      names = levels(vbl2D[[var]]),
      clrp.adjust = clrp_adjust
    )

  list(
    ggnewscale::new_scale_fill(),
    ggplot2::geom_raster(
      data = vbl2D,
      mapping = ggplot2::aes(x = col, y = row, fill = .data[[var]]),
      opacity = .eval_tidy_opacity(vbl2D, opacity = opacity, var = var)
    ),
    ggplot2::scale_fill_manual(values = color_mapping, ...)
  )

}



#' @title Add a color layer for mask variables
#' @description Overlay a logical mask variable on a `ggplane()` plot by filling
#' voxels and/or drawing polygon outlines around mask regions.
#'
#' @param color Color used for the mask fill and outline.
#' @param fill Logical. If `TRUE`, draw a filled raster layer over mask voxels.
#' @param outline Logical. If `TRUE`, draw polygon outlines around connected
#' mask regions.
#' @param concavity Numeric passed to \link{concaveman}() to control the shape
#' of the concave hull used for outlines.
#' @param nv Integer. Number of vertices used by `densify_poly()` to densify each
#' polygon outline.
#' @param ... Additional arguments passed to \link{geom_polygon}() for the outline.
#'
#' @inherit vbl_doc_layer params return
#' @inheritParams vbl_doc_var_mask
#' @inheritParams vbl_doc
#' @inheritSection vbl_doc Opacity options
#'
#' @export
layer_mask_var <- function(var,
                           color,
                           opacity = 0.25,
                           fill = TRUE,
                           outline = FALSE,
                           concavity = 2.5,
                           nv = 50,
                           .cond = NULL,
                           .by = NULL,
                           ...){

  opacity_quo <- rlang::enquo(opacity)
  .cond_quo <- rlang::enquo(.cond)

  vbl_layer(
    fun = function(vbl2D){

      layer <- glue::glue("layer_mask_var(var = '{var}', ...")
      vbl2D <- .filter_layer(vbl2D, .cond = .cond_quo, .by = .by, layer = layer)

      .layer_mask_var_impl(
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
.layer_mask_var_impl <- function(vbl2D,
                                 var,
                                 color,
                                 opacity = 0.45,
                                 fill = TRUE,
                                 outline = TRUE,
                                 concavity = 2.5,
                                 nv = 50,
                                 ...){

  is_vartype(vbl2D, var = var, type = "mask")

  data <- vbl2D[vbl2D[[var]], ]

  layer_lst <- list()

  if(isTRUE(fill)){

    layer_lst$fill <-
      ggplot2::geom_raster(
        mapping = ggplot2::aes(x = col, y = row),
        data = data,
        opacity = .eval_tidy_opacity(data, opacity = opacity, var = var),
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
layer_numeric_var <- function(var,
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

      layer <- glue::glue("layer_numeric_var(var = '{var}', ...)")
      vbl2D <- .filter_layer(vbl2D, .cond = .cond_quo, .by = .by, layer = layer)

      .layer_numeric_var_impl(
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
.layer_numeric_var_impl <- function(vbl2D,
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
      opacity = .eval_tidy_opacity(vbl2D, opacity = opacity, var = var),
      interpolate = interpolate
    ),
    scale_fill_numeric(
      clrsp,
      limits = var_smr(vbl, var)$limits,
      ...
    )
  )

}
