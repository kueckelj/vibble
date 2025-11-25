# ggvibble overlays and themes


# data --------------------------------------------------------------------

#' @export
layer_bb <- function(var,
                     color,
                     buffer = 0.05,
                     slices = NULL,
                     ...){

  vbl_layer(
    fun = function(vbl2D){

      vbl2D <- filter_slices(vbl2D, slices)

      .layer_bb_impl(
        vbl2D = vbl2D,
        color = color,
        buffer = buffer,
        ...
      )

    }
  )

}


#' @export
.layer_bb_impl <- function(vbl2D,
                           var,
                           color,
                           buffer = 0.05,
                           ...){

  is_vartype(vbl2D, var = var, type = "mask")

  list(
    ggplot2::geom_rect(
      data = bb2D(vbl2D, var = var, buffer = buffer),
      mapping = ggplot2::aes(xmin = cmin, xmax = cmax, ymin = rmin, ymax = rmax),
      color = color,
      fill = NA,
      ...
    )
  )

}

#' @export
layer_label <- function(var,
                        labels = NULL,
                        labels_rm = NULL,
                        clrp = "default",
                        clrp_adjust = NULL,
                        alpha = 0.45,
                        slices = NULL,
                        cond = NULL,
                        ...){

  alpha_quo <- rlang::enquo(alpha)
  cond_quo <- rlang::enquo(cond)

  vbl_layer(
    fun = function(vbl2D){

      vbl2D <- filter_slices(vbl2D, slices, cond_quo)

      .layer_label_impl(
        vbl2D = vbl2D,
        var = var,
        labels = labels,
        labels_rm = labels_rm,
        clrp = clrp,
        clrp_adjust = clrp_adjust,
        alpha = alpha_quo,
        ...
      )

    }
  )

}


#' @export
.layer_label_impl <- function(vbl2D,
                              var,
                              labels = NULL,
                              labels_rm = NULL,
                              clrp = "default",
                              clrp_adjust = NULL,
                              alpha = 0.45,
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
      alpha = eval_tidy_alpha(vbl2D, alpha = alpha)
    ),
    ggplot2::scale_fill_manual(values = color_mapping, ...)
  )

}


#' @export
layer_mask <- function(var,
                       color,
                       alpha = 0.45,
                       fill = TRUE,
                       outline = TRUE,
                       concavity = 2.5,
                       nv = 50,
                       slices = NULL,
                       cond = NULL,
                       ...){

  alpha_quo <- rlang::enquo(alpha)
  cond_quo <- rlang::enquo(cond)

  vbl_layer(
    fun = function(vbl2D){

      vbl2D <- filter_slices(vbl2D, slices, cond_quo)

      .layer_mask_impl(
        vbl2D = vbl2D,
        var = var,
        color = color,
        alpha = alpha_quo,
        fill = fill,
        outline = outline,
        concavity = concavity,
        nv = nv,
        ...
      )

    }
  )

}


#' @export
.layer_mask_impl <- function(vbl2D,
                             var,
                             color,
                             alpha = 0.45,
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
        alpha = eval_tidy_alpha(data, alpha = alpha),
        fill = color
      )

  }

  if(isTRUE(outline)){

    require(concaveman)

    outline_df <-
      dplyr::group_by(data, slice) %>%
      dplyr::group_split() %>%
      purrr::map_dfr(.f = ~ dbscan2D(.x, var = var, rm0 = TRUE)) %>%
      dplyr::group_by(slice, !!rlang::sym(var)) %>%
      dplyr::group_split() %>%
      purrr::imap_dfr(.f = function(df, idx){

        concaveman(as.matrix(df[,c("col", "row")]), concavity = concavity) %>%
          as_tibble() %>%
          magrittr::set_colnames(value = c("col", "row")) %>%
          densify_poly(n = nv) %>%
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

#' @export
layer_numeric <- function(var,
                          clrsp,
                          alpha = c(0.2, 0.45),
                          interpolate = TRUE,
                          slices = NULL,
                          cond = NULL,
                          layer_add = NULL,
                          ...){

  cond_quo <- rlang::enquo(cond)
  alpha_quo <- rlang::enquo(alpha)

  vbl_layer(
    fun = function(vbl2D){

      vbl2D <- filter_slices(vbl2D, slices, cond_quo)

      .layer_numeric_impl(
        vbl2D = vbl2D,
        var = var,
        clrsp = clrsp,
        alpha = alpha_quo,
        interpolate = interpolate,
        ...
      )

    }
  )

}

#' @export
.layer_numeric_impl <- function(vbl2D,
                                var,
                                clrsp,
                                alpha = c(0.2, 0.45),
                                interpolate = TRUE,
                                ...){

  is_vartype(vbl2D, var = var, type = "numeric")

  list(
    ggnewscale::new_scale_fill(),
    ggplot2::geom_raster(
      data = vbl2D,
      mapping = ggplot2::aes(x = col, y = row, fill = .data[[var]]),
      alpha = eval_tidy_alpha(vbl2D, alpha = alpha),
      interpolate = interpolate
    ),
    scale_fill_numeric(
      clrsp,
      limits = var_smr(vbl, var)$limits,
      ...
    )
  )

}


# non-data ----------------------------------------------------------------

#' @export
layer_grid <- function(col = 0.1,
                       row = 0.1,
                       alpha = 0.2,
                       color = "lightgrey",
                       linewidth = 0.5,
                       linetype = "solid",
                       slices = NULL,
                       ...){

  vbl_layer(
    fun = function(vbl2D){

      vbl2D <- filter_slices(vbl2D, slices)

      .layer_grid_impl(
        vbl2D = vbl2D,
        col = col,
        row = row,
        alpha = alpha,
        color = color,
        linewidth = linewidth,
        slices = slices,
        ...
      )

    }
  )

}


#' @export
.layer_grid_impl <- function(vbl2D,
                             col = 0.1,
                             row = 0.1,
                             alpha = 0.2,
                             color = "lightgrey",
                             linewidth = 0.5,
                             linetype = "solid",
                             ...){

  style_lst <-
    list(alpha = alpha, color = color, linewidth = linewidth, linetype = linetype) %>%
    purrr::map(.f = ~ if(length(.x)==1){ rep(.x, 2) } else { .x } )

  data <-
    tidyr::expand_grid(
      slice = sort(unique(vbl2D$slice)),
      col = grid_intercepts(col, limits = attr(vbl2D, "var_smr")[["col"]]$limits),
      row = grid_intercepts(row, limits = attr(vbl2D, "var_smr")[["row"]]$limits)
    )

  layer_lst <- list()

  if(is.numeric(col)){

    layer_lst$col <-
      ggplot2::geom_vline(
        data = data,
        mapping = ggplot2::aes(xintercept = col),
        alpha = style_lst$alpha[1],
        color = style_lst$color[1],
        linetype = style_lst$linetype[1],
        linewidth = style_lst$linewidth[1]
      )

  }

  if(is.numeric(row)){

    layer_lst$row <-
      ggplot2::geom_hline(
        data = data,
        mapping = ggplot2::aes(yintercept = row),
        alpha = style_lst$alpha[2],
        color = style_lst$color[2],
        linetype = style_lst$linetype[2],
        linewidth = style_lst$linewidth[2]
      )

  }

  return(layer_lst)

}

layer_slice_number <- function(){}

.layer_slice_number_impl <- function(){}







