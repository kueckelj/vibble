# The core plotting grammar bridge to ggplot2.

#' @export
as_ggplot <- function(p){ build_ggplot.ggvibble(p) }


#' @export
build_ggplot.ggvibble <- function(p){

  vbl2D <- p$vbl2D

  # build ggvibble
  g <- do.call(
    what = .ggplane_impl,
    args = c(list(vbl2D = vbl2D), p$base_args)
  )

  # add ggvibble_layers
  if(length(p$layers) > 0){

    # sort layers
    layers_ann <- purrr::keep(p$layers, .p = ~ "layer_ann" %in% class(.x))
    layers_bb <- purrr::keep(p$layers, .p = ~ "layer_bb" %in% class(.x))
    layers_mask <- purrr::keep(p$layers, .p = ~ "layer_mask" %in% class(.x))
    layers_outline <- purrr::keep(p$layers, .p = ~ "layer_outline" %in% class(.x))
    layers_raster <- purrr::keep(p$layers, .p = ~ "layer_raster" %in% class(.x))

    # 1. raster
    if(length(layers_raster) != 0){

      for(ly in layers_raster){

        g <- g + ly$fun(vbl2D)

      }

    }

    # 2. masks
    if(length(layers_mask) != 0){

      color_naming <-
        purrr::map(layers_mask, "color_nm") %>%
        purrr::flatten_chr()

      if(length(color_naming) != 0){

        g <-
          g +
          ggnewscale::new_scale_fill() +
          ggplot2::scale_fill_manual(values = color_naming, name = vbl_opts("legend.name.mask"))

      }

      for(ly in layers_mask){

        g <- g + ly$fun(vbl2D)

      }

    }

    # 3. bbs
    if(length(layers_bb) != 0){

      color_naming <-
        purrr::map(layers_bb, "color_nm") %>%
        purrr::flatten_chr()

      if(length(color_naming) != 0){

        g <-
          g +
          ggnewscale::new_scale_color() +
          ggplot2::scale_color_manual(values = color_naming, name = vbl_opts("legend.name.bb"))

      }

      for(ly in layers_bb){

        g <- g + ly$fun(vbl2D)

      }

    }

    # 4. outlines
    if(length(layers_outline) != 0){

      color_naming <-
        purrr::map(layers_outline, "color_nm") %>%
        purrr::flatten_chr()

      if(length(color_naming) != 0){

        g <-
          g +
          ggnewscale::new_scale_color() +
          ggplot2::scale_color_manual(values = color_naming, name = vbl_opts("legend.name.outline"))

      }

      for(ly in layers_outline){

        g <- g + ly$fun(vbl2D)

      }

    }

    # 5. annotations
    if(length(layers_ann) != 0){

      for(ly in layers_ann){

        g <- g + ly$fun(vbl2D)

      }

    }

  }

  g

}


#' @export
print.ggvibble <- function(x, ...){

  g <- build_ggplot.ggvibble(x)

  print(g)

  invisible(x)

}

# layer class -------------------------------------------------------------

#' @title Create a ggvibble layer object
#' @description Wrap a layer-construction function so it can be added to a
#' `ggvibble` object using the `+` operator. This is the constructor used
#' internally by all `layer_*()` functions.
#'
#' @param fun A function that takes a `vbl2D` object and returns one or more
#' ggplot2 layers (typically a list of geoms or scales).
#' @param class_add Character or NULL. If character, specifies additional layer
#' classes for (potential) specific management in the `build_ggplot.ggvibble()`.
#'
#'
#' @return A `ggvibble_layer` object containing the supplied function. When
#' added to a `ggvibble`, the function is executed and its returned layers are
#' inserted into the plot.
#'
#' @details
#' `vbl_layer()` standardizes how custom layers are attached to ggvibble plots.
#' The returned object is only a container; the actual layer generation happens
#' when `+.ggvibble` evaluates `fun(vbl2D)` during plot building.
#'
#' @seealso \link{ggplane}(), \link{layer_numeric}(), \link{layer_label}()
#'
#' @examples
#' vbl_layer(function(v) ggplot2::geom_point(aes(x = col, y = row)))
#'
#' @keywords internal
vbl_layer <- function(fun, class_add = NULL, ...){

  structure(
    .Data = list(fun = fun, ...),
    class = c(class_add, "ggvibble_layer")
  )

}


#' @export
`+.ggvibble` <- function(p, layer){

  if(inherits(layer, "ggvibble_layer")){

    p$layers <- c(p$layers, list(layer))
    return(p)

  } else {

    stop("Can only add ggvibble_layer objects to a ggvibble.", call. = FALSE)

  }

}


# ggplane constructor -----------------------------------------------------


#' @title Build a 2D ggplot representation of voxel data
#' @description Initializes a 2D visualization of voxel data by selecting slices in a
#' specified anatomical plane that can be complemented with additional layers.
#'
#' @param var Character scalar naming the variable to plot.
#' @param slices Integer vector. Slice indices in the specified anatomical
#' `plane` to be included in the plot.
#' @param ncol,nrow Passed to `facet_wrap()` setting the facetting layout
#' when neither of `offset_col` or `offset_row` differs from 0.
#' @param .cond Optional. An additional logical filter expression that determines the specific
#' voxels plotted with `ggplane()` and passed to added \link[vbl_doc_vbl_layer]{layers}. <br>
#'
#' The expression is evaluated via \link[rlang:args_data_masking]{data-masking} semantics.
#'
#' @param ... Additional arguments forwarded to the scale functions controlling the fill aesthetic.
#' @inheritParams vibble2D
#' @inheritParams vbl_doc
#'
#' @return A \link{ggvibble} object.
#'
#' @details
#' `ggplane()` converts `vbl` to a `vbl2D` object via \link{vibble2D}().
#' It then stores plotting arguments and returns a `ggvibble` container. If multiple slices are present
#' and the object is not offset, slices are arranged using \link{facet_wrap}() with the requested `layout`.
#'
#' Numeric variables are plotted with \link{scale_fill_numeric}() using `clrsp`. Non-numeric variables are
#' plotted with \link{scale_fill_label}() using `clrp`. Note, that in `ggplane()`, logical mask variables
#' are coerced to factors with levels `c("TRUE", "FALSE")` before plotting, for binary mask overlays see \link{layer_mask()}.
#'
#' @export
ggplane <- function(vbl,
                    var,
                    slices,
                    plane = vbl_opts("plane"),
                    crop = NULL,
                    expand = 0.1,
                    offset_col = 0,
                    offset_row = 0,
                    zstack = "desc",
                    ncol = NULL,
                    nrow = NULL,
                    alpha = 1,
                    clrp = vbl_opts("clrp"),
                    clrsp = vbl_opts("clrsp"),
                    verbose = vbl_opts("verbose"),
                    .cond = NULL,
                    .by = NULL,
                    ...){

  stopifnot(is_vbl(vbl))

  .vbl_confirm(
    expr = length(slices) > 50,
    msg = "You are trying to plot >50 slices at once."
    )

  # construct vbl2D
  vbl2D <- vibble2D(
    vbl = vbl,
    plane = plane,
    slices = slices,
    crop = crop,
    offset_col = offset_col,
    offset_row = offset_row,
    zstack = zstack,
    expand = expand
  )

  # filter
  .cond_quo <- rlang::enquo(.cond)
  .by_quo <- rlang::enquo(.by)
  vbl2D <- .filter_layer(vbl2D, .cond = .cond_quo, .by = .by_quo, layer = "ggplane()")

  assign("vbl2D", vbl2D, envir = .GlobalEnv)

  structure(
    list(
      vbl2D = vbl2D,
      plane = plane,
      slices = slices,
      base_args = list(
        var = var,
        alpha = alpha,
        clrp = clrp,
        clrsp = clrsp,
        ncol = ncol,
        nrow = nrow,
        verbose = verbose,
        ...
      ),
      layers = list()
    ),
    class = "ggvibble"
  )

}

#' @keywords internal
.ggplane_impl <- function(vbl2D,
                          var,
                          alpha,
                          clrp,
                          clrsp,
                          ncol,
                          nrow,
                          verbose,
                          ...){

  # handle multiple slices
  if(dplyr::n_distinct(vbl2D$slice) > 1 & !is_offset(vbl2D)){

    layer_facet <- ggplot2::facet_wrap(facets = . ~ slice, ncol = ncol, nrow = nrow)

  } else {

    layer_facet <- NULL

  }

  # layer colors
  if(is_numeric_var(vbl2D[[var]])){

    layer_colors <- scale_fill_numeric(clrsp = clrsp, limits = .get_var_limits(vbl2D, var, verbose), ...)

  } else {

    if(is_mask_var(vbl2D[[var]])){

      vbl2D[[var]] <- factor(as.character(vbl2D[[var]]), levels = c("TRUE", "FALSE"))

    }

    layer_colors <- scale_fill_categorical(clrp = clrp, name = var, names = levels(vbl2D[[var]]), ...)

  }

  col_lim <- plot_bb(vbl2D)$col
  row_lim <- plot_bb(vbl2D)$row

  # construct plot
  ggplot2::ggplot(data = vbl2D) +
    ggplot2::geom_raster(
      mapping = ggplot2::aes(x = col, y = row, fill = .data[[var]]),
      alpha = alpha,
      interpolate = vbl_opts("interpolate")
    ) +
    layer_colors +
    layer_facet +
    ggplot2::scale_y_reverse() +
    ggplot2::coord_equal(ratio = .ratio2D(vbl2D), xlim = col_lim, ylim = rev(row_lim), expand = vbl_opts("expand.ggplot")) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.background = ggplot2::element_rect(fill = "black"),
      legend.text = ggplot2::element_text(color = "white"),
      legend.title = ggplot2::element_text(color = "white"),
      panel.background = ggplot2::element_rect(fill = "black"),
      panel.grid = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "black", color = "black"),
      plot.caption = ggplot2::element_text(color = "white"),
      plot.subtitle = ggplot2::element_text(color = "white"),
      plot.title = ggplot2::element_text(color = "white"),
      strip.background = ggplot2::element_rect(fill = "black", color = "black"),
      strip.text = ggplot2::element_text(color = "white")
    )


}



