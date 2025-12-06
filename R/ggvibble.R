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

    for(ly in p$layers){

      g <- g + ly$fun(vbl2D)

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
vbl_layer <- function(fun){

  structure(
    .Data = list(fun = fun),
    class = "ggvibble_layer"
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
#' @description Build a 2D visualization of vocel data by selecting slices in a
#' specified anatomical plane and plotting a numeric variable as a raster image.
#'
#' @param var Character scalar naming the variable to plot.
#' @param layout Optional numeric vector of length two specifying `c(ncol, nrow)` for `facet_wrap()`
#' when multiple slices are shown and `offset_dist` ist 0.
#' @param flip Logical. If `TRUE`, the z-order of the slices is flipped. Only relevent for offset-layouts.
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

#' @export
ggplane <- function(vbl,
                    var,
                    plane,
                    slices,
                    lim = NULL,
                    expand = 0.1,
                    offset_col = 0,
                    offset_row = 0,
                    order = "desc",
                    clrp = vbl_opts("clrp"),
                    clrsp = vbl_opts("clrsp"),
                    interpolate = vbl_opts("interpolate"),
                    layout = NULL,
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
    lim = lim,
    offset_col = offset_col,
    offset_row = offset_row,
    order = order,
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
        clrp = clrp,
        clrsp = clrsp,
        interpolate = interpolate,
        layout = layout,
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
                          clrp = vbl_opts("clrp"),
                          clrsp = vbl_opts("clrsp"),
                          interpolate = vbl_opts("interpolate"),
                          layout = NULL,
                          ...){

  stopifnot(is.numeric(vbl2D[[var]]))

  # handle multiple slices
  if(dplyr::n_distinct(vbl2D$slice) > 1 & !is_offset(vbl2D)){

    layer_facet <- ggplot2::facet_wrap(facets = . ~ slice, ncol = layout[1], nrow = layout[2])

  } else {

    layer_facet <- NULL

  }

  # layer colors
  if(is_numeric_var(vbl2D[[var]])){

    layer_colors <- scale_fill_numeric(clrsp = clrsp, limits = var_limits(vbl2D, var), ...)

  } else {

    if(is_mask_var(vbl2D[[var]])){

      vbl2D[[var]] <- factor(as.character(vbl2D[[var]]), levels = c("TRUE", "FALSE"))

    }

    layer_colors <- scale_fill_label(clrp = clrp, ...)

  }

  col_lim <- plot_limits(vbl2D)$col
  row_lim <- plot_limits(vbl2D)$row

  # construct plot
  ggplot2::ggplot(data = vbl2D) +
    ggplot2::geom_tile(
      mapping = ggplot2::aes(x = col, y = row, fill = .data[[var]]),
      color = NA
    ) +
    layer_colors +
    layer_facet +
    ggplot2::scale_y_reverse() +
    ggplot2::coord_equal(ratio = .ratio2D(vbl2D), xlim = col_lim, ylim = rev(row_lim), expand = vbl_opts("ggplot.expand")) +
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



