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

#' @export
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

#' @export
ggplane <- function(vbl,
                    plane,
                    slices,
                    var,
                    offset_dist = 0,
                    offset_dir = "left",
                    clrsp = c("black", "white"),
                    interpolate = TRUE,
                    lim = NULL,
                    lim_col = NULL,
                    lim_row = NULL,
                    expand = TRUE,
                    animate = FALSE,
                    cond = NULL,
                    ...){

  cond_quo <- rlang::enquo(cond)

  vbl2D <- vibble2D(
    vbl = vbl,
    plane = plane,
    slices = slices,
    offset_dist = offset_dist,
    offset_dir = offset_dir,
    cond = cond_quo
  )

  structure(
    list(
      vbl2D = vbl2D,
      plane = plane,
      slices = slices,
      base_args = list(
        var = var,
        clrsp = clrsp,
        interpolate = interpolate,
        lim = lim,
        lim_col = lim_col,
        lim_row = lim_row,
        expand = expand,
        animate = animate,
        ...
      ),
      layers = list()
    ),
    class = "ggvibble"
  )

}

#' @importFrom purrr map_lgl
#' @export
.ggplane_impl <- function(vbl2D,
                          var,
                          clrsp = c("black", "white"),
                          interpolate = TRUE,
                          lim = NULL,
                          lim_col = NULL,
                          lim_row = NULL,
                          expand = TRUE,
                          animate = FALSE,
                          ...){

  stopifnot(is.numeric(vbl2D[[var]]))

  # handle multiple slices
  if(dplyr::n_distinct(vbl2D$slice) > 1 & !isTRUE(attr(vbl2D, "offset")) & !isTRUE(animate)){

    layer_facet <- ggplot2::facet_wrap(facets = . ~ slice)

  } else {

    layer_facet <- NULL

  }

  # input for coord_equal
  ratio <- max(vbl2D$col)/max(vbl2D$row)

  # define limits
  if(is.numeric(lim)){

    stopifnot(is.numeric(lim) & length(lim) == 2)

    lim_col <- lim
    lim_row <- lim

  } else if(is.list(lim)){

    stopifnot(all(c("col", "row") %in% names(lim)))
    stopifnot(all(map_lgl(lim, ~ is.numeric(.x) & length(.x) == 2)))

    lim_col <- range(lim[["col"]])
    lim_row <- range(lim[["row"]])

  }

  lim_col <- if(is.numeric(lim_col)){ lim_col } else { range(vbl2D$col) }
  xlim <- c(min(lim_col), max(lim_col))

  lim_row <- if(is.numeric(lim_row)){ lim_row } else { range(vbl2D$row) }
  ylim <- c(max(lim_row), min(lim_row)) # invert for scale_y_reverse()

  # construct plot
  ggplot2::ggplot(data = vbl2D) +
    ggplot2::geom_raster(mapping = ggplot2::aes(x = col, y = row, fill = .data[[var]]), interpolate = interpolate) +
    scale_fill_numeric(clrsp = clrsp, limits = var_smr(vbl2D, var)$limits, ...) +
    ggplot2::scale_y_reverse() +
    ggplot2::coord_equal(ratio = ratio, xlim = xlim, ylim = ylim, expand = expand) +
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
    ) +
    layer_facet

}



