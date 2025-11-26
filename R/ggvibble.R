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


#' @title Create a 2D ggplot representation of voxel data
#' @description Build a 2D visualization of a 3D vibble by selecting slices in a
#' specified anatomical plane and plotting a numeric variable as a raster image.
#'
#' @param slices Numeric vector of slice positions in the selected `plane`.
#' @param var Name of the numeric variable to visualize.
#' @param offset_num Should slice index numbers be displayed if `offset_dist > 0`.
#'
#' If `TRUE`, the slice numbers are visualized with default settings of \link{layer_slice_num}().
#' Alternatively, a named list can be provided to specicy how `layer_slice_num()` should
#' be called.
#'
#' @param clrsp Color spectrum for numeric fill. May be two colors, a palette
#' name, or a viridis option.
#' @param lim Flexible input for the axis limits of the plot. `NULL`, named list of length 2
#' or numeric vector length 2 to constrain both axes. See section *Col and row limits*.
#' @param lim_col,lim_row Optional numeric vector length 2 for limits of the respective
#' plotting axis. Overwrite `lim`.
#' @param animate Logical. If `TRUE`, construct a plot structure suitable for
#' animation (no facetting across slices).
#' @param ncol,nrow Numeric or NULL. If `offset = 0`, both arguments are passed to
#' \link{facet_wrap()}.
#' @param cond Optional logical filter expression that determines the specific
#' voxels to be included in the plot - including all layers added to it. The expression
#' is evaluated with \link[rlang:args_data_masking]{tidyverse's data-masking}
#' semantics on the \link{vibble2D}() created. See \link[=ggvibble_doc_cond]{Details} for
#' more information and examples.
#'
#' @param ... Additional arguments forwarded to \link{scale_fill_numeric}().
#'
#' @inheritParams vbl_doc_var_numeric
#' @inheritParams vbl_doc
#'
#' @return A `ggvibble` object, which prints as a ggplot and can accept
#' additional layers via `+`.
#'
#' @details See \link{ggvibble}.
#'
#' @section Col and row limits:
#' Col and row limits are resolved in the following order. If `lim` is a numeric
#' vector of length 2, the same limits are applied to both axes. If `lim`
#' is a list, it must contain numeric elements `col` and `row`, which are
#' used separately for col and row. If `lim` is `NULL`, any numeric `lim_col`
#' and `lim_row` are used. Otherwise, limits default to the full range of
#' `col` and `row`. The y-axis is internally reversed to match
#' image-style coordinates.
#'
#' If `offset_dist > 0`, limits are computed automatically.
#'
#' @seealso [vibble2D()]
#'
#' @examples
#' vbl <- example_vbl()
#'
#' ggplane(
#'   vbl = vbl,
#'   plane = "axi",
#'   slices = 90,
#'   var = "raw_t1"
#' )
#'
#' @export
ggplane <- function(vbl,
                    plane,
                    slices,
                    var,
                    offset_dist = 0,
                    offset_dir = "left",
                    offset_num = TRUE,
                    clrp = vbl_opts("clrp"),
                    clrsp = vbl_opts("clrsp"),
                    interpolate = vbl_opts("interpolate"),
                    expand = vbl_opts("expand"),
                    lim = NULL,
                    ncol = NULL,
                    nrow = NULL,
                    animate = FALSE,
                    cond = NULL,
                    ...){

  .vbl_confirm(
    expr = length(slices) > 50,
    msg = "You are trying to plot >50 slices at once."
    )

  cond_quo <- rlang::enquo(cond)

  vbl2D <- vibble2D(
    vbl = vbl,
    plane = plane,
    slices = slices,
    lim = lim,
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
        offset_num = offset_num,
        interpolate = interpolate,
        expand = expand,
        lim = lim,
        ncol = ncol,
        nrow = nrow,
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
                          clrp = vbl_opts("clrp"),
                          clrsp = vbl_opts("clrsp"),
                          interpolate = vbl_opts("interpolate"),
                          expand = vbl_opts("expand"),
                          offset_num = TRUE,
                          lim = NULL,
                          ncol = NULL,
                          nrow = NULL,
                          animate = FALSE,
                          ...){

  stopifnot(is.numeric(vbl2D[[var]]))

  # handle multiple slices
  if(dplyr::n_distinct(vbl2D$slice) > 1 & !is_offset(vbl2D) & !isTRUE(animate)){

    layer_facet <- ggplot2::facet_wrap(facets = . ~ slice, ncol = ncol, nrow = nrow)

  } else {

    layer_facet <- NULL

  }

  # layer slice numbers
  if(is_offset(vbl2D) && isTRUE(offset_num)){

    layer_slice_numbers <- .layer_slice_number_impl(vbl2D)

  } else if(is_offset(vbl2D) && is.list(offset_num)){

    # offset_num is a list of arguments for .layer_slice_number_impl()
    # e.g. offset_num = list(vbl2D = vbl2D, fontsize = 12, col = "red")
    offset_num[["vbl2D"]] <- vbl2D
    layer_slice_numbers <- do.call(.layer_slice_number_impl, offset_num)

  } else {

    layer_slice_numbers <- list()

  }


  # layer colors
  if(is_numeric_var(vbl2D[[var]])){

    layer_colors <- scale_fill_numeric(clrsp = clrsp, limits = var_smr(vbl2D, var)$limits, ...)

  } else {

    if(is_mask_var(vbl2D[[var]])){

      vbl2D[[var]] <- factor(as.character(vbl2D[[var]]), levels = c("TRUE", "FALSE"))

    }

    layer_colors <- scale_fill_label(clrp = clrp, ...)

  }

  # construct plot
  ggplot2::ggplot(data = vbl2D) +
    ggplot2::geom_raster(mapping = ggplot2::aes(x = col, y = row, fill = .data[[var]]), interpolate = interpolate) +
    layer_colors +
    layer_slice_numbers +
    ggplot2::scale_y_reverse() +
    ggplot2::coord_equal(ratio = ratio2D(vbl2D), xlim = NULL, ylim = NULL, expand = expand) +
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



