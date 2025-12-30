# The core plotting grammar bridge to ggplot2.


#' @title Convert a ggvibble to a ggplot object
#'
#' @description Materializes a `ggvibble` object into a ggplot object.
#'
#' @param x A `ggvibble` object.
#'
#' @return
#' A ggplot object that can be further modified using ggplot2 or composed using
#' patchwork.
#'
#' @details
#' `as_ggplot()` terminates the ggvibble layer pipeline and converts it into a
#' concrete ggplot representation. After conversion, ggvibble layers can no
#' longer be added.
#'
#' @export
as_ggplot <- function(p, ...){

  build_ggplot.ggvibble(p, ...)

}

as_ggp <- as_ggplot


#' @keywords internal
#' @export
build_ggplot.ggvibble <- function(p, animate = FALSE, ...){

  vbl2D <- p$vbl2D
  p$base_args$animate <- isTRUE(animate)

  # build ggvibble
  g <-
    ggplot2::ggplot(data = vbl2D) +
    do.call(
      what = .ggplane_impl,
      args = c(list(vbl2D = vbl2D), p$args_ggplane)
    )

  # add ggvibble_layers
  if(length(p$layers) > 0){

    # identify layer types
    layers_ann <- purrr::keep(p$layers, .p = ~ "layer_ann" %in% class(.x))
    layers_bb <- purrr::keep(p$layers, .p = ~ "layer_bb" %in% class(.x))
    layers_crop <- purrr::keep(p$layers, .p = ~ "layer_crop" %in% class(.x))
    layers_mask <- purrr::keep(p$layers, .p = ~ "layer_mask" %in% class(.x))
    layers_misc <- purrr::keep(p$layers, .p = ~ "layer_misc" %in% class(.x))
    layers_outline <- purrr::keep(p$layers, .p = ~ "layer_outline" %in% class(.x))
    layers_raster <- purrr::keep(p$layers, .p = ~ "layer_raster" %in% class(.x))

    # sort layers
    # 1. raster
    if(length(layers_raster) != 0){

      for(ly in layers_raster){

        g <- g + ly$fun(vbl2D, p$context)

      }

    }

    # 2. masks
    if(length(layers_mask) != 0){

      legend_labels <-
        purrr::map(layers_mask, "legend_label") %>%
        purrr::flatten_chr()

      if(length(legend_labels) != 0){

        g <-
          g +
          ggnewscale::new_scale_fill() +
          ggplot2::scale_fill_manual(values = legend_labels, name = vbl_opts("legend.name.mask"))

      }

      for(ly in layers_mask){

        g <- g + ly$fun(vbl2D, p$context)

      }

    }

    # 3. bbs
    if(length(layers_bb) != 0){

      legend_labels <-
        purrr::map(layers_bb, "legend_label") %>%
        purrr::flatten_chr()

      if(length(legend_labels) != 0){

        g <-
          g +
          ggnewscale::new_scale_color() +
          ggplot2::scale_color_manual(values = legend_labels, name = vbl_opts("legend.name.bb"))

      }

      for(ly in layers_bb){

        g <- g + ly$fun(vbl2D, p$context)

      }

    }

    # 4. outlines
    if(length(layers_outline) != 0){

      legend_labels <-
        purrr::map(layers_outline, "legend_label") %>%
        purrr::flatten_chr()

      if(length(legend_labels) != 0){

        g <-
          g +
          ggnewscale::new_scale_color() +
          ggplot2::scale_color_manual(values = legend_labels, name = vbl_opts("legend.name.outline"))

      }

      for(ly in layers_outline){

        g <- g + ly$fun(vbl2D, p$context)

      }

    }

    # 5. annotations
    if(length(layers_ann) != 0){

      for(ly in layers_ann){

        g <- g + ly$fun(vbl2D, p$context)

      }

    }

    # 6. misc
    if(length(layers_misc) != 0){

      for(ly in layers_misc){

        g <- g + ly$fun(vbl2D, p$context)

      }

    }

    # 7. cropping
    if(length(layers_crop) != 0){

      if(length(layers_crop) > 1){

        msg <- "Multple instances of `layer_crop()` detected. Using last one."
        rlang::inform(msg)

      }

      ly_crop <- layers_crop[[length(layers_crop)]]

      g <- g + ly_crop$fun(vbl2D, p$context)

    }

  }

  g

}

#' @title Plot and print ggvibble objects
#'
#' @description Methods to render a `ggvibble` by materializing it as a ggplot object.
#'
#' @param x A `ggvibble` object.
#' @inheritParams vbl_doc
#' @param ... Additional arguments, currently unused.
#'
#' @return Invisibly returns `x`.
#'
#' @export
plot.ggvibble <- function(x,
                          zstack = NULL,
                          offset_col = NULL,
                          offset_row = NULL,
                          offset_reverse = FALSE,
                          ...){

  # quick offset before plotting, if offset_reverse is not desired
  if(!isTRUE(offset_reverse)){

    # offset = 0 -> nothing happens
    x$vbl2D <-
      apply_offset(
        vbl2D = x$vbl2D,
        offset_col = ifelse(is.null(offset_col), 0, offset_col),
        offset_row = ifelse(is.null(offset_row), 0, offset_row)
      )

  }

  # quick offset reversing before plotting
  if(isTRUE(offset_reverse)){

    x$vbl2D <- reverse_offset(x$vbl2D)

  }

  # quick zstack changing before plotting
  if(is.character(zstack)){

    x$vbl2D <- apply_zstack(vbl2D, zstack = zstack)

  }


  g <- as_ggplot(x)

  plot(g)

  invisible(x)

}

#' @rdname plot.ggvibble
#' @export
print.ggvibble <- function(p, ...){

  g <- as_ggplot(p)

  print(g)

  invisible(p)

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
#' @return A `ggvibble_layer` object containing the supplied function. When
#' added to a `ggvibble`, the function is executed and its returned layers are
#' inserted into the plot.
#'
#' @details
#' `vbl_layer()` standardizes how custom layers are attached to ggvibble plots.
#' The returned object is only a container; the actual layer generation happens
#' when `+.ggvibble` evaluates `fun(vbl2D, p$context)` during plot building.
#'
#' @keywords internal
vbl_layer <- function(fun, class_add = NULL, ...){

  structure(
    .Data = list(fun = fun, ...),
    class = c(class_add, "ggvibble_layer")
  )

}


#' @title Operators in the ggvibble framework
#' @name vbl_doc_ggvibble_operators
#' @description
#' Defines how `ggvibble` objects behave with the operators `+`, `/`, and `|`.
#'
#' @details
#' The `ggvibble` plotting framework is a deferred plotting pipeline that is
#' converted into a ggplot object only when needed. Operator support is therefore
#' limited to operations that can be implemented safely and unambiguously.
#'
#' \itemize{
#'   \item{`+` : } Adds a ggvibble layer to a ggvibble, or combines two ggvibbles
#'   (requires patchwork to be attached).
#'   \item{`/` : } Combines two ggvibbles vertically (requires patchwork to be attached).
#'   \item{`|` : } Combines two ggvibbles horizontally (requires patchwork to be attached).
#' }
#'
#' @section Why direct integration with ggplot2 is not supported:
#' ggplot2 uses S3 operator methods for `+` (e.g. `+.gg`). Many ggplot2 components
#' (themes, scales, coordinate systems) participate in this operator dispatch.
#' Adding such objects directly to a `ggvibble` via `+` can trigger ambiguous S3
#' dispatch and results in errors such as "Incompatible methods" or fallback to the
#' base numeric `+` operator.
#'
#' For this reason, `ggvibble` **does not support** adding ggplot2 components to a
#' `ggvibble` object directly with `+`. Instead, convert explicitly to ggplot, or
#' inject ggplot2 objects through `layer_misc()`.
#'
#' \preformatted{
#' p <- ggplane(vbl, var = "t1", plane = "axi", slices = 120)
#'
#' # fails:
#' p + ggplot2::theme_void()
#'
#' # preferred: convert explicitly and continue with ggplot2
#' as_ggplot(p) + ggplot2::theme_void()
#'
#' # alternative: add ggplot2 objects through the ggvibble pipeline
#' p + layer_misc(ggplot2::theme_void())
#' }
#'
#' @section Patchwork composition:
#' Combining ggvibbles with `+`, `/`, or `|` relies on patchwork composition.
#' Patchwork must be attached to enable plot composition.
#'
#' \preformatted{
#' library(patchwork)
#'
#' p1 <- ggplane(vbl, var = "t1", plane = "axi", slices = 120)
#' p2 <- ggplane(vbl, var = "flair", plane = "axi", slices = 120)
#'
#' # horizontal and vertical composition
#' p1 | p2
#' p1 / p2
#'
#' # combining with `+` is also supported for ggvibble objects
#' p1 + p2
#' }
#'
#' @section Notes:
#' \itemize{
#'   \item{Operator scope: } Only `+`, `/`, and `|` are implemented for `ggvibble`.
#'   \item{Layer addition: } `+` supports adding `ggvibble_layer` objects to a `ggvibble`.
#'   \item{Composition: } Combining two `ggvibble` objects converts both to ggplot and
#'   returns a patchwork object.
#' }
#'
#' @seealso \link{as_ggplot}()
#' @export
Ops.ggvibble <- function(x, y){

  if(!.Generic %in% c("+", "/", "|")){

    rlang::abort(
      c(
        glue::glue("Operator `{.Generic}` is not implemented for `ggvibble`."),
        "i" = "Supported operators are `+`, `/`, and `|`."
      ),
      call = FALSE
    )

  }

  if(.Generic == "+"){

    if(inherits(y, "list")){

      y <- purrr::keep(y, .p = ~ inherits(.x, "ggvibble_layer"))

      for(i in seq_along(y)){

        x$layers <- c(x$layers, list(y[[i]]))

      }

      return(x)

    } else if(inherits(y, "ggvibble_layer")){

      x$layers <- c(x$layers, list(y))
      return(x)

    } else if(inherits(y, "ggvibble")) {

      .check_patchwork_attached()

      xp <- as_ggplot(x)
      yp <- as_ggplot(y)

      return(xp + yp)

    }

    rlang::abort(
      c(
        glue::glue("Invalid object supplied to `{.Generic}.ggvibble`."),
        "i" = "Use `+` to add ggvibble layers or other ggvibble objects."
      )
    )

  }

  if(.Generic == "/"){

    if(inherits(y, "ggvibble")){

      .check_patchwork_attached()

      xp <- as_ggplot(x)
      yp <- as_ggplot(y)

      return(xp / yp)

    }

    ic <-
      stringr::str_c(class(y), collapse = "', '") %>%
      stringr::str_c("c('", ., "')")

    rlang::abort(
      c(
        glue::glue("Invalid object supplied of class {ic} to `/.ggvibble`."),
        "i" = "Use `/` to combine ggvibbles."
      )
    )

  }

  if(.Generic == "|"){

    if(inherits(y, "ggvibble")){

      .check_patchwork_attached()

      xp <- as_ggplot(x)
      yp <- as_ggplot(y)

      return(xp | yp)

    }

    ic <-
      stringr::str_c(class(y), collapse = "', '") %>%
      stringr::str_c("c('", ., "')")

    rlang::abort(
      c(
        glue::glue("Invalid object supplied of class {ic} to `|.ggvibble`."),
        "i" = "Use `|` to combine ggvibbles."
      )
    )

  }

}










# ggplane constructor -----------------------------------------------------


#' @title Initialize a 2D representation of voxel data
#' @description
#' Initializes a 2D visualization of voxel data in a specified anatomical plane that
#' can be customized with additional layers.
#'
#' @param var Determines the data to plot in the background.
#'
#' \itemize{
#'  \item{character:}{ Specifies the variable explicitly by name (recommended).}
#'  \item{`NULL`:}{ No data is plotted in the background.}
#'  \item{default:}{ Selects the first name of \link{vars_numeric()}. }
#'  }
#'
#' @param slices Integer vector of \link[=is_slice]{slice numbers} in the specified anatomical
#' `plane` to be included in the plot. Defaults to using \link{slices_mid}().
#' @param ncol,nrow Passed to \link{facet_wrap()} in no-offset layouts.
#' @param guide Passed to `guide` of the `scale_fill_<fn>()` required to map a color
#' to `var` (if not `NULL`). In `ggplane()`, this defaults to *'none'*, cause it expects
#' an intensity variable like T1 or FLAIR which usually does not need a legend for
#' interpretation.
#'
#' To force a guide, use `guide = 'colourbar'` for numeric and `guide = 'legend'` for categorical variables.
#' @param .cond Optional. A logical filter expression evaluated on the constructed
#' `vbl2D`. The expression is evaluated via \link[rlang:args_data_masking]{data-masking} semantics.
#' @param ... Additional arguments forwarded to the fill scale functions.
#' @param context List of context specifications with which layers are evaluated.
#' @inheritParams vibble2D
#' @inheritParams vbl_doc
#'
#' @return A \link{ggvibble} object.
#'
#' @details
#' `ggplane()` constructs a `ggvibble` in a small number of explicit steps.
#'
#' \itemize{
#'   \item{1. Build 2D representation: } Converts `vbl` to `vbl2D` via \link{vibble2D}()
#'   by selecting `plane` and `slices` and applying `crop` and `expand`.
#'   \item{2. Filter plotted voxels: } Applies `.cond` (and optional grouping via `.by`)
#'   using `.filter_layer()` to determine which voxels are shown.
#'   \item{3. Order slices: } Optionally arranges `vbl2D` by slice number via `zstack`
#'   to control draw order when slices overlap in offset layouts.
#'   \item{4. Apply offsets: } Applies `offset_col` and `offset_row` via `apply_offset()`.
#'   If both offsets are zero and multiple slices are present, slices are displayed in
#'   separate panels using `facet_wrap(. ~ slice)`; otherwise slices are drawn in one
#'   shared panel.
#'   \item{5. Store plot specification: } Stores the resulting `vbl2D`, base plotting
#'   arguments, and an initially empty layer list in a `ggvibble` container.
#'   \item{6. Determine plot limits and expansion: } The plotting bounding box is
#'   derived from the spatial extent of the displayed voxels. Expansion is applied
#'   in absolute image-space units, choosing the smallest symmetric expansion across
#'   axes to preserve aspect ratio. Final limits are enforced via `coord_equal()`.
#' }
#'
#' The final ggplot is created when the object is printed or plotted. The base plot
#' uses `geom_raster()` in image space with `scale_y_reverse()` and `coord_equal()`.
#'
#' Numeric variables are mapped with \link{scale_fill_numeric}() (via `clrsp`).
#' Non-numeric variables are mapped with \link{scale_fill_categorical}() (via `clrp`).
#' Logical mask variables are coerced to a factor with levels `c("TRUE", "FALSE")`.
#' For binary mask overlays, use \link{layer_mask}().
#'
#' @seealso Details about the \link[=vbl_doc_ggvibble]{ggvibble framework}
#' and \link[=vbl_doc_ggvibble_operators]{ggplot2 compatibility}.
#'
#' @export
ggplane <- function(vbl,
                    var = vbl_def(),
                    slices = vbl_def(),
                    plane = vbl_def(),
                    crop = NULL,
                    expand = 0.1,
                    offset_col = 0,
                    offset_row = 0,
                    zstack = "desc",
                    ncol = NULL,
                    nrow = NULL,
                    guide = "none",
                    context = list(),
                    .cond = NULL,
                    .by = NULL,
                    ...){

  .stop_if_not(is_vbl(vbl))
  .stop_if_not(is.list(context))

  plane <- .resolve_plane(plane = plane)

  if(.is_vbl_def(slices)){

    slices <- slices_mid(vbl, plane = plane)

  } else {

    .stop_if_not(is_slice_set(slices))

  }

  # construct vbl2D
  vbl2D <- vibble2D(
    vbl = vbl,
    plane = plane,
    slices = slices,
    crop = crop,
    expand = expand,
    .cond = {{ .cond }},
    .by = {{ .by }}
  )

  # apply offset
  vbl2D <- apply_offset(vbl2D, offset_col = offset_col, offset_row = offset_row)

  # manage z-stack
  vbl2D <- apply_zstack(vbl2D, zstack = zstack)

  # manage visibility (in offset layouts)
  vbl2D$visible. <- TRUE
  if(.clip_offset(vbl2D)){

    vbl2D <- .clip_offset_raster(vbl2D)

  }

  # context
  context <- list()
  context$outlines_full <- if(.clip_offset(vbl2D)) .comp_outlines(vbl2D)

  assign("vbl2D", vbl2D, envir = .GlobalEnv)

  structure(
    list(
      vbl2D = vbl2D,
      args_ggplane = list(
        var = var,
        ncol = ncol,
        nrow = nrow,
        guide = guide,
        animate = FALSE,
        ...
      ),
      context = context,
      layers = list()
    ),
    class = "ggvibble"
  )

}

#' @title ggplane implementation
#' @param animate Logical. If `TRUE`, no facets are created. In `ggplane()`, this
#' is always FALSE. Inside build_ggplot.ggvibble() this can be set to TRUE. Should
#' only be set to TRUE within `render_animation()`.
#'
#' @keywords internal
.ggplane_impl <- function(vbl2D,
                          var,
                          ncol,
                          nrow,
                          guide,
                          animate,
                          ...){

  # handle multiple slices
  if(dplyr::n_distinct(vbl2D$slice) > 1 & !is_offset(vbl2D) & isFALSE(animate)){

    layer_facet <- ggplot2::facet_wrap(facets = . ~ slice, ncol = ncol, nrow = nrow)

  } else {

    layer_facet <- NULL

  }

  # resolve var if vbl_def
  # resolve var if vbl_def
  if(.is_vbl_def(var)){

    vars_n <- vars_numeric(vbl2D)

    if(length(vars_n) == 0){

      var <- NULL

      msg <- c(
        "No numeric variables available for default background mapping in ggplane().",
        i = "Proceeding with `var = NULL`; no background data will be drawn.",
        i = "Consider specifying `var` explicitly."
      )

      rlang::warn(message = msg, .frequency = "always")

    } else {

      var <- vars_n[1]

      msg <- c(
        "Using default mechanism for selecting a data variable in ggplane().",
        i = glue::glue("Selected first numeric variable: {var}"),
        i = "Consider specifying `var` explicitly."
      )

      rlang::inform(
        message = msg,
        .frequency = "once",
        .frequency_id = "ggplane.var.default2"
      )

    }

  }

  # prepar layer colors
  if(is.character(var)){

    layer_raster <-
      ggplot2::geom_raster(
        data = vbl2D,
        mapping = ggplot2::aes(x = col, y = row, fill = .data[[var]]),
        alpha = 1,
        interpolate = vbl_opts("interpolate")
      )

    if(is_numeric_var(vbl2D[[var]])){

      layer_colors <- scale_fill_numeric(limits = .get_var_limits(vbl2D, var), guide = guide, ...)

    } else {

      if(is_mask_var(vbl2D[[var]])){

        vbl2D[[var]] <- factor(as.character(vbl2D[[var]]), levels = c("TRUE", "FALSE"))

      }

      layer_colors <- scale_fill_categorical(name = var, names = levels(vbl2D[[var]]), guide = guide, ...)

    }

  } else {

    layer_raster <- NULL
    layer_colors <- NULL

  }

  # which expansion is smaller in absolutes?
  plot_lim <-
    expand_bb2D(
      bb2D = plot_bb(vbl2D),
      expand = vbl_opts("expand.ggplane")
      )

  expand <-
    purrr::map2_dbl(
      .x = plot_lim,
      .y = plot_bb(vbl2D),
      .f = ~ (abs(diff(.x)) - abs(diff(.y)))/2
    )

  plot_lim <-
    expand_bb2D(
      bb2D = plot_bb(vbl2D),
      expand = as_abs(expand[which.min(expand)])
    )

  col_lim <- plot_lim$col
  row_lim <- plot_lim$row

  # construct plot
  list(
      layer_raster,
      layer_colors,
      layer_facet,
      ggplot2::scale_y_reverse(),
      ggplot2::coord_equal(xlim = col_lim, ylim = rev(row_lim), expand = FALSE),
      ggplot2::theme(
        axis.ticks = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
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
  )

}



