# ggvibble overlays and themes


# non-data ----------------------------------------------------------------

#' @title Add a data-driven 2D bounding-box
#' @description Draw a rectangular bounding box around the extent of voxels
#' or \link[=vbl_doc_ref_bb]{2D reference bounding boxes}.
#'
#' @param color Color of the bounding-box outline.
#' @param fill Fill of the bounding-box.
#'
#' @param ... Additional arguments passed to \link{geom_rect}().
#'
#' @inherit vbl_doc_layer params return
#'
#' @inheritParams vbl_doc_var_mask
#' @inheritParams vbl_doc
#'
#' @details
#' `layer_bb()` computes and visualizes a 2D bounding box for voxels based on the
#' minimal and maximal `col` and `row` coordinates where `.cond` evaluates to `TRUE`.
#' In all cases, the graphical rendering is handled by \link{geom_rect}().
#'
#' Bounding boxes are **always** computed slicewise.
#'
#' @export
layer_bb <- function(.cond = NULL,
                     color = "red",
                     alpha = 0.9,
                     linetype = "solid",
                     linewidth = 0.5,
                     expand = as_abs(0.5),
                     label = TRUE,
                     slices = NULL,
                     .by = "slice",
                     ...){

  .cond_quo <- rlang::enquo(.cond)

  # resolve label
  legend_label <- .resolve_legend_label(label, color, rlang::quo_text(.cond_quo))

  vbl_layer(
    fun = function(vbl2D){

      # compute full outlines before subsetting!
      if(.clip_offset(vbl2D)){

        outlines_full <-
          .comp_outlines(
            vbl2D = vbl2D,
            var = NULL,
            concavity = 1,
            use_dbscan = FALSE
          )

      } else {

        outlines_full <- NULL

      }

      layer_str <- glue::glue("layer_bb(..., color = '{color}')")
      vbl2D <- .filter_layer(vbl2D, slices = slices, .cond_quo = .cond_quo, .by = .by, layer_str = layer_str)

      .layer_bb_impl(
        vbl2D = vbl2D,
        alpha = alpha,
        color = color,
        linetype = linetype,
        linewidth = linewidth,
        expand = expand,
        name = names(legend_label),
        outlines_full = outlines_full,
        ...
      )

    },
    legend_label = legend_label,
    class_add = "layer_bb"
  )

}


#' @keywords internal
.layer_bb_impl <- function(vbl2D,
                           alpha,
                           color,
                           linetype,
                           linewidth,
                           expand,
                           name,
                           outlines_full,
                           ...){

  bb_df <- bb2D_df(vbl2D, expand = expand)

  if(.clip_offset(vbl2D)){

    bb_df <- .clip_offset_bb(vbl2D = vbl2D, bb_df = bb_df, outlines_full = outlines_full)

  }

  .layer_lst_bb(
    data = bb_df,
    name = name,
    alpha = alpha,
    color = color,
    linetype = linetype,
    linewidth = linewidth,
    clip_offset = .clip_offset(vbl2D),
    ...
  )

}

#' @rdname layer_bb
#' @export
layer_bb_data <- function(color = "orange",
                          alpha = 0.9,
                          fill = NA,
                          linetype = "solid",
                          linewidth = 0.5,
                          label = TRUE,
                          slices = NULL,
                          ...){

  legend_label <- .resolve_legend_label(label, color, "Data")

  vbl_layer(
    fun = function(vbl2D){

      vbl2D <- .filter_layer(vbl2D, slices = slices, layer_str = "layer_bb_data()")

      .layer_bb_data_impl(
        vbl2D = vbl2D,
        alpha = alpha,
        color = color,
        fill = fill,
        linetype = linetype,
        linewidth = linewidth,
        name = names(legend_label),
        ...
      )

    },
    legend_label = legend_label,
    class_add = "layer_bb"
  )

}

#' @keywords internal
.layer_bb_data_impl <- function(vbl2D,
                                alpha,
                                color,
                                fill,
                                linetype,
                                linewidth,
                                name,
                                ...){

  data <-
    purrr::map_df(
      .x = slices(vbl2D),
      .f = function(slice){

        dbb <- data_bb(vbl2D, slice = slice)

        tibble::tibble(
          slice = slice,
          cmin = dbb$col[1],
          cmax = dbb$col[2],
          rmin = dbb$row[1],
          rmax = dbb$row[2]
        )

      }
    )

  .layer_lst_bb(
    data = data,
    name = name,
    alpha = alpha,
    color = color,
    fill = fill,
    linetype = linetype,
    linewidth = linewidth,
    clip_offset = FALSE,
    ...
  )

}


#' @rdname layer_bb
#' @export
layer_bb_plot <- function(color = "forestgreen",
                          alpha = 0.9,
                          fill = NA,
                          linetype = "solid",
                          linewidth = 0.5,
                          label = TRUE,
                          ...){

  # resolve label
  legend_label <- .resolve_legend_label(label, color, "Plot")

  vbl_layer(
    fun = function(vbl2D){

      .layer_bb_plot_impl(
        vbl2D = vbl2D,
        alpha = alpha,
        color = color,
        fill = fill,
        linetype = linetype,
        linewidth = linewidth,
        name = names(color_nm),
        ...
      )

    },
    color_nm = color_nm,
    class_add = "layer_bb"
  )

}

#' @keywords internal
.layer_bb_plot_impl <- function(vbl2D,
                                alpha,
                                color,
                                fill,
                                linetype,
                                linewidth,
                                name,
                                ...){

  data <-
    plot_bb(vbl2D) %>%
    as_bb2D_df()

  .layer_lst_bb(
    data = data,
    name = name,
    alpha = alpha,
    color = color,
    fill = fill,
    linetype = linetype,
    linewidth = linewidth,
    clip_offset = FALSE,
    ...
  )

}

#' @rdname layer_bb
#' @export
layer_bb_screen <- function(color = "steelblue",
                            alpha = 0.9,
                            fill = NA,
                            linetype = "solid",
                            linewidth = 0.5,
                            label = TRUE,
                            slices = NULL,
                            ...){

  legend_label <- .resolve_legend_label(label, color, "Screen")

  vbl_layer(
    fun = function(vbl2D){

      vbl2D <- .filter_layer(vbl2D, slices = slices, layer_str = "layer_bb_screen()")

      .layer_bb_screen_impl(
        vbl2D = vbl2D,
        alpha = alpha,
        color = color,
        fill = fill,
        linetype = linetype,
        linewidth = linewidth,
        name = names(legend_label),
        ...
      )

    },
    legend_label = legend_label,
    class_add = "layer_bb"
  )

}

#' @keywords internal
.layer_bb_screen_impl <- function(vbl2D,
                                  alpha,
                                  color,
                                  fill,
                                  linetype,
                                  linewidth,
                                  name,
                                  ...){

  data <-
    purrr::map_df(
      .x = slices(vbl2D),
      .f = function(slice){

        sbb <- screen_bb(vbl2D, slice = slice)

        tibble::tibble(
          slice = slice,
          cmin = sbb$col[1],
          cmax = sbb$col[2],
          rmin = sbb$row[1],
          rmax = sbb$row[2]
        )

      }
    )

  .layer_lst_bb(
    data = data,
    name = name,
    alpha = alpha,
    color = color,
    fill = fill,
    linetype = linetype,
    linewidth = linewidth,
    ...
  )

}

#' @rdname layer_bb
#' @export
layer_bb_slice <- function(color = "purple",
                           alpha = 0.9,
                           fill = NA,
                           linetype = "solid",
                           linewidth = 0.5,
                           label = TRUE,
                           slices = NULL,
                           ...){

  legend_label <- .resolve_legend_label(label, color, "Slice")

  vbl_layer(
    fun = function(vbl2D){

      vbl2D <- .filter_layer(vbl2D, slices = slices, layer_str = "layer_bb_slice()")

      .layer_bb_slice_impl(
        vbl2D = vbl2D,
        alpha = alpha,
        color = color,
        fill = fill,
        name = names(legend_label),
        linetype = linetype,
        linewidth = linewidth,
        ...
      )

    },
    legend_label = legend_label,
    class_add = "layer_bb"
  )

}

#' @keywords internal
.layer_bb_slice_impl <- function(vbl2D,
                                 alpha,
                                 color,
                                 fill,
                                 linetype,
                                 linewidth,
                                 name,
                                 ...){

  data <-
    purrr::map_df(
      .x = slices(vbl2D),
      .f = function(slice){

        sbb <- slice_bb(vbl2D, slice = slice)

        tibble::tibble(
          slice = slice,
          cmin = sbb$col[1],
          cmax = sbb$col[2],
          rmin = sbb$row[1],
          rmax = sbb$row[2]
        )

      }
    )

  .layer_lst_bb(
    data = data,
    name = name,
    alpha = alpha,
    color = color,
    fill = fill,
    linetype = linetype,
    linewidth = linewidth,
    clip_offset = FALSE,
    ...
    )

}


#' @title Add a color layer for categorical variables
#' @description Overlay a categorical label variable on a `ggplane()` plot by
#' filling voxels with discrete colors.
#'
#' @param var Character. The name of a factor-variable with categorical labels.
#' If a logical (mask) variable is specified, it is treated as a categorical one
#' with labels = c('TRUE', 'FALSE').
#' @param ... Additional arguments passed to \link{scale_fill_categorical}().
#'
#' @inherit vbl_doc_layer params return
#'
#' @inheritParams vbl_doc_var_categorical
#' @inheritParams vbl_doc
#' @export
layer_categorical <- function(var,
                              clrp = "hue_pal",
                              clrp_adjust = NULL,
                              opacity = 0.25,
                              guide = "drop",
                              slices = NULL,
                              .cond = NULL,
                              .by = "slice",
                              ...){

  opacity_quo <- rlang::enquo(opacity)
  .cond_quo <- rlang::enquo(.cond)

  vbl_layer(
    fun = function(vbl2D){

      layer_str <- glue::glue("layer_categorical(var = '{var}', ...")
      vbl2D <- .filter_layer(vbl2D, slices = slices, .cond_quo = .cond_quo, .by = .by, layer_str = layer_str)

      .layer_categorical_impl(
        vbl2D = vbl2D,
        var = var,
        clrp = clrp,
        clrp_adjust = clrp_adjust,
        opacity = opacity_quo,
        guide = guide,
        ...
      )

    },
    class_add = "layer_raster"
  )

}


#' @keywords internal
.layer_categorical_impl <- function(vbl2D,
                                    var,
                                    clrp,
                                    clrp_adjust,
                                    opacity,
                                    guide,
                                    ...){

  if(is_mask_var(vbl2D[[var]])){

    vbl2D[[var]] <- factor(as.character(vbl2D[[var]]), levels = c("TRUE", "FALSE"))

  }

  if(is.character(vbl2D[[var]])){

    vbl2D[[var]] <- as.factor(vbl2D[[var]])

  }

  var <- .check_input_var(vbl2D, var = var, type = "categorical")

  clr_values <- color_vector(clrp = clrp, clrp_adjust = clrp_adjust, names = levels(vbl2D[[var]]))

  if(is.character(guide) && length(guide) == 1 && guide == "drop"){

    levels_keep <-
      dplyr::filter(vbl2D, visible.) %>%
      droplevels() %>%
      dplyr::pull(var = {{ var }}) %>%
      levels()

    clr_values <- clr_values[levels_keep]

  }

  if(length(clr_values) > 15){

    msg <- c(
      glue::glue("Number of legend entries for `var = {var}` is {length(clr_values)}."),
      i = "Use `guide = 'none'` to omit legend plotting for this layer_categorical()."
    )

    rlang::inform(
      message = msg,
      .frequency = "regularly",
      .frequency_id = "layer.categorical.guide.drop"
      )

  }

  data <- vbl2D[!is.na(vbl2D[[var]]) & vbl2D$visible., ]

  list(
    ggnewscale::new_scale_fill(),
    ggplot2::geom_raster(
      data = data,
      mapping = ggplot2::aes(x = col, y = row, fill = .data[[var]]),
      alpha = .eval_tidy_opacity(data, opacity = opacity, var = var),
      interpolate = vbl_opts("interpolate")
    ),
    ggplot2::scale_fill_manual(values = clr_values, guide = guide, ...)
  )

}



#' @title Crop a ggplane plot to a filtered voxel extent
#' @description
#' Crops the plot extent to the bounding box of voxels selected by `.cond`.
#'
#' @param .cond A logical filter expression evaluated on `vbl2D` that determines
#' the voxels used to define the cropped plot extent.
#' @param expand Numeric. Expansion factor passed to `expand_bb2D()` to grow the
#' cropped plot extent.
#'
#' @inherit vbl_doc_layer params return
#'
#' @details
#' The layer filters `vbl2D` via `.cond` and then sets plot limits using
#' `ggplot2::coord_equal()` based on the resulting voxel extent.
#'
#' @examples
#' vbl <- example_vbl()
#'
#' ggplane(vbl, var = "t1", plane = "axi", slices = 100) +
#'   layer_crop(brain, expand = 0.05)
#'
#' @export
layer_crop <- function(.cond, expand = FALSE, ...){

  .cond_quo <- rlang::enquo(.cond)

  vbl_layer(
    fun = function(vbl2D){

      vbl2D <- .filter_layer(vbl2D, .cond_quo = .cond_quo, layer = "layer_crop()")

      .layer_crop_impl(
        vbl2D = vbl2D,
        expand = expand,
        ...
      )

    },
    class_add = "layer_crop"
  )

}

#' @keywords internal
.layer_crop_impl <- function(vbl2D, expand, ...){

  plot_lim <-
    list(
      col = range(vbl2D$col),
      row = range(vbl2D$row)
    ) %>%
    expand_bb2D(expand = expand)

  layer_lst <-
    list(
      ggplot2::coord_equal(
        ratio = 1,
        xlim = plot_lim$col,
        ylim = rev(plot_lim$row),
        expand = FALSE
      ),
      ...
    )

  layer_lst[[1]]$default <- TRUE

  return(layer_lst)

}


#' @title Add grid lines to a ggvibble plot
#' @description Draw vertical and/or horizontal grid lines at regular positions
#' across slices of a `vbl2D` object.
#'
#' @param col Numeric or `NULL`. Controls vertical grid lines:
#' \itemize{
#'   \item `length(col) > 1`: values are used directly as col-intercepts.
#'   \item `length(col) == 1` and `col < 1`: interpreted as a proportion of
#'   the maximal `col` of the \link[=plot_bb]{plot bounding box.}
#'   Grid lines are placed symmetrically around the center (half the maximum) with that spacing.
#'   \item `length(col) == 1` and `col >= 1`: interpreted as an absolute
#'   spacing in coordinate units, again used to place symmetric lines around
#'   the center.
#'   \item `NULL` or length 0: no vertical grid lines are drawn.
#' }
#'
#' @param row Numeric or `NULL`. Controls horizontal grid lines. See `col` for
#' details.
#' @param alpha,color,linewidth,linetype \link[=vbl_doc_plot_aesthetics]{Aesthetic settings}
#' for grid appearance. Each can be supplied either as a single value or as a vector of
#' length 2:
#' \itemize{
#'   \item \strong{Length 1}: The value is applied to both axes (vertical and
#'   horizontal grid lines).
#'   \item \strong{Length 2}: The first element is applied to vertical grid
#'   lines and the second element to horizontal grid lines.
#' }
#'
#' @inherit vbl_doc_layer return
#'
#' @export
layer_grid <- function(col = 0.1,
                       row = 0.1,
                       alpha = 0.15,
                       color = "lightgrey",
                       linewidth = 0.25,
                       linetype = "solid"
                       ){

  vbl_layer(
    fun = function(vbl2D){

      .layer_grid_impl(
        vbl2D = vbl2D,
        col = col,
        row = row,
        alpha = alpha,
        color = color,
        linetype = linetype,
        linewidth = linewidth
      )

    },
    class_add = "layer_ann"
  )

}

#' @export
.layer_grid_impl <- function(vbl2D,
                             ref_bb,
                             col,
                             row,
                             alpha,
                             color,
                             linetype,
                             linewidth
                             ){

  style_lst <-
    list(alpha = alpha, color = color, linetype = linetype, linewidth = linewidth) %>%
    purrr::map(.f = ~ if(length(.x) == 1){ rep(.x, 2) } else { .x } )

  data <-
    tidyr::expand_grid(
      slice = if(!is_offset(vbl2D)){ slices(vbl2D) },
      col = .grid_intercepts(col, limits = plot_bb(vbl2D)$col),
      row = .grid_intercepts(row, limits = plot_bb(vbl2D)$row)
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



#' @title Add label annotations to categorical regions
#' @description
#' Computes slice-wise centroid positions of categorical label regions and adds
#' text annotations for each label.
#'
#' @param var Character. Name of a categorical variable from which to draw the labels.
#' @param include Optional character vector of labels to include. If named the
#' labels are renamed according to `c(<orig label> = <displayed label>)`.
#' @param exclude Optional character vector of labels to exclude.
#' @param alpha Text alpha (transparency) passed to `geom_text()`.
#' @param color Text color passed to `geom_text()`.
#' @param size Text size passed to `geom_text()`.
#' @param use_dbscan Logical. If `TRUE` (default), label voxels within each slice
#' are clustered using `dbscan2D()` before centroid computation; otherwise all
#' voxels of a label form a single region.
#' @param centroid The function with which to compute the centroid label position
#' based on col and row. A named list of functions can be supplied, where names
#' must be *c('col', 'row')*.
#' @param abbrev Optional function or formula applied to label names for abbreviation.
#' Should take a and return a character scalar.
#' @param repel Logical. If `TRUE`, the text is rendered with \link{geom_text_repel}(),
#' which prevents label overlap. If `FALSE`, text ist rendered with \link{geom_text}().
#' @param ... Additional arguments passed to `geom_text()` or `geom_text_repel()`.
#'
#' @inherit vbl_doc_layer params return
#'
#' @details
#' The function identifies, per slice, all occurrences of the categorical variable
#' specified in `var`. Optionally, label sets can be restricted via `include`
#' and/or filtered out via `exclude`. If `use_dbscan = TRUE`, voxels belonging to
#' the same label are partitioned into spatial clusters using `dbscan2D()`, and
#' centroids are computed for each cluster to avoid assigning a single label
#' position to widely separated regions. If `use_dbscan = FALSE`, all voxels of a
#' label within a slice are treated as one region. The resulting centroids are
#' rendered using `geom_text()`. A user-supplied function in `abbrev` can modify
#' displayed label text (e.g., shortening region names).
#'
#' @export

layer_labels <- function(var,
                         include = NULL,
                         exclude = NULL,
                         alpha = 0.9,
                         color = "white",
                         size = 3.5,
                         use_dbscan = TRUE,
                         centroid = median,
                         abbrev = NULL,
                         repel = TRUE,
                         slices = NULL,
                         .cond = NULL,
                         .by = "slice",
                         ...){

  .cond_quo <- rlang::enquo(.cond)

  vbl_layer(
    fun = function(vbl2D){

      layer_str <- glue::glue("layer_labels(var = '{var}', ...")
      vbl2D <- .filter_layer(vbl2D, slices = slices, .cond_quo = .cond_quo, .by = .by, layer_str = layer_str)

      .layer_labels_impl(
        vbl2D = vbl2D,
        var = var,
        include = include,
        exclude = exclude,
        alpha = alpha,
        color = color,
        size = size,
        use_dbscan = use_dbscan,
        centroid = centroid,
        abbrev = abbrev,
        repel = repel,
        ...
      )

    },
    class_add = "layer_ann"
  )

}

#' @keywords internal
.layer_labels_impl <- function(vbl2D,
                               var,
                               include,
                               exclude,
                               alpha,
                               color,
                               size,
                               use_dbscan,
                               centroid,
                               abbrev,
                               repel,
                               ...){

  var <- .check_input_var(vbl2D, var = var, type = "categorical")

  if(!is.list(centroid)){

    stopifnot(is.function(centroid))
    centroid <- list(col = centroid, row = centroid)

  } else {

    stopifnot(all(c("col", "row") %in% names(centroid)))
    stopifnot(all(purrr::map_lgl(centroid, is.function)))

  }

  if(.clip_offset(vbl2D)){

    vbl2D <- .clip_offset_raster(vbl2D)

  }

  labels <- levels(vbl2D[[var]])

  if(is.character(include)){

    if(.is_named(include)){

      labels <- labels[labels %in% names(include)]

    } else {

      labels <- labels[labels %in% include]

    }

  }

  if(is.character(exclude)){ labels <- labels[!labels %in% exclude] }

  data <-
    purrr::map_df(
      .x = slices(vbl2D),
      .f = function(slice){

        slice_df <- dplyr::filter(vbl2D, slice == {{slice}})

        purrr::map_df(
          .x = labels[labels %in% slice_df[[var]]],
          .f = function(label){

            label_df <- dplyr::filter(slice_df, !!rlang::sym(var) == {{ label }})

            if(isTRUE(use_dbscan)){

              label_df <-
                dbscan2D(
                  slice_df = label_df,
                  var_out = "lbl.idx",
                  pref_out = "lbl.",
                  minPts = 6,
                  min_size = 0.33,
                  rm_outlier = TRUE
                )

            } else {

              label_df$lbl.idx <- "1"

            }

            dplyr::summarize(
              .data = label_df,
              .by = "lbl.idx",
              col = centroid[["col"]](col),
              row = centroid[["row"]](row),
              {{var}} := {{ label }}
            )

          }
        ) %>%
          dplyr::mutate(slice = {{ slice }})

      }
    )

  data[[var]] <- as.character(data[[var]])

  if(is.character(include) && .is_named(include)){

    renamed <- unname(include[data[[var]]])

    data[[var]] <- dplyr::if_else(is.na(renamed), data[[var]], renamed)

  }

  if(is.function(abbrev) | purrr::is_formula(abbrev)){

    data <-
      dplyr::mutate(
        .data = data,
        dplyr::across(
          .cols = {{ var }},
          .fns = abbrev
        )
      )

  }

  geom_use <- ifelse(repel, ggrepel::geom_text_repel, ggplot2::geom_text)

  list(
    geom_use(
      data = data,
      mapping = ggplot2::aes(x = col, y = row, label = .data[[var]]),
      alpha = alpha,
      color = color,
      size = size,
      ...
    )
  )

}

#' @title Modify plot labels and theme.
#' @description
#' Adds plot-level annotations and appearance settings to a \code{ggvibble} plot.
#'
#' @param ... Arguments passed directly to \link{ggplot2::labs}() or
#' \link{ggplot2::theme}(), depending on the layer.
#'
#' @inherit vbl_doc_layer return
#'
#' @details
#' These layers apply globally and are independent of slice content, layout, or
#' faceting. They are evaluated once on the resulting \code{ggplot} object and are
#' therefore compatible with all \code{ggvibble} layouts.
#'
#' @export
layer_labs <- function(...){

  vbl_layer(
    fun = function(vbl2D){

      list(
        ggplot2::labs(
          ...
        )
      )

    },
    class_add = "layer_misc"
  )


}

#' @title Add a masking layer
#' @description Overlay a logical mask on a \link{ggplane}() plot by filling
#' voxels.
#'
#' @param color Character scalar. The \link[=is_color]{color} used for
#' the mask.
#'
#' @details
#' The condition of `.cond` determines which voxels are included in the mask.
#' If no condition is provided (`.cond = NULL`) this layer masks every voxel in
#' every slice of the 2D vibble passed to it by `ggplane()`.
#'
#' @inherit vbl_doc_layer params return
#' @inheritParams vbl_doc
#'
#' @export
layer_mask <- function(.cond = NULL,
                       color = "red",
                       opacity = 0.25,
                       label = TRUE,
                       slices = NULL,
                       .by = "slice",
                       ...){

  .cond_quo <- rlang::enquo(.cond)
  opacity_quo <- rlang::enquo(opacity)

  legend_label <- .resolve_legend_label(label, color, rlang::quo_text(.cond_quo))

  vbl_layer(
    fun = function(vbl2D){

      layer_str <- glue::glue("layer_mask(..., color = '{color}')")
      vbl2D <- .filter_layer(vbl2D, slices = slices, .cond_quo = .cond_quo, .by = .by, layer_str = layer_str)

      .layer_mask_impl(
        vbl2D = vbl2D,
        color = color,
        opacity = opacity_quo,
        name = names(legend_label),
        ...
      )

    },
    legend_label = legend_label,
    class_add = "layer_mask"
  )

}

#' @keywords internal
.layer_mask_impl <- function(vbl2D,
                             color,
                             opacity,
                             name,
                             ...){

  data <- vbl2D[vbl2D$visible.,]

  if(is.character(name)){

    layer_lst <-
      list(
        ggplot2::geom_raster(
          data = dplyr::mutate(data, mask. = {{ name }}),
          mapping = ggplot2::aes(x = col, y = row, fill = mask.),
          alpha = .eval_tidy_opacity(data, opacity = opacity, var = var),
          interpolate = vbl_opts("interpolate")
        )
      )

  } else {

    layer_lst <-
      list(
        ggplot2::geom_raster(
          data = data,
          mapping = ggplot2::aes(x = col, y = row),
          alpha = .eval_tidy_opacity(data, opacity = opacity, var = var),
          fill = color,
          interpolate = vbl_opts("interpolate")
        )
      )

  }

  return(layer_lst)

}



#' @title Add ggplot2 components via a ggvibble layer
#'
#' @description
#' Collects ggplot2 objects supplied via `...` and injects them into the ggvibble
#' plotting pipeline as a single ggvibble layer.
#'
#' @details
#' `layer_misc()` provides a controlled interoperability mechanism with ggplot2.
#' Objects passed via `...` are filtered to ggplot-compatible objects (class
#' containing `"gg"`) and added during plot materialization.
#'
#' This avoids direct use of `+` with ggplot2 components, which can lead to
#' ambiguous operator dispatch.
#'
#' @param ... ggplot2 objects such as themes, annotations, scales, or coordinates.
#'
#' @note `gg` objects that inherit *'Facet'* are not allowed and discarded.
#'
#' @inherit vbl_doc_layer return
#'
#' @export
layer_misc <- function(...){

  input <- list(...)

  vbl_layer(
    fun = function(vbl2D){

      purrr::keep(
        .x = input,
        .p = ~ "gg" %in% class(.x)
      ) %>%
      purrr::discard(
        .x = .,
        .p = ~ "Facet" %in% class(.x)
      )

    },
    class_add = "layer_misc"
  )

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
#' @export
layer_numeric <- function(var,
                          clrsp,
                          opacity = 0.25,
                          slices = NULL,
                          .cond = NULL,
                          .by = "slice",
                          ...){

  .cond_quo <- rlang::enquo(.cond)
  opacity_quo <- rlang::enquo(opacity)

  vbl_layer(
    fun = function(vbl2D){

      layer_str <- glue::glue("layer_numeric(var = '{var}', ...)")
      vbl2D <- .filter_layer(vbl2D, slices = slices, .cond_quo = .cond_quo, .by = .by, layer_str = layer_str)

      .layer_numeric_impl(
        vbl2D = vbl2D,
        var = var,
        clrsp = clrsp,
        opacity = opacity_quo,
        ...
      )

    },
    class_add = "layer_raster"
  )

}

#' @keywords internal
.layer_numeric_impl <- function(vbl2D,
                                var,
                                clrsp,
                                opacity,
                                ...){

  var <- .check_input_var(vbl2D, var = var, type = "numeric")

  data <- vbl2D[vbl2D$visible.,]

  list(
    ggnewscale::new_scale_fill(),
    ggplot2::geom_raster(
      data = data,
      mapping = ggplot2::aes(x = col, y = row, fill = .data[[var]]),
      alpha = .eval_tidy_opacity(data, opacity = opacity, var = var),
      interpolate = vbl_opts("interpolate")
    ),
    scale_fill_numeric(
      clrsp,
      limits = .get_var_limits(data, var),
      ...
    )
  )

}



#' @title Add orientation labels
#' @description
#' Adds per-slice orientation labels (e.g., L/R, A/P, S/I) to a `ggplane()`.
#'
#' @param col Logical. If \code{TRUE}, annotate the column axis orientation.
#' @param row Logical. If \code{TRUE}, annotate the row axis orientation.
#' @param ref_bb Character scalar. Reference bounding box used to position the labels.
#' Valid values are *c("data", "plot", "screen", "slice")*.
#' If \code{vbl_def()}, defaults to *'plot'* for offset layouts and *'screen'* otherwise.
#' @param alpha,color,size Passed to \link{geom_text()} to define the text \link[=vbl_doc_plot_aesthetics]{aesthetics}.
#' @param ... Additional arguments passed to `geom_text()`
#'
#' @inherit vbl_doc_layer return
#'
#' @details
#' The orientation labels are derived from \code{ccs_orientation_mapping} for the current plane.
#' For each requested axis (\code{col} and/or \code{row}), two labels are created (min/max) and
#' anchored to the corresponding side of the selected reference bounding box.
#'
#' Label positions are computed per slice. For each slice, the reference bounding box is obtained
#' via \code{.ref_bb(..., type = ref_bb, slice = slice)} and converted to absolute image coordinates
#' using \code{as_img_anchor_abs()}. The labels are drawn with \code{ggplot2::geom_text()}.
#'
#' @export

layer_orientation <- function(col = TRUE,
                              row = TRUE,
                              ref_bb = vbl_def(),
                              alpha = 0.5,
                              color = "white",
                              size = 3.5,
                              ...){

  col <- isTRUE(col[1])
  row <- isTRUE(row[1])

  if(!any(c(col, row))){

    msg <- "At least one of `col` or `row` must be TRUE."
    rlang::abort(msg)

  }

  vbl_layer(
    fun = function(vbl2D){

      .layer_orientation_impl(
        vbl2D = vbl2D,
        col = col,
        row = row,
        ref_bb = ref_bb,
        alpha = alpha,
        color = color,
        size = size,
        ...
      )

    },
    class_add = "layer_ann"
  )


}

#' @keywords internal
.layer_orientation_impl <- function(vbl2D,
                                    col,
                                    row,
                                    ref_bb,
                                    alpha,
                                    color,
                                    size,
                                    ...){

  if(.is_vbl_def(ref_bb)){

    ref_bb <- ifelse(is_offset(vbl2D), "plot", "screen")

  }

  plane <- plane(vbl2D)
  axes <- req_axes_2D(plane)[c("col", "row")]
  axes <- axes[c(col, row)]

  mapping <-
    t(as.data.frame(ccs_orientation_mapping)) %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "ccs") %>%
    magrittr::set_colnames(value = c("ccs", "min", "max")) %>%
    dplyr::filter(ccs %in% {{ axes }}) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(c("min", "max")),
      values_to = "label",
      names_to = "or"
    )

  mapping$axis <- ""
  mapping$anchor <- ""
  for(i in 1:nrow(mapping)){

    mapping$axis[i] <- names(axes)[axes==mapping$ccs[i]]

    if(mapping$axis[i] == "col"){

      mapping$anchor[i] <- ifelse(mapping$or[i] == "min", "left", "right")

    } else if(mapping$axis[i] == "row"){

      mapping$anchor[i] <- ifelse(mapping$or[i] == "min", "top", "bottom")

    }

  }

  text_df <-
    purrr::map_df(
      .x = slices(vbl2D),
      .f = function(slice){

          bb <- .ref_bb(vbl2D, type = ref_bb, slice = slice)


          pos_df <-
            purrr::map_df(
              .x = 1:nrow(mapping),
              .f = ~ as_img_anchor_abs(anchor = mapping$anchor[.x], bb2D = bb)
            ) %>%
            dplyr::mutate(slice = {{ slice }})

          cbind(mapping, pos_df)

      }
    )

  # output
  list(
    ggplot2::geom_text(
      data = text_df,
      mapping = ggplot2::aes(x = col, y = row, label = label),
      alpha = alpha,
      color = color,
      size = size,
      ...
    )
  )


}



#' @title Add outlines
#' @description Overlay a \link{ggplane}() plot by outlining voxels
#' that match a certain condition.
#'
#' @param alpha Numeric. Controls the transparency of the lines.
#' @param use_dbscan Logical. If `TRUE`, for every slice \link[dbscan:dbscan]{DBSCAN}
#' is used to identify spatial islands of voxels matching the condition which
#' are then outlined separately.
#'
#' @details
#' The condition of `.cond` determines which voxels are outlined. If no condition
#' is provided (`.cond = NULL`) this layer outlines all voxels in every slice of the
#' 2D vibble passed to this layer by `ggplane()`.
#'
#' Outlines are **always** computed slicewise.
#'
#' @inherit vbl_doc_layer params return
#' @inheritParams vbl_doc
#'
#' @export
layer_outline <- function(.cond = NULL,
                          color = "gold",
                          fill = NA,
                          linetype = "solid",
                          linewidth = 0.5,
                          use_dbscan = TRUE,
                          concavity = 1.5,
                          label = TRUE,
                          slices = NULL,
                          .by = "slice",
                          ...){

  .cond_quo <- rlang::enquo(.cond)

  legend_label <- .resolve_legend_label(label, color, rlang::quo_text(.cond_quo))

  vbl_layer(
    fun = function(vbl2D){

      # compute full outlines before subsetting!
      if(.clip_offset(vbl2D)){

        outlines_full <-
          .comp_outlines(
            vbl2D = vbl2D,
            var = NULL,
            concavity = 1,
            use_dbscan = FALSE
          )

      } else {

        outlines_full <- NULL

      }

      layer_str <- glue::glue("layer_outline(..., color = '{color}')")
      vbl2D <- .filter_layer(vbl2D, slices = slices, .cond_quo = .cond_quo, .by = .by, layer_str = layer_str)

      .layer_outline_impl(
        vbl2D = vbl2D,
        color = color,
        fill = fill,
        linetype = linetype,
        linewidth = linewidth,
        use_dbscan = use_dbscan,
        concavity = concavity,
        outlines_full = outlines_full,
        name = names(legend_label),
        ...
      )

    },
    legend_label = legend_label,
    class = "layer_outline"
  )

}

#' @keywords internal
.layer_outline_impl <- function(vbl2D,
                                color,
                                fill,
                                linetype,
                                linewidth,
                                concavity,
                                use_dbscan,
                                outlines_full,
                                name,
                                ...){


  # create outlines on the input vbl2D
  # which was filtered by .cond/.by in layer_outline()!
  outlines <-
    .comp_outlines(
      vbl2D = vbl2D,
      var = NULL,
      concavity = concavity,
      use_dbscan = use_dbscan
      )

  if(.clip_offset(vbl2D)){

    outlines <-
      .clip_offset_outlines(
        vbl2D = vbl2D,
        outlines = outlines,
        outlines_full = outlines_full
        )

  }

  # output
  layer_lst <- list()

  if(any(outlines$split)){

    data <-
      dplyr::filter(outlines, split) %>%
      dplyr::mutate(outline = stringr::str_c(outline, slice, part, sep = "."))

    if(is.character(name)){

      layer_lst$path <-
        ggplot2::geom_path(
          data = dplyr::mutate(data, outline. = {{ name }}),
          mapping = ggplot2::aes(x = col, y = row, group = outline, color = outline.),
          linetype = linetype,
          linewidth = linewidth
        )

    } else {

      layer_lst$path <-
        ggplot2::geom_path(
          data = data,
          mapping = ggplot2::aes(x = col, y = row, group = outline),
          color = color,
          linetype = linetype,
          linewidth = linewidth
        )

    }

  }

  if(any(!outlines$split)){

    data <-
      dplyr::filter(outlines, !split) %>%
      dplyr::mutate(outline = stringr::str_c(outline, slice, part, sep = "."))

    if(is.character(name)){

      layer_lst$polygon <-
        ggplot2::geom_polygon(
          data = dplyr::mutate(data, outline. = {{ name }}),
          mapping = ggplot2::aes(x = col, y = row, group = outline, color = outline.),
          linetype = linetype,
          linewidth = linewidth,
          fill = fill,
          ...
        )

    } else {

      layer_lst$polygon <-
        ggplot2::geom_polygon(
          data = data,
          mapping = ggplot2::aes(x = col, y = row, group = outline),
          color = color,
          linetype = linetype,
          linewidth = linewidth,
          fill = fill,
          ...
        )

    }

  }

  return(layer_lst)

}


#' @title Add slice numbers
#' @description Add slice numbers as text labels to a \link{ggplane()}-plot.
#'
#' @param anchor \link[=vbl_doc_img_anchor]{Image anchor} specification for
#' positioning the slice labels. If, character one of *c('center', 'top', 'bottom', 'left', 'right')*.
#' Absolute anchors are not allowed in offset-layouts.
#' @param ref_bb Character scalar indicating which \link[=vbl_doc_ref_bb]{2D reference bounding box}
#' to use when anchoring slice numbers via relative or character image anchors:
#'   \itemize{
#'     \item \code{"data"}: uses global bounding boxes from \link{data_bb}().
#'     \item \code{"slice"}: uses per-slice bounding boxes from \link{slice_bb}().
#'     \item \code{"screen"}: uses screen-space bounding boxes from \link{screen_bb}().
#'   }
#' @param wrap Optional template for label text. Defaults to \code{"{slice}"}.
#' @param angle Rotation angle passed to \code{geom_text()}.
#' @param alpha,color,size Passed to \code{geom_text()} to define the text aesthetics.
#' @param ... Additional arguments passed to \code{geom_text()}.
#'
#' @inherit vbl_doc_layer return
#'
#' @export

layer_slice_numbers <- function(anchor = vbl_def(),
                                ref_bb = "screen",
                                wrap = "{slice}",
                                angle = 0,
                                alpha = 0.9,
                                color = "white",
                                size = 3.5,
                                ...){

  vbl_layer(
    fun = function(vbl2D){

      .layer_slice_numbers_impl(
        vbl2D = vbl2D,
        anchor = anchor,
        ref_bb = ref_bb,
        wrap = wrap,
        angle = angle,
        alpha = alpha,
        color = color,
        size = size,
        ...
      )

    },
    class_add = "layer_ann"
  )

}

#' @keywords internal
.layer_slice_numbers_impl <- function(vbl2D,
                                      anchor,
                                      ref_bb,
                                      wrap,
                                      angle,
                                      alpha,
                                      color,
                                      size,
                                      ...){

  stopifnot(within_limits(angle, c(0, 360)))

  ref_bb <- .match_arg(ref_bb, choices = c("data", "screen", "slice"))

  # anchor sanity checks and conversion to anchor as a relative image anchor
  if(.is_vbl_def(anchor)){

    anchor <-
      dplyr::case_when(
        !is_offset(vbl2D) ~ "top",
        abs(offset_col(vbl2D)) > abs(offset_row(vbl2D)) ~ "top",
        TRUE ~ "left"
      )

  } else {

    if(is_offset(vbl2D) && is_img_anchor_abs(vbl2D)){

      stop("Need relative or character imgage anchor for `anchor` in offset-layouts.")

    }

    if(is.character(anchor)){

      anchor <- .match_arg(anchor, choices = names(img_anchors))

    }

    .stop_if_not(is_img_anchor(anchor))

  }


  if(is_img_anchor_chr(anchor)){

    anchor <- img_anchors[[anchor]]

  }

  wrap <- ifelse(is.character(wrap), wrap[1], "{slice}")

  df <-
    purrr::map_df(
      .x = slices(vbl2D),
      .f = function(slice){

        if(ref_bb == "data"){

          bb2D <- data_bb(vbl2D, slice)

        } else if(ref_bb == "screen"){

          bb2D <- screen_bb(vbl2D, slice)

        } else if(ref_bb == "slice"){

          bb2D <- slice_bb(vbl2D, slice)

        }

        as_img_anchor_abs(anchor, bb2D = bb2D)

      }
    ) %>%
    dplyr::mutate(
      slice = slices(vbl2D),
      label = as.character(glue::glue(wrap))
    )

  list(
    ggplot2::geom_text(
      data = df,
      mapping = ggplot2::aes(x = col, y = row, label = label),
      alpha = alpha,
      angle = angle,
      color = color,
      size = size,
      ...
    )
  )

}


#' @title Add orthogonal slice references
#' @description
#' Add projection lines and slice labels indicating the positions of slices from
#' another anatomical plane.
#'
#' @param alpha Numeric vector of length one or two specifying alpha for
#'   the line and labels (recycled if length one).
#' @param color Character vector of length one or two specifying colors for
#'   the line and labels (recycled if length one).
#' @param plane_proj Character scalar specifying the anatomical plane of the
#' projected slices. Must differ from the plane used in \link{ggplane}().
#' @param slices_proj Integer vector giving the slice indices to display as projection lines.
#' @param ref_bb Character scalar indicating which \link[=vbl_doc_ref_bb]{2D reference bounding box}
#' to use when spanning the projection lines and anchoring slice labels:
#'   \itemize{
#'     \item \code{"data"}: uses global bounding boxes from \link{data_bb}().
#'     \item \code{"slice"}: uses per-slice bounding boxes from \link{slice_bb}().
#'     \item \code{"screen"}: uses screen-space bounding boxes from \link{screen_bb}().
#'   }
#' @param label_pos Character, logical, or `vbl_def()`. Controls the position of the label
#' (slice number) relative to the projection line.
#'
#'   \itemize{
#'     \item{character: } One of *c("top", "bottom", "left", "right",
#'       "top-left", "top-right", "bottom-left", "bottom-right")*.
#'     \item{`vbl_def()`: } Internal default based on the projection axis.
#'     \item{`FALSE`: } Do not draw labels.
#'   }
#'
#' @param label_just Numeric vector of length two or `vbl_def()`. Text justification
#'   for labels, passed to `geom_text()` as `hjust` and `vjust`.
#'
#'   \itemize{
#'     \item{`vbl_def()`: } Compute justification automatically from `label_pos`,
#'       the projection axis, and `spacer`.
#'     \item{Numeric: } Explicit justification given as *c(hjust, vjust)*.
#'   }
#' @param linewidth Numeric line width passed to \code{geom_segment()}.
#' @param size Numeric text size passed to \code{geom_text()}.
#' @param label_spacer Numeric scalar. Controls the offset used when computing default
#' text justification for slice projection labels. Larger values move labels further away
#' from the projection line.
#' @param ... Additional arguments forwarded to \code{geom_text()}.
#'
#' @inherit vbl_doc_layer params return
#'
#' @note Label alignment (\code{hjust}/\code{vjust}) is determined
#' automatically from \code{label_just}.
#'
#' @export

layer_slice_projections <- function(slices_proj,
                                    plane_proj,
                                    ref_bb = "data",
                                    alpha = 0.9,
                                    color = "red",
                                    size = 3.5,
                                    linetype = "solid",
                                    linewidth = 0.5,
                                    label_pos = vbl_def(),
                                    label_just = vbl_def(),
                                    label_spacer = vbl_def(),
                                    slices = NULL,
                                    ...){

  vbl_layer(
    fun = function(vbl2D){

      if(plane_proj == plane(vbl2D)){

        .glue_stop("`layer_slice_projections(plane_proj = '{plane_proj}')` is not possible with `ggplane(..., plane = '{plane(vbl2D)}').")

      }

      .layer_slice_projections_impl(
        vbl2D = vbl2D,
        plane_proj = plane_proj,
        slices_proj = slices_proj,
        alpha = alpha,
        color = color,
        size = size,
        linetype = linetype,
        linewidth = linewidth,
        ref_bb = ref_bb,
        label_pos = label_pos,
        label_just = label_just,
        label_spacer = label_spacer,
        slices = .resolve_slices(slices, slices(vbl2D), layer_str = "layer_slice_projections()"),
        ...
      )

    },
    class_add = "layer_ann"
  )

}

#' @keywords internal
.layer_slice_projections_impl <- function(vbl2D,
                                          plane_proj,
                                          slices_proj,
                                          alpha,
                                          color,
                                          size,
                                          linewidth,
                                          linetype,
                                          ref_bb,
                                          label_pos,
                                          label_just,
                                          label_spacer,
                                          slices,
                                          ...){

  vbl2D_proj <- reverse_offset(vbl2D)

  # sanity checks
  if(any(c("hjust", "vjust") %in% names(list(...)))){

    warning("`hjust` and `vjust` are determined for `geom_text()` with `label_just`.")

  }

  ref_bb <- .match_arg(ref_bb, choices = c("data", "screen", "slice"))

  alpha <- if(length(alpha) == 1){ rep(alpha, 2) } else { alpha }
  color <- if(length(color) == 1){ rep(color, 2) } else { color }

  axis_proj <-
    .req_axis_2D_proj(
      plane = plane(vbl2D),
      plane_proj = plane_proj
    )

  layer_lst <- list()

  # line positioning
  line_df <-
    purrr::map_df(
      .x = slices,
      .f = function(slice){

        purrr::map_df(
          .x = slices_proj,
          .f = function(slice_proj){

            if(ref_bb == "data"){

              bb0 <- data_bb(vbl2D_proj, slice = slice)

            } else if(ref_bb == "screen"){

              bb0 <- screen_bb(vbl2D_proj, slice = slice)

            } else if(ref_bb == "slice") {

              bb0 <- slice_bb(vbl2D_proj, slice)

            }

            bb0[[axis_proj]] <- rep(slice_proj, 2)

            bb <- .offset_bb0(vbl2D, slice = slice, bb0 = bb0)

            tibble::tibble(
              cmin = min(bb$col),
              cmax = max(bb$col),
              cmid = ifelse(axis_proj=="row", mid(bb$col), unique(bb$col)),
              rmin = min(bb$row),
              rmax = max(bb$row),
              rmid = ifelse(axis_proj=="col", mid(bb$row), unique(bb$row)),
              slice_proj = slice_proj
            )

          }

        ) %>%
          dplyr::mutate(slice = slice)

      }
    )

  layer_lst[[1]] <-
    ggplot2::geom_segment(
      data = line_df,
      mapping = ggplot2::aes(x = cmin, xend = cmax, y = rmin, yend = rmax),
      alpha = alpha[1],
      color = color[1],
      linetype = linetype,
      linewidth = linewidth
    )

  # text positioning

  # skip if FALSE
  if(isFALSE(label_pos)) return(layer_lst)

  # label positioning
  if(.is_vbl_def(label_pos)){

    label_pos <- ifelse(axis_proj == "col", "top", "left")

  } else {

    .stop_if_not(is.character(label_pos))
    choices <- setdiff(names(img_anchors), "center")
    label_pos <- .match_arg(label_pos, choices = choices)

  }

  col_pos <- .col_pos(axis_proj, label_pos)
  row_pos <- .row_pos(axis_proj, label_pos)

  text_df <-
    dplyr::mutate(
      .data = line_df,
      col = !!rlang::sym(col_pos),
      row = !!rlang::sym(row_pos)
    )

  # label justification
  if(.is_vbl_def(label_just)){

    label_spacer <- .resolve_label_spacer(label_spacer)

    label_just <- .label_just(label_pos, axis_proj, label_spacer)

  } else {

    stopifnot(is.numeric(label_just) && length(label_just) == 2)

  }

  layer_lst[[2]] <-
    ggplot2::geom_text(
      data = text_df,
      mapping = ggplot2::aes(x = col, y = row, label = slice_proj),
      alpha = alpha[2],
      color = color[2],
      size = size,
      hjust = label_just[1],
      vjust = label_just[2],
      ...
    )

  return(layer_lst)

}




#' @title Add text annotations to slices.
#' @description
#' Adds a per-slice text annotation layer using \code{ggplot2::geom_text()}.
#'
#' @param text Text specification. See Details for more information.
#'
#' \itemize{
#'   \item{Character: } Flattened with \code{collapse} and drawn on every slice.
#'   \item{data.frame: } Must contain a \code{slice} column.
#'   \item{Named list of formulas: } Each named formula is evaluated per slice via
#'   \link{summarise}(vbl2D, ..., .by = "slice").
#' }
#'
#' @param sep Character scalar used to separate names and values when \code{text} is a data.frame.
#' @param collapse Character scalar used to collapse multi-line text.
#' @param ref_bb Character scalar indicating which \link[=vbl_doc_ref_bb]{2D reference bounding box}
#' to use when anchoring the text:
#'   \itemize{
#'     \item \code{"data"}: uses global bounding boxes from \link{data_bb}().
#'     \item \code{"screen"}: uses screen-space bounding boxes from \link{screen_bb}().
#'     \item \code{"slice"}: uses per-slice bounding boxes from \link{slice_bb}().
#'     \item \code{"plot"}: uses the plot bounding box rom \link{plot_bb)}().
#'   }
#' @param alpha,color,size Passed to \code{geom_text()} to define the text \link[=vbl_doc_plot_aesthetics]{aesthetics}.
#' @param hjust,vjust Numeric scalars or \link[=vbl_def]{default}. Default justifications depend
#' on input for `anchor`. If `anchor` is specified as a character anchor definition, `hjust` and `vjust`
#' are picked such that the text is aligned to the respective side.
#'
#' @param ... Additional arguments passed to \code{ggplot2::geom_text()}.
#'
#' @inherit vbl_doc_layer params return
#' @inheritParams vbl_doc
#'
#' @details
#' Text handling depends on the type of \code{text}.
#'
#' If \code{text} is a character vector, it is checked to be non-empty and flattened with
#' \code{collapse}. The resulting string is drawn on every slice of the incoming \code{vbl2D}.
#'
#' If \code{text} is a data.frame, it must contain a valid \code{slice} column. The provided slices
#' are matched to the layer’s available slices. If no overlap exists, an error is raised reporting
#' both the provided slices and the layer’s slices. For each matched slice, all non-\code{slice}
#' columns are converted into \code{<name><sep><value>} strings and then combined using \code{collapse}.
#'
#' If \code{text} is a named list of formulas, only named formulas are kept. Each formula is converted
#' to a quosure anchored in the caller environment and evaluated per slice using
#' \code{dplyr::summarise(vbl2D, .by = "slice", !!!text)}. The resulting summary columns are then
#' converted into \code{<name><sep><value>} strings and combined using \code{collapse}.
#'
#' Positioning is computed per slice by selecting the reference bounding box specified by \code{ref_bb}
#' (\code{data_bb()}, \code{plot_bb()}, \code{screen_bb()}, or \code{slice_bb()}) and converting the chosen
#' \code{anchor} into absolute image coordinates via \code{as_img_anchor_abs()}.
#'
#' @export
layer_text_ann <- function(text,
                           anchor = "top-left",
                           ref_bb = "screen",
                           sep = ": ",
                           collapse = "\n",
                           alpha = 0.9,
                           color = "white",
                           size = 3.5,
                           slices = NULL,
                           hjust = vbl_def(),
                           vjust = vbl_def(),
                           ...){

  .stop_if_not(is.character(sep))
  .stop_if_not(is.character(collapse))

  ref_bb <- .match_arg(ref_bb, choices = c("data", "plot", "screen", "slice"))

  vbl_layer(
    fun = function(vbl2D){

      vbl2D <- .filter_layer(vbl2D, slices = slices)

      .layer_text_ann_impl(
        vbl2D = vbl2D,
        text = text,
        anchor = anchor,
        ref_bb = ref_bb,
        collapse = collapse,
        alpha = alpha,
        color = color,
        size = size,
        hjust = hjust,
        vjust = vjust,
        ...
      )

    },
    class_add = "layer_ann"
  )

}

#' @keywords internal
.layer_text_ann_impl <- function(vbl2D,
                                 text,
                                 anchor,
                                 ref_bb,
                                 collapse,
                                 alpha,
                                 color,
                                 size,
                                 hjust,
                                 vjust,
                                 ...){

  # prepare text df
  text_df <- tibble::tibble(slice = slices(vbl2D), text. = "")

  if(is.character(text)){

    # sanity check
    if(length(text) == 0){

      msg <- "Input for `text` is of length 0."
      rlang::abort(msg)

    }

    text_df <-
      dplyr::mutate(
        .data = text_df,
        text. = stringr::str_flatten({{ text }}, collapse = collapse)
        )

  } else if(is.data.frame(text)){

    # sanity check
    if(!"slice" %in% colnames(text) || !is_slice_set(text[["slice"]])){

      msg <- "If `text` is a data.frame, it must contain a slice variable."
      rlang::abort(msg)

    } else {

      overlap <- intersect(text$slice, text_df$slice)

      if(length(overlap) == 0){

        msg <- c(
          "Could not match text to any slice.",
          i = glue::glue("Text input for slices: {.slice_collapse(text$slice, ' and ') }"),
          i = glue::glue("Layer has slices: {.slice_collapse(slices(vbl2D), ' and ')}")
        )

        rlang::abort(msg)

      }

    }

    text_df <- dplyr::filter(text_df, slice %in% text$slice)

    # collapse columns to text.
    vnames <- setdiff(colnames(text), "slice")

    for(i in 1:nrow(text_df)){

      idx <- which(text$slice == text_df$slice[i])

      text_df$text.[[i]] <-
        purrr::map_chr(
          .x = vnames,
          .f = ~ paste0(.x, sep, text[idx, .x])
        ) %>%
        stringr::str_flatten(string = ., collapse = collapse)

    }

  } else if(is.list(text)){

    # process text
    text <- purrr::keep(text, .p = rlang::is_formula)
    text <- text[names(text) != ""]

    if(length(text) == 0){

      msg <- "If `text` is a list, it must contain at least one named formula."
      rlang::abort(msg)

    }

    text <-
      purrr::imap(
        .x = text,
        .f = ~ rlang::as_quosure(.x, env = rlang::caller_env())
      )

    text <- dplyr::summarise(vbl2D, !!!text, .by = "slice")

    # collapse columns to text.
    vnames <- setdiff(colnames(text), "slice")

    for(i in 1:nrow(text_df)){

      idx <- which(text$slice == text_df$slice[i])

      text_df$text.[[i]] <-
        purrr::map_chr(
          .x = vnames,
          .f = ~ paste0(.x, sep, text[idx, .x])
        ) %>%
        stringr::str_flatten(string = ., collapse = collapse)

    }

  } else {

    msg <- "Invalid input for `text`. Must be a character vector, a data.frame or a named list of formulas."

  }

  # add positioning
  pos_df <-
    purrr::map_dfr(
      .x = text_df$slice,
      .f = function(slice){

        if(ref_bb == "data"){

          bb <- data_bb(vbl2D, slice = slice)

        } else if(ref_bb == "plot"){

          bb <- plot_bb(vbl2D)

        } else if(ref_bb == "screen"){

          bb <- screen_bb(vbl2D, slice = slice)

        } else if(ref_bb == "slice"){

          bb <- slice_bb(vbl2D, slice = slice)

        }

        as_img_anchor_abs(anchor, bb2D = bb)

      }
    )

  text_df <- cbind(text_df, pos_df)

  if(is_offset(vbl2D) && ref_bb == "plot"){

    text_df <- dplyr::distinct(text_df, text., col, row)

  }

  # justifications
  if(.is_vbl_def(hjust)){

    if(is.character(anchor)){

      hjust <-
        dplyr::case_when(
          grepl("left", anchor) ~ 0,
          grepl("right", anchor) ~ 1,
          TRUE ~ 0.5
        )

    } else {

      hjust <- 0.5

    }

  }

  if(.is_vbl_def(vjust)){

    if(is.character(anchor)){

      vjust <-
        dplyr::case_when(
          grepl("top", anchor) ~ 1,
          grepl("bottom", anchor) ~ 0,
          TRUE ~ 0.5
        )

    } else {

      vjust <- 0.5

    }

  }

  # output
  list(
    ggplot2::geom_text(
      data = text_df,
      mapping = ggplot2::aes(x = col, y = row, label = text.),
      alpha = alpha,
      color = color,
      size = size,
      hjust = hjust,
      vjust = vjust,
      ...
    )
  )

}


#' @rdname layer_labs
#' @export
layer_theme <- function(...){

  vbl_layer(
    fun = function(vbl2D){

      list(
        ggplot2::theme(
          ...
        )
      )

    },
    class_add = "layer_misc"
  )

}


