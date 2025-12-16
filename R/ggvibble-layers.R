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
#' @export
layer_bb <- function(color,
                     .cond,
                     .by = NULL,
                     fill = NA,
                     linetype = "solid",
                     linewidth = 0.75,
                     expand = as_abs(0.5),
                     name = TRUE,
                     ...){

  .cond_quo <- rlang::enquo(.cond)

  color_nm <- NULL
  if(isTRUE(name)){

    color_nm <- purrr::set_names(color, rlang::quo_text(.cond_quo))

  } else if(is.character(name)){

    color_nm <- purrr::set_names(color, name)

  }

  vbl_layer(
    fun = function(vbl2D){

      vbl2D <- .filter_layer(vbl2D, .cond = .cond_quo, .by = .by, layer = "layer_bb()")

      .layer_bb_impl(
        vbl2D = vbl2D,
        color = color,
        fill = fill,
        linetype = linetype,
        linewidth = linewidth,
        expand = expand,
        name = names(color_nm),
        ...
      )

    },
    color_nm = color_nm,
    class_add = "layer_bb"
  )

}


#' @keywords internal
.layer_bb_impl <- function(vbl2D,
                           color,
                           fill,
                           linetype,
                           linewidth,
                           expand,
                           name,
                           ...){

  data <- bb2D_df(vbl2D, expand = expand)

  .layer_lst_bb(
    data = data,
    name = name,
    color = color,
    fill = fill,
    linetype = linetype,
    linewidth = linewidth,
    ...
  )

}

#' @rdname layer_bb
#' @export
layer_bb_data <- function(color = alpha("green", 0.7),
                          fill = NA,
                          linetype = "solid",
                          linewidth = 0.75,
                          slices = NULL,
                          name = TRUE,
                          ...){

  color_nm <- NULL
  if(isTRUE(name)){

    color_nm <- purrr::set_names(color, "Data")

  } else if(is.character(name)){

    color_nm <- purrr::set_names(color, name)

  }

  vbl_layer(
    fun = function(vbl2D){

      if(is.numeric(slices)){

        vbl2D <- dplyr::filter(vbl2D, slice %in% slices)

      }

      .layer_bb_data_impl(
        vbl2D = vbl2D,
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
.layer_bb_data_impl <- function(vbl2D,
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
    color = color,
    fill = fill,
    linetype = linetype,
    linewidth = linewidth,
    ...
  )

}


#' @rdname layer_bb
#' @export
layer_bb_plot <- function(color = alpha("yellow", 0.7),
                          fill = NA,
                          linetype = "solid",
                          linewidth = 0.75,
                          name = TRUE,
                          ...){

  color_nm <- NULL
  if(isTRUE(name)){

    color_nm <- purrr::set_names(color, "Plot")

  } else if(is.character(name)){

    color_nm <- purrr::set_names(color, name)

  }

  vbl_layer(
    fun = function(vbl2D){

      .layer_bb_plot_impl(
        vbl2D = vbl2D,
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
    color = color,
    fill = fill,
    linetype = linetype,
    linewidth = linewidth,
    ...
  )

}

#' @rdname layer_bb
#' @export
layer_bb_screen <- function(color = alpha("blue", 0.7),
                            fill = NA,
                            linetype = "solid",
                            linewidth = 0.75,
                            slices = NULL,
                            name = TRUE,
                            ...){

  color_nm <- NULL
  if(isTRUE(name)){

    color_nm <- purrr::set_names(color, "Screen")

  } else if(is.character(name)){

    color_nm <- purrr::set_names(color, name)

  }

  vbl_layer(
    fun = function(vbl2D){

      if(is.numeric(slices)){

        vbl2D <- dplyr::filter(vbl2D, slice %in% slices)

      }

      .layer_bb_screen_impl(
        vbl2D = vbl2D,
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
.layer_bb_screen_impl <- function(vbl2D,
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
    color = color,
    fill = fill,
    linetype = linetype,
    linewidth = linewidth,
    ...
  )

}

#' @rdname layer_bb
#' @export
layer_bb_slice <- function(color = alpha("red", 0.7),
                           fill = NA,
                           linetype = "solid",
                           linewidth = 0.75,
                           slices = NULL,
                           name = TRUE,
                           ...){

  color_nm <- NULL
  if(isTRUE(name)){

    color_nm <- purrr::set_names(color, "Slice")

  } else if(is.character(name)){

    color_nm <- purrr::set_names(color, name)

  }

  vbl_layer(
    fun = function(vbl2D){

      if(is.numeric(slices)){

        vbl2D <- dplyr::filter(vbl2D, slice %in% slices)

      }

      .layer_bb_slice_impl(
        vbl2D = vbl2D,
        color = color,
        fill = fill,
        name = names(color_nm),
        linetype = linetype,
        linewidth = linewidth,
        ...
      )

    },
    color_nm = color_nm,
    class_add = "layer_bb"
  )

}

#' @keywords internal
.layer_bb_slice_impl <- function(vbl2D,
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
    color = color,
    fill = fill,
    linetype = linetype,
    linewidth = linewidth,
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
#' @param ... Additional arguments passed to \link{scale_fill_manual}().
#'
#' @inherit vbl_doc_layer params return
#'
#' @inheritParams vbl_doc_var_categorical
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

    },
    class_add = "layer_raster"
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

  is_vartype(vbl2D, var = var, type = "categorical")

  vbl2D <- vbl2D[!is.na(vbl2D[[var]]), ]

  if(is_offset(vbl2D)){ vbl2D <- .remove_overlap(vbl2D) }

  if(is.character(vbl2D[[var]])){ vbl2D[[var]] <- as.factor(vbl2D[[var]]) }

  cvec <- color_vector(clrp, names = levels(vbl2D[[var]]), clrp_adjust = clrp_adjust)

  alpha_use <- .eval_tidy_opacity(vbl2D, opacity = opacity, var = var)

  list(
    ggnewscale::new_scale_fill(),
    ggplot2::geom_raster(
      data = vbl2D,
      mapping = ggplot2::aes(x = col, y = row, fill = .data[[var]]),
      alpha = alpha_use,
      interpolate = vbl_opts("interpolate")
    ),
    scale_fill_categorical(
      clrp = clrp,
      clrp_adjust = clrp_adjust,
      names = levels(vbl2D[[var]]),
      ...
    )
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
layer_crop <- function(.cond, ...){

  .cond_quo <- rlang::enquo(.cond)

  vbl_layer(
    fun = function(vbl2D){

      vbl2D <- .filter_layer(vbl2D, .cond = .cond_quo, layer = "layer_crop()")

      .layer_crop_impl(
        vbl2D = vbl2D,
        ...
      )

    },
    class_add = "layer_crop"
  )

}

#' @keywords internal
.layer_crop_impl <- function(vbl2D,
                             ...){

  plot_lim <-
    list(
      col = range(vbl2D$col),
      row = range(vbl2D$row)
    )

  layer_lst <-
    list(
      ggplot2::coord_equal(
        ratio = .ratio2D(vbl2D),
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
#' @param col,row Numeric or `NULL`. Controls vertical grid lines:
#' \itemize{
#'   \item `length(col) > 1`: values are used directly as x-intercepts.
#'   \item `length(col) == 1` and `col < 1`: interpreted as a proportion of
#'   the maximal `col` limit. Grid lines are placed symmetrically around the
#'   center (half the maximum) with that spacing.
#'   \item `length(col) == 1` and `col >= 1`: interpreted as an absolute
#'   spacing in coordinate units, again used to place symmetric lines around
#'   the center.
#'   \item `NULL` or length 0: no vertical grid lines are drawn.
#' }
#' @param alpha,line_color,linewidth,linetype Aesthetic settings for grid appearance.
#' Each can be supplied either as a single value or as a vector of length 2:
#' \itemize{
#'   \item \strong{Length 1}: The value is applied to both axes (vertical and
#'   horizontal grid lines).
#'
#'   \item \strong{Length 2}: The first element is applied to vertical grid
#'   lines (controlled by `col`) and the second element to horizontal grid
#'   lines (controlled by `row`).
#' }
#'
#' Input specifics:
#' \itemize{
#'   \item \strong{alpha}: Numeric transparency in `[0,1]`.
#'   \item \strong{color}: Any valid color specification.
#'   \item \strong{linewidth}: Positive numeric line width.
#'   \item \strong{linetype}: Any valid line type (e.g., `"solid"`, `"dashed"`).
#' }
#'
#' @inherit vbl_doc_layer return
#'
#' @details
#' `layer_grid()` computes grid-line positions using \link{grid_intercepts}()
#' based on the `col` and `row` settings and the coordinate limits stored in
#' `var_smr`. For each slice, vertical lines are created via \link{geom_vline}()
#' and horizontal lines via \link{geom_hline}(), using axis-specific aesthetics
#' derived from `alpha`, `color`, `linewidth`, and `linetype`.
#'
#' This layer is primarily intended for visual guidance and debugging and can
#' be combined with numeric, mask, or label layers in the `ggplane()` framework.
#'
#' @export
layer_grid <- function(col = 0.1,
                       row = 0.1,
                       alpha = 0.25,
                       color = "lightgrey",
                       linewidth = 0.5,
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
                             col,
                             row,
                             alpha,
                             color,
                             linewidth,
                             linetype
                             ){

  style_lst <-
    list(alpha = alpha, color = color, linewidth = linewidth, linetype = linetype) %>%
    purrr::map(.f = ~ if(length(.x) ==1 ){ rep(.x, 2) } else { .x } )

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
#' @param var Character. Name of a categorical variable to annotate.
#' @param include Optional character vector of labels to include. If named the
#' labels are renamed according to *c('<orig label>' = '<displayed label>').
#' @param exclude Optional character vector of labels to exclude.
#' @param alpha Numeric transparency for label text.
#' @param color Text color passed to `geom_text()`.
#' @param size Text size passed to `geom_text()`.
#' @param use_dbscan Logical. If TRUE, label voxels within each slice are clustered
#' using `dbscan2D()` before centroid computation; otherwise all voxels of a label
#' form a single region.
#' @param centroid The function with which to compute the centroid label position
#' based on col and row. A named list of functions can be supplied, where names
#' must be *c('col', 'row')*.
#' @param abbrev Optional function applied to label names for abbreviation.
#' @param repel Logical. If `TRUE`, the text is rendered with \link{geom_text_repel}()
#' else with \link{geom_text}().
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
                         size = 4.5,
                         use_dbscan = TRUE,
                         centroid = median,
                         abbrev = NULL,
                         repel = FALSE,
                         .cond = NULL,
                         .by = NULL,
                         ...){

  .cond_quo <- rlang::enquo(.cond)

  vbl_layer(
    fun = function(vbl2D){

      layer <- glue::glue("layer_labels(var = '{var}', ...")
      vbl2D <- .filter_layer(vbl2D, .cond = .cond_quo, .by = .by, layer = layer)

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

  if(!is.list(centroid)){

    stopifnot(is.function(centroid))
    centroid <- list(col = centroid, row = centroid)

  } else {

    stopifnot(all(c("col", "row") %in% names(centroid)))
    stopifnot(all(purrr::map_lgl(centroid, is.function)))

  }

  if(is_offset(vbl2D)){ vbl2D <- .remove_overlap(vbl2D)}

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

            label_df <- slice_df[slice_df[[var]] == label,]

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

#' @title Add a masking layer
#' @description Overlay a logical mask on a \link{ggplane}() plot by filling
#' voxels.
#'
#' @param color Color used for the mask fill.
#'
#' @details
#' The condition of `.cond` determines for which voxels are included in the mask.
#' If no condition is provided (`.cond = NULL`) this layer masks every voxel in
#' every slice of the 2D vibble passed to this layer by `ggplane()`.
#'
#' @inherit vbl_doc_layer params return
#' @inheritParams vbl_doc
#'
#' @export
layer_mask <- function(color,
                       .cond = NULL,
                       .by = NULL,
                       opacity = 0.25,
                       name = TRUE,
                       ...){

  opacity_quo <- rlang::enquo(opacity)
  .cond_quo <- rlang::enquo(.cond)

  color_nm <- NULL
  if(isTRUE(name)){

    color_nm <- purrr::set_names(color, rlang::quo_text(.cond_quo))

  } else if(is.character(name)){

    color_nm <- purrr::set_names(color, name)

  }

  vbl_layer(
    fun = function(vbl2D){

      layer <- glue::glue("layer_mask(color = '{color}', ...)")
      vbl2D <- .filter_layer(vbl2D, .cond = .cond_quo, .by = .by, layer = layer)

      .layer_mask_impl(
        vbl2D = vbl2D,
        color = color,
        opacity = opacity_quo,
        name = names(color_nm),
        ...
      )

    },
    color_nm = color_nm,
    class_add = "layer_mask"
  )

}

#' @keywords internal
.layer_mask_impl <- function(vbl2D,
                             color,
                             opacity,
                             name,
                             ...){

  data <- if(is_offset(vbl2D)){ .remove_overlap(vbl2D) } else { vbl2D }

  if(is.character(name)){

    layer_lst <-
      list(
        ggplot2::geom_raster(
          data = dplyr::mutate(vbl2D, mask. = {{ name }}),
          mapping = ggplot2::aes(x = col, y = row, fill = mask.),
          alpha = .eval_tidy_opacity(vbl2D, opacity = opacity, var = var),
          interpolate = vbl_opts("interpolate")
        )
      )

  } else {

    layer_lst <-
      list(
        ggplot2::geom_raster(
          data = vbl2D,
          mapping = ggplot2::aes(x = col, y = row),
          alpha = .eval_tidy_opacity(vbl2D, opacity = opacity, var = var),
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
                          opacity = c(0.2, 0.45),
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
                                opacity = c(0.2, 0.45),
                                ...){

  is_vartype(vbl2D, var = var, type = "numeric")

  if(is_offset(vbl2D)){ vbl2D <- .remove_overlap(vbl2D) }

  list(
    ggnewscale::new_scale_fill(),
    ggplot2::geom_raster(
      data = vbl2D,
      mapping = ggplot2::aes(x = col, y = row, fill = .data[[var]]),
      alpha = .eval_tidy_opacity(vbl2D, opacity = opacity, var = var),
      interpolate = vbl_opts("interpolate")
    ),
    scale_fill_numeric(
      clrsp,
      limits = .get_var_limits(vbl2D, var),
      ...
    )
  )

}



#' @title Add outlines
#' @description Overlay a \link{ggplane}() plot by outlining voxels
#' that match a certain condition.
#'
#' @param alpha Numeric. Controls the transparency of the lines.
#' @param color Character. Controls the color used for the lines.
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
layer_outline <- function(color,
                          .cond = NULL,
                          .by = NULL,
                          linetype = "solid",
                          linewidth = 0.75,
                          use_dbscan = TRUE,
                          concavity = 2.5,
                          clip_overlap = TRUE,
                          name = TRUE,
                          ...){

  .cond_quo <- rlang::enquo(.cond)

  color_nm <- NULL
  if(isTRUE(name)){

    color_nm <- purrr::set_names(color, rlang::quo_text(.cond_quo))

  } else if(is.character(name)){

    color_nm <- purrr::set_names(color, name)

  }

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
        color = color,
        linetype = linetype,
        linewidth = linewidth,
        use_dbscan = use_dbscan,
        concavity = concavity,
        clip_overlap = clip_overlap,
        outlines_slice = outlines_slice,
        name = names(color_nm),
        ...
      )

    },
    color_nm = color_nm,
    class = "layer_outline"
  )

}

#' @keywords internal
.layer_outline_impl <- function(vbl2D,
                                color,
                                linetype,
                                linewidth,
                                concavity,
                                use_dbscan,
                                outlines_slice,
                                clip_overlap,
                                name,
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
  if(is_offset(vbl2D) && isTRUE(clip_overlap)){

    slices_main <- unique(vbl2D$slice)
    slices_lead <- dplyr::lead(slices_main)

    outlines <-
      purrr::map_df(
        .x = seq_along(slices_main),
        .f = function(i){

          sm <- slices_main[i]
          sl <- slices_lead[i]

          # outline main
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
          fill = NA,
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
          fill = NA,
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
#' @param alpha,color,size Passed to \code{geom_text()} to define the label aesthetics.
#' @param ... Additional arguments passed to \code{geom_text()}.
#'
#' @inherit vbl_doc_layer return
#'
#' @details
#' If \code{align} is used, the selected axis
#' (\code{col} or \code{row}) is harmonized across all slices.
#'
#' @export

layer_slice_numbers <- function(anchor = waiver(),
                                ref_bb = "screen",
                                wrap = "{slice}",
                                angle = 0,
                                alpha = 0.8,
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
  if(.is_waiver(anchor)){

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

  # plot
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
#' @param label_pos Character, logical, or `waiver()`. Controls where slice
#'   projection labels are placed relative to the projection line. Allowed values
#'   are the anchor names *c("top", "bottom", "left", "right", "top-left", "top-right",
#'   "bottom-left", "bottom-right")*.
#'
#'   If `waiver()`, a default position is chosen based on the projection axis
#'   (top for column projections, left for row projections).
#'
#'   If `FALSE`, labels are not drawn.
#'
#' @param label_just Numeric vector of length two or `waiver()`. Horizontal and
#'   vertical text justification passed to `geom_text()` as `hjust` and `vjust`.
#'
#'   If `waiver()`, justification is computed automatically from `label_pos`,
#'   the projection axis, and `spacer`.
#'   If supplied, must be a numeric vector *c(hjust, vjust)*.
#' @param linewidth Numeric line width passed to \code{geom_segment()}.
#' @param linetype Line type passed to \code{geom_segment()}.
#' @param size Numeric text size passed to \code{geom_text()}.
#' @param spacer Numeric scalar. Controls the offset used when computing default
#'   text justification for slice projection labels.
#'
#'   The value is applied as a small additive or subtractive adjustment to the
#'   automatically determined `hjust` and `vjust` values when `label_just =
#'   waiver()`. Larger values move labels further away from the projection line.
#' @param ... Additional arguments forwarded to \code{geom_text()}.
#'
#' @inherit vbl_doc_layer params return
#'
#' @note Label alignment (\code{hjust}/\code{vjust}) is determined
#' automatically from \code{anchor}.
#'
#' @export

layer_slice_projections <- function(slices_proj,
                                    plane_proj,
                                    color = "red",
                                    alpha = 1,
                                    size = 3.5,
                                    linewidth = 0.75,
                                    linetype = "solid",
                                    ref_bb = "data",
                                    spacer = 0.75,
                                    label_pos = waiver(),
                                    label_just = waiver(),
                                    slices = waiver(),
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
        spacer = spacer,
        slices = slices,
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
                                          spacer,
                                          slices,
                                          ...){

  vbl2D_proj <- reverse_offset(vbl2D)

  # sanity checks
  if(any(c("hjust", "vjust") %in% names(list(...)))){

    warning("`hjust` and `vjust` are determined for `geom_text()` with `label_just`.")

  }

  ref_bb <- match.arg(ref_bb, choices = c("data", "screen", "slice"))

  alpha <- if(length(alpha) == 1){ rep(alpha, 2) } else { alpha }
  color <- if(length(color) == 1){ rep(color, 2) } else { color }

  axis_proj <-
    .req_axis_2D_proj(
      plane = plane(vbl2D),
      plane_proj = plane_proj
    )

  if(!is.numeric(slices)){

    slices <- slices(vbl2D)

  } else {

    stopifnot(all(slices %in% slices(vbl2D)))

  }

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
  if(.is_waiver(label_pos)){

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
  if(.is_waiver(label_just)){

    label_just <- .label_just(label_pos, axis_proj, spacer)

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







