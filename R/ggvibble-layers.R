# ggvibble overlays and themes


# non-data ----------------------------------------------------------------

#' @title Add a data-driven 2D bounding-box
#' @description Draw a rectangular bounding box around the extent of voxels.
#'
#' @param color Color of the bounding-box outline.
#' @param fill Fill of the bounding-box.
#' @param name Logical or character scalar. If character, the name with which
#' the color is associated with in the color legend. If logical, `FALSE` prevents
#' appearance in the legend and `TRUE` falls back to the naming default of the
#' function.
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
#' The bounding box can be expanded by `buffer`, which adds a proportional
#' margin to all sides. The graphical rendering is handled by \link{geom_rect}().
#'
#' @export
layer_bb <- function(color,
                     .cond,
                     .by = NULL,
                     fill = NA,
                     expand = FALSE,
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
                           expand,
                           name,
                           ...){

  data <- bb2D_df(vbl2D, expand = expand)

  .layer_lst_bb(
    data = data,
    name = name,
    color = color,
    fill = fill,
    ...
  )

}

#' @rdname layer_bb
#' @export
layer_bb_data <- function(color = alpha("green", 0.7),
                          fill = NA,
                          slices = NULL,
                          name = "Data",
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
                                name,
                                ...){

  data <-
    purrr::map_df(
      .x = slices(vbl2D),
      .f = function(slice){

        ls <- data_bb(vbl2D, slice = slice)

        tibble::tibble(
          slice = slice,
          cmin = ls$col[1],
          cmax = ls$col[2],
          rmin = ls$row[1],
          rmax = ls$row[2]
        )

      }
    )

  .layer_lst_bb(
    data = data,
    name = name,
    color = color,
    fill = fill,
    ...
  )

}


#' @rdname layer_bb
#' @export
layer_bb_plot <- function(color = alpha("yellow", 0.7),
                          fill = NA,
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
                                name,
                                ...){

  data <- as_bb2D_df(plot_bb(vbl2D))

  .layer_lst_bb(
    data = data,
    name = name,
    color = color,
    fill = fill,
    ...
  )

}

#' @rdname layer_bb
#' @export
layer_bb_screen <- function(color = alpha("blue", 0.7),
                            fill = NA,
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
        name = name,
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
                                  name,
                                  ...){

  data <-
    purrr::map_df(
      .x = slices(vbl2D),
      .f = function(slice){

        ls <- screen_bb(vbl2D, slice = slice)

        tibble::tibble(
          slice = slice,
          cmin = ls$col[1],
          cmax = ls$col[2],
          rmin = ls$row[1],
          rmax = ls$row[2]
        )

      }
    )

  .layer_lst_bb(
    data = data,
    name = name,
    color = color,
    fill = fill,
    ...
  )

}

#' @rdname layer_bb
#' @export
layer_bb_slice <- function(color = alpha("red", 0.7),
                           fill = NA,
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
                                 name,
                                 ...){

  data <- bb2D_df(vbl2D, expand = FALSE)

  .layer_lst_bb(
    data = data,
    name = name,
    color = color,
    fill = fill,
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
  color_use <- alpha(cvec[vbl2D[[var]]], alpha_use)

  list(
    ggnewscale::new_scale_fill(),
    ggplot2::geom_tile(
      data = vbl2D,
      mapping = ggplot2::aes(x = col, y = row, fill = .data[[var]]),
      alpha = alpha_use,
      color = color_use
    ),
    scale_fill_categorical(
      clrp = clrp,
      clrp_adjust = clrp_adjust,
      names = levels(vbl2D[[var]]),
      ...
    )
  )

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
                             col = 0.1,
                             row = 0.1,
                             alpha = 0.2,
                             color = "lightgrey",
                             linewidth = 0.5,
                             linetype = "solid"
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
                         abbrev = NULL,
                         repel = FALSE,
                         centroid = function(x) median(x, TRUE),
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
                  var_out = "idx",
                  pref_out = "lbl.",
                  minPts = 6,
                  min_size = 0.33,
                  rm_outlier = TRUE
                  )

            } else {

              label_df$idx <- "1"

            }

            dplyr::group_by(label_df, slice, idx, !!rlang::sym(var)) %>%
              dplyr::mutate(
                col_cent. = centroid[["col"]](col),
                row_cent. = centroid[["row"]](row),
                col_dist. = abs(col - col_cent.),
                row_dist. = abs(row - row_cent.),
                comb_dist. = col_dist. + row_dist.
              ) %>%
              dplyr::slice_min(order_by = comb_dist.)

          }
        )

      }
    )

  data[[var]] <- as.character(data[[var]])

  assign("labels", labels, envir = .GlobalEnv)
  assign("data", data, envir = .GlobalEnv)

  if(is.character(include) && .is_named(include)){

    renamed <- unname(include[data[[var]]])

    data[[var]] <- dplyr::if_else(is.na(renamed), data[[var]], renamed)

  }

  if(is.function(abbrev)){

    data <-
      dplyr::mutate(
        .data = data,
        dplyr::across(
          .cols = dplyr::all_of(var),
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
          alpha = .eval_tidy_opacity(vbl2D, opacity = opacity, var = var)
        )
      )

  } else {

    layer_lst <-
      list(
        ggplot2::geom_raster(
          data = vbl2D,
          mapping = ggplot2::aes(x = col, y = row),
          alpha = .eval_tidy_opacity(vbl2D, opacity = opacity, var = var),
          fill = color
        )
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

    },
    class_add = "layer_raster"
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

  if(is_offset(vbl2D)){ vbl2D <- .remove_overlap(vbl2D) }

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
      limits = .get_var_limits(vbl2D, var),
      ...
    )
  )

}



#' @title Add an outline layer
#' @description Overlay a \link{ggplane}() plot by outlining voxels
#' that match a certain condition. Outlines are computed slice-wise.
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
#' @inherit vbl_doc_layer params return
#' @inheritParams vbl_doc
#'
#' @export
layer_outline <- function(color,
                          .cond = NULL,
                          .by = NULL,
                          linetype = "solid",
                          linewidth = 1,
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
          color = ggplot2::alpha(color, alpha),
          linetype = linetype,
          linewidth = linewidth,
          fill = NA,
          ...
        )

    }

  }

  return(layer_lst)

}


#' @title Layer for displaying slice numbers
#' @description Add slice numbers as text labels to a \link{ggplane()}-plot.
#'
#' @param anchor Character or numeric anchor specification defining the relative
#'   position of labels within each slice's bounding box. See
#'   \code{\link{img_anchors}} for valid presets.
#' @param space One of \code{"slice"}, \code{"screen"}, or \code{"avg"}, indicating
#'   which 2D limit should define the anchor position:
#'   \itemize{
#'     \item \code{"slice"} – use raw slice limits.
#'     \item \code{"screen"} – use screen-space limits (after offsets).
#'     \item \code{"avg"} – use the average of slice and screen limits.
#'   }
#' @param align Optional. If \code{"col"} or \code{"row"}, enforce alignment of
#'   label positions across slices along the chosen axis.
#' @param wrap Optional template for label text. Defaults to \code{"{slice}"}.
#' @param angle Rotation angle passed to \code{geom_text()}.
#' @param alpha,color,size Passed to \code{geom_text()} to define the label aesthetics.
#' @param ... Additional arguments passed to \code{geom_text()}.
#'
#' @inherit vbl_doc_layer return
#'
#' @details
#' Anchor coordinates may be supplied either as preset strings (e.g.
#' \code{"top-right"}) or as relative coordinates in \eqn{[0,1]^2}. For each
#' slice, the chosen limit specification (\code{to}) determines the bounding box
#' in which the anchor is resolved. If \code{align} is used, the selected axis
#' (\code{col} or \code{row}) is harmonized across all slices.
#'
#' @export

layer_slice_number <- function(anchor,
                               space = "avg",
                               align = NULL,
                               wrap = NULL,
                               angle = 0,
                               alpha = 0.8,
                               color = "white",
                               size = 3.5,
                               ...){

  vbl_layer(
    fun = function(vbl2D){

      .layer_slice_number_impl(
        vbl2D = vbl2D,
        anchor = anchor,
        space = space,
        align = align,
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
.layer_slice_number_impl <- function(vbl2D,
                                     anchor,
                                     space,
                                     align,
                                     wrap,
                                     angle,
                                     alpha,
                                     color,
                                     size,
                                     ...){

  stopifnot(within_limits(angle, c(0, 360)))
  stopifnot(is_img_anchor_chr(anchor) | is_img_anchor_rel(anchor))

  # anchor instructions
  if(is_img_anchor_chr(anchor)){

    anchor_chr <- anchor
    anchor_rel <- img_anchors[[anchor]]

  } else {

    anchor_rel <- anchor

  }

  # construct data.frame
  wrap <- ifelse(is.character(wrap), wrap[1], "{slice}")

  df <-
    purrr::map_df(
      .x = slices(vbl2D),
      .f = function(slice){

        if(space == "slice"){

          bb2D <- slice_bb(vbl2D, slice)

        } else if(space == "screen"){

          bb2D <- screen_bb(vbl2D, slice)

        } else if(space == "avg"){

          bb2D <- avg_bb2D(
            a = slice_bb(vbl2D, slice),
            b = screen_bb(vbl2D, slice)
          )

        }

        as_img_anchor_abs(anchor_rel, bb2D = bb2D)

      }
    ) %>%
    dplyr::mutate(
      slice = slices(vbl2D),
      label = as.character(glue::glue(wrap))
    )

  # ensure alignment of slice num positions across slices, if desired
  if(is.character(align)){

    align <- match.arg(align, choices = c("col", "row"))

    idx <- which(align == c("col", "row"))
    val <- anchor_rel[idx]

    fn_use <-
      ifelse(
        test = val == 0.5,
        yes = mean,
        no = ifelse(test = val < 0.5,
                    yes = ifelse(idx == 1, min, max),
                    no = ifelse(idx == 2, min, max)
        )
      )

    df[[align]] <- fn_use(df[[align]])

  }

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
#' Add reference lines and slice labels indicating the positions of slices from
#' another anatomical plane.
#'
#' @param plane_ref Character scalar specifying the anatomical plane of the
#'   reference slices (\code{"sag"}, \code{"axi"}, or \code{"cor"}). Must differ
#'   from the plane used in \link{ggplane}().
#' @param slices_ref Integer vector giving the slice indices (in
#'   \code{plane_ref}) to display as reference lines.
#' @param anchor Character scalar controlling the label anchor relative to the
#'   reference line. Valid values depend on the orientation of the reference
#'   plane and resolve to one of: \code{"top"}, \code{"bottom"},
#'   \code{"left"}, \code{"right"}. If \code{NULL}, a default anchor is chosen
#'   based on the intersecting axis.
#' @param space Character scalar indicating which 2D limit space to use when
#'   constructing reference lines:
#'   \itemize{
#'     \item \code{"slice"} – use per-slice limits from \link{slice_bb}.
#'     \item \code{"screen"} – use screen-space limits from \link{screen_bb} (after offsets).
#'   }
#' @param spacer Numeric offset (in voxel units) applied to the label position
#' away from the reference line along the perpendicular axis. Interpreted in absolutes.
#' @param color Numeric vector of length one or two specifying alpha for
#'   the line and labels (recycled if length one).
#' @param color Character vector of length one or two specifying colors for
#'   the line and labels (recycled if length one).
#' @param size Numeric text size passed to \code{geom_text()}.
#' @param linewidth Numeric line width passed to \code{geom_segment()}.
#' @param linetype Line type passed to \code{geom_segment()}.
#' @param ... Additional arguments forwarded to \code{geom_text()}.
#'
#' @inherit vbl_doc_layer params return
#' @inheritParams vbl_doc
#'
#' @details
#' The function reverses slice offsets in the input \code{vbl2D}, computes
#' orthogonal reference lines for all combinations of plotted slices and
#' \code{slices_ref}, optionally applies a label anchor and spacing, and then
#' reapplies the offset layout. Label alignment (\code{hjust}/\code{vjust}) is
#' determined automatically from \code{anchor}.
#'
#' @seealso \code{\link{slice_bb}}, \code{\link{screen_bb}},
#'   \code{\link{layer_slice_number}}
#'
#' @export

layer_slice_ref <- function(plane_ref,
                            slices_ref,
                            alpha = 0.9,
                            color = "red",
                            size = 4.5,
                            linewidth = 0.5,
                            linetype = "solid",
                            anchor = NULL,
                            space = "slice",
                            spacer = 5,
                            align = TRUE,
                            ...){

  vbl_layer(
    fun = function(vbl2D){

      if(plane_ref == plane(vbl2D)){

        .glue_stop("`plane` input for `ggplane()` and `layer_slice_ref()` must not be identical.")

      }

      .layer_slice_ref_impl(
        vbl2D = vbl2D,
        plane_ref = plane_ref,
        slices_ref = slices_ref,
        alpha = alpha,
        color = color,
        size = size,
        linetype = linetype,
        linewidth = linewidth,
        anchor = anchor,
        space = space,
        spacer = spacer,
        align = align,
        ...
      )

    },
    class_add = "layer_ann"
  )

}

#' @keywords internal
.layer_slice_ref_impl <- function(vbl2D,
                                  plane_ref,
                                  slices_ref,
                                  alpha,
                                  color,
                                  size,
                                  linewidth,
                                  linetype,
                                  anchor,
                                  space,
                                  spacer,
                                  align,
                                  ...){

  vbl2D_ref <- .reverse_offset(vbl2D)

  # sanity checks
  if(any(c("hjust", "vjust") %in% names(list(...)))){

    warning("`hjust` and `vjust` are automatically determined for `geom_text()`.")

  }

  space <- match.arg(space, choices = c("screen", "slice"))

  alpha <- if(length(alpha) == 1){ rep(alpha, 2) } else { alpha }
  color <- if(length(color) == 1){ rep(color, 2) } else { color }

  axis_ref <-
    .req_axis_2D_ref(
      plane = plane(vbl2D),
      plane_ref = plane_ref
    )

  # text anchor validation
  if(is.null(anchor)){

    anchor <- unname(c("col" = "top", "row" = "left")[axis_ref])

  }

  if(axis_ref == "col"){

    anchor <- match.arg(anchor, choices = c("top", "bottom"))

  } else if(axis_ref == "row"){

    anchor <- match.arg(anchor, choices = c("left", "right"))

  }

  end <- unname(c(
    "left" = "cmin",
    "right" = "cmax",
    "top" = "rmin",
    "bottom" = "rmax"
  )[anchor])

  # line positioning
  line_df <-
    purrr::map_df(
      .x = slices(vbl2D),
      .f = function(slice){

        purrr::map_df(
          .x = slices_ref,
          .f = function(slice_ref){

            if(space == "screen"){

              lim0 <- screen_bb(vbl2D_ref, slice = slice)

            } else if(space == "slice") {

              lim0 <- slice_bb(vbl2D_ref, slice)

            }

            lim0[[axis_ref]] <- rep(slice_ref, 2)

            lim_adj <- .offset_lim0(vbl2D, slice = slice, lim0 = lim0)

            tibble::tibble(
              cmin = min(lim_adj$col),
              cmax = max(lim_adj$col),
              rmin = min(lim_adj$row),
              rmax = max(lim_adj$row),
              slice_ref = slice_ref
            )

          }

        ) %>%
          dplyr::mutate(slice = slice)

      }
    )

  if(isTRUE(align)){

    p1 <- ifelse(grepl("min", end), "min", "max")
    p2 <- ifelse(grepl("min", p1), "max", "min")

    start <- stringr::str_replace(end, pattern = p1, replacement = p2)

    line_df$dst <- abs(c(line_df[[start]] - line_df[[end]]))

    max_dst <- max(line_df$dst)

    sign_dir <- sign(line_df[[end]] - line_df[[start]])

    line_df[[end]] <- line_df[[start]] + sign_dir * max_dst

  }

  # text position
  text_df <- line_df[, c("slice_ref", "slice")]

  if(axis_ref == "col"){

    text_df$x <- line_df$cmin # identical with cmax
    text_df$y <- line_df[[end]]

    if(anchor == "top"){

      text_df$y <- text_df$y - spacer

    } else if(anchor == "bottom") {

      text_df$y <- text_df$y + spacer

    }

  } else if(axis_ref == "row"){

    text_df$x <- line_df[[end]]
    text_df$y <- line_df$rmin # identical with rmax

    if(anchor == "left"){

      text_df$y <- text_df$y - spacer

    } else if(anchor == "right") {

      text_df$y <- text_df$y + spacer

    }

  }

  text_just <- list(
    "left" = c(1, 0.5),
    "right" = c(0, 0.5),
    "top" = c(0.5, 0),
    "bottom" = c(0.5, 1)
  )[[anchor]]

  list(
    ggplot2::geom_segment(
      data = line_df,
      mapping = ggplot2::aes(x = cmin, xend = cmax, y = rmin, yend = rmax),
      alpha = alpha[1],
      color = color[1],
      linetype = linetype,
      linewidth = linewidth
    ),
    ggplot2::geom_text(
      data = text_df,
      mapping = ggplot2::aes(x = x, y = y, label = slice_ref),
      alpha = alpha[2],
      color = color[2],
      size = size,
      hjust = text_just[1],
      vjust = text_just[2],
      ...
    )
  )

}







