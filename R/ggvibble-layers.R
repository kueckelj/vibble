# ggvibble overlays and themes


# data --------------------------------------------------------------------


#' @title Add a bounding-box layer for mask variables
#' @description Draw a rectangular bounding box around the extent of a mask
#' variable in a `vbl2D` object. Useful for quickly highlighting the spatial
#' footprint of binary structures such as tumors, regions, or segmentations.
#'
#' @param color Color of the bounding-box outline.
#' @param ... Additional arguments passed to \link{geom_rect}().
#'
#' @inherit vbl_doc_vbl_layer params return
#'
#' @inheritParams vbl_doc_var_mask
#' @inheritParams vbl_doc
#'
#' @details
#' `layer_bb()` computes the 2D bounding box of a mask variable based on the
#' minimal and maximal `col` and `row` coordinates where the mask is `TRUE`.
#' The bounding box can be expanded by `buffer`, which adds a proportional
#' margin to all sides. Internally, the bounding box is calculated via
#' `bb2D()`, and the graphical rendering is handled by `\link{geom_rect}()`.
#'
#' @seealso \link{ggplane}(), \link{bb2D}()
#'
#' @examples
#' vbl <- example_vbl()
#' p <- ggplane(vbl, plane = "axi", slices = 90, var = "raw_t1")
#' p + layer_bb(var = "tumor", color = "red")
#'
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


#' @keywords internal
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



#' @title Add a label layer for categorical variables
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
#' @inherit vbl_doc_vbl_layer params return
#'
#' @inheritParams vbl_doc_var_label
#' @inheritParams vbl_doc
#'
#' @seealso \link{ggplane}(), \link{layer_mask}(), \link{layer_numeric}()
#'
#' @examples
#' vbl <- example_vbl()
#'
#' p <- ggplane(
#'   vbl = vbl,
#'   plane = "axi",
#'   slices = 90,
#'   var = "raw_t1"
#' )
#'
#' p + layer_label(
#'   var = "region",
#'   labels = c("frontal", "parietal"),
#'   clrp = "default",
#'   alpha = 0.45
#' )
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


#' @keywords internal
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
      alpha = eval_tidy_alpha(vbl2D, alpha = alpha, var = var)
    ),
    ggplot2::scale_fill_manual(values = color_mapping, ...)
  )

}


#' @title Add a mask layer for logical variables
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
#' @inherit vbl_doc_vbl_layer params return
#' @inheritParams vbl_doc_var_mask
#' @inheritParams vbl_doc
#'
#' @seealso \link{ggplane}(), \link{layer_label}(), \link{layer_bb}(),
#'
#' @examples
#' vbl <- example_vbl()
#'
#' p <- ggplane(
#'   vbl = vbl,
#'   plane = "axi",
#'   slices = 90,
#'   var = "raw_t1"
#' )
#'
#' # Filled mask overlay
#' p + layer_mask(var = "tumor", color = "red")
#'
#' # Outline only
#' p + layer_mask(var = "tumor", color = "red", fill = FALSE, outline = TRUE)
#'
#' @inherit vbl_layer return
#' @export
layer_mask <- function(var,
                       color,
                       alpha = 0.25,
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

#' @keywords internal
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
        alpha = eval_tidy_alpha(data, alpha = alpha, var = var),
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



#' @title Add a numeric layer to a ggvibble plot
#' @description Overlay a numeric variable on a `ggplane()` plot by mapping its
#' values to a continuous fill scale.
#'
#' @param clrsp Color specification passed to \link{scale_fill_numeric}() to
#' define the numeric color mapping.
#' @param ... Additional arguments passed to \link{scale_fill_numeric}().
#'
#' @inherit vbl_doc_vbl_layer params return
#' @inheritParams vbl_doc_var_numeric
#' @inheritParams vbl_doc
#'
#' @details
#' `layer_numeric()` draws a raster layer of `var` using `geom_raster()`. Transparency is
#' computed by `eval_tidy_alpha()` based on `alpha` and the values of `var`. The fill scale is
#' constructed by \link{scale_fill_numeric}(), using `clrsp` and the numeric range obtained from
#' `var_smr()`. A new fill scale is created via `ggnewscale::new_scale_fill()` so that numeric layers do not
#' interfere with previously defined fill scales.
#'
#' @examples
#' vbl <- example_vbl()
#'
#' ggplane(
#'   vbl = vbl,
#'   plane = "axi",
#'   slices = 90,
#'   var = "raw_t1"
#' ) +
#'   layer_numeric(
#'     var = "score",
#'     clrsp = "Inferno",
#'     alpha = c(0.2, 0.6)
#'   )
#'
#' @export
layer_numeric <- function(var,
                          clrsp,
                          alpha = c(0.2, 0.45),
                          interpolate = .vbl_opt("interpolate"),
                          slices = NULL,
                          cond = NULL,
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

#' @keywords internal
.layer_numeric_impl <- function(vbl2D,
                                var,
                                clrsp,
                                alpha = c(0.2, 0.45),
                                interpolate = .vbl_opt("interpolate"),
                                ...){

  is_vartype(vbl2D, var = var, type = "numeric")

  list(
    ggnewscale::new_scale_fill(),
    ggplot2::geom_raster(
      data = vbl2D,
      mapping = ggplot2::aes(x = col, y = row, fill = .data[[var]]),
      alpha = eval_tidy_alpha(vbl2D, alpha = alpha, var = var),
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
#' @param alpha,color,linewidth,linetype Aesthetic settings for grid appearance.
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
#' @inherit vbl_doc_vbl_layer params return
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
#' @examples
#' vbl <- example_vbl()
#'
#' # Regular grid at 10-voxel spacing on both axes
#' ggplane(vbl, "axi", 90, var = "raw_t1") +
#'   layer_grid(col = 10, row = 10)
#'
#' # Relative spacing: grid every 10% of the maximum extent
#' ggplane(vbl, "axi", 90, var = "raw_t1") +
#'   layer_grid(col = 0.1, row = 0.1)
#'
#' # Only horizontal grid lines
#' ggplane(vbl, "axi", 90, var = "raw_t1") +
#'   layer_grid(col = NULL, row = 20)
#'
#' @export
layer_grid <- function(col = 0.1,
                       row = 0.1,
                       alpha = 0.2,
                       color = "lightgrey",
                       linewidth = 0.5,
                       linetype = "solid",
                       slices = NULL
                       ){

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
        slices = slices
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
                             linetype = "solid"
                             ){

  style_lst <-
    list(alpha = alpha, color = color, linewidth = linewidth, linetype = linetype) %>%
    purrr::map(.f = ~ if(length(.x) ==1 ){ rep(.x, 2) } else { .x } )

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


#' @title Add slice numbers to an offset ggvibble plot
#' @description Display the numeric slice index on each offset slice.
#' The placement and orientation of the slice number adapts automatically
#' to the offset direction used in the \link{ggplane()} call.
#'
#' @param pos Character specifying where the slice number should be placed
#' relative to the 2D slice. If `NULL`, valid options and default depend on `offset_dir`
#' from the \link{ggplane}() call:
#' \itemize{
#'   \item `"top"` or `"bottom"` when slices were offset horizontally (default: *'top'*)
#'   \item `"left"` or `"right"` when slices were offset vertically (default: *'left'*)
#' }
#'
#' @param buffer Numeric offset controlling how far the slice number is placed
#' from the slice edge. Values `<1` are treated as proportions of the axis
#' range; values `>=1` are treated as absolute offsets. Direction depends on
#' `pos` and the offset orientation.
#'
#' @param wrap Optional character template used to format slice labels. Must be
#' of length 1. The placeholder `{slice}` is replaced with the numeric slice
#' value (e.g., `wrap = "Slice {slice}"`).
#'
#' @param angle Numeric rotation angle for the text label. If `NULL`,
#' orientation defaults to commonly readable angles depending on the chosen
#' `pos` (0°, 90°, 270°).
#'
#' @param alpha Numeric transparency in `[0,1]`.
#' @param color Color of the slice label.
#' @param size Text size passed to \link{geom_text}().
#' @param ... Additional arguments forwarded to \link{geom_text}() like `hjust`,
#' `vjust`, `fontsize`.
#' @inherit vbl_doc params
#'
#' @return A `ggvibble_layer` that annotates each slice with its slice number.
#'
#' @details
#' `layer_slice_number()` is designed for use with offset visualizations created
#' by \link{ggplane}(). Slice labels are positioned along the appropriate axis
#' depending on whether \link{vibble2D}() offset the slice horizontally or
#' vertically. The internal implementation computes:
#'
#' \itemize{
#'   \item the orientation (`"h"` or `"v"`) via \link{offset_dir}()
#'   \item default positions (`"top"`, `"bottom"`, `"right"`, `"left"`)
#'   \item default angles for readability
#'   \item the absolute plotting coordinates using \link{slice_num_col}() and
#'   \link{slice_num_row}()
#' }
#'
#' A custom label format can be supplied via `wrap`, and aesthetics (color,
#' transparency, size) behave as in \link{geom_text}().
#'
#' @seealso \link{ggplane}(), \link{offset_dir}(), \link{vibble2D}()
#'
#' @examples
#' vbl <- example_vbl()
#'
#' # Default slice numbering
#' ggplane(vbl, "axi", slices = 88:92, var = "raw_t1") +
#'   layer_slice_number()
#'
#' # Custom position and formatting
#' ggplane(vbl, "axi", slices = 88:92, var = "raw_t1") +
#'   layer_slice_number(pos = "top", wrap = "Slice {slice}", color = "yellow")
#'
#' @export
layer_slice_number <- function(pos = NULL,
                               buffer = 0.1,
                               wrap = NULL,
                               angle = NULL,
                               alpha = 0.8,
                               color = "white",
                               size = 3.5,
                               ...){

  vbl_layer(
    fun = function(vbl2D){

      .layer_slice_number_impl(
        vbl2D = vbl2D,
        pos = pos,
        wrap = wrap,
        buffer = buffer,
        angle = angle,
        alpha = alpha,
        color = color,
        size = size,
        ...
      )

    }
  )

}

#' @keywords internal
.layer_slice_number_impl <- function(vbl2D,
                                     pos = NULL,
                                     wrap = NULL,
                                     buffer = 0.05,
                                     angle = NULL,
                                     alpha = 0.8,
                                     color = "white",
                                     size = 3.5,
                                     ...){

  # horizontal offset vs vertical
  offset_axis <- ifelse(grepl("left|right", offset_dir(vbl2D)), "h", "v")

  # relative positioning
  pos <- if(is.null(pos)){ ifelse(offset_axis == "h", "top", "left") } else { pos }

  if(offset_axis == "h"){

    pos <- match.arg(pos, choices = c("top", "bottom"))

  } else if(offset_axis == "v"){

    pos <- match.arg(pos, choices = c("right", "left"))

  }

  # angle
  defaults <- c("top" = 0, "bottom" = 0, "right" = 270, "left" = 90)
  angle <- ifelse(is.numeric(angle), angle, defaults[pos])

  # absolute positioning
  df <-
    dplyr::group_by(vbl2D, slice) %>%
    dplyr::select(slice, col, row) %>%
    dplyr::summarise(
      col = .slice_num_col(col, pos, offset_axis, buffer),
      row = .slice_num_row(row, pos, offset_axis, buffer),
      .groups = "drop"
    )

  adj <- ifelse(offset_axis == "h", "row", "col")

  df[[adj]] <- ifelse(pos %in% c("right", "top"), min(df[[adj]]), max(df[[adj]]))

  if(is.character(wrap)){

    stopifnot(length(wrap) == 1)

    df[["label"]] <- as.character(glue::glue(wrap, slice = df$slice))

  } else {

    df[["label"]] <- as.character(df$slice)

  }

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









