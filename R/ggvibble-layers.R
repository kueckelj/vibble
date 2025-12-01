# ggvibble overlays and themes


# non-data ----------------------------------------------------------------

#' @title Add a data-driven 2D bounding-box
#' @description Draw a rectangular bounding box around the extent of voxels
#' that match a certain condition.
#'
#' @param color Color of the bounding-box outline.
#' @param fill Fill of the bounding-box.
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
                     fill = NA,
                     expand = FALSE,
                     .cond = NULL,
                     .by = NULL,
                     ...){

  .cond_quo <- rlang::enquo(.cond)

  vbl_layer(
    fun = function(vbl2D){

      vbl2D <- .filter_layer(vbl2D, .cond = .cond_quo, .by = .by, layer = "layer_bb()")

      .layer_bb_impl(
        vbl2D = vbl2D,
        color = color,
        fill = fill,
        expand = expand,
        ...
      )

    }
  )

}


#' @keywords internal
.layer_bb_impl <- function(vbl2D,
                           color,
                           fill,
                           expand = FALSE,
                           ...){

  list(
    ggplot2::geom_rect(
      data = bb2D_df(vbl2D, expand = expand),
      mapping = ggplot2::aes(xmin = cmin, xmax = cmax, ymin = rmin, ymax = rmax),
      color = color,
      fill = fill,
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

  .cond_quo <- rlang::enquo(.cond)

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
      col = .grid_intercepts(col, limits = attr(vbl2D, "var_smr")[["col"]]$limits),
      row = .grid_intercepts(row, limits = attr(vbl2D, "var_smr")[["row"]]$limits)
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


#' @title Layers for visualizing 2D limit regions
#' @name vbl_doc_plotting_regions
#' @description
#' Add rectangular overlays showing different 2D limit regions of a \code{vbl2D}
#' object. These layers visualize:
#'
#' \itemize{
#'   \item \code{layer_plot_limits()}: The global plot limits spanning all slices
#'     after applying screen-space offsets.
#'
#'   \item \code{layer_screen_limits()}: The screen-space limits of each slice,
#'     optionally restricted to selected slices.
#'
#'   \item \code{layer_slice_limits()}: The raw per-slice bounding boxes
#'     (voxel extents) without offsets or screen-space adjustments.
#' }
#'
#' Each function draws one or more \code{geom_rect()} layers representing the
#' corresponding limit region in the 2D layout.
#'
#' @param color Line color of the drawn rectangles.
#' @param fill Fill color for the rectangles. Defaults to \code{NA}.
#' @param slices Optional integer vector of slice values to visualize
#'   (for \code{layer_screen_limits()} and \code{layer_slice_limits()}).
#' @param ... Additional arguments passed to \code{geom_rect()}.
#'
#' @return A list of ggplot2 layers.
#'
#' @seealso \code{\link{plot_limits}()}, \code{\link{screen_limits}()},
#'   \code{\link{slice_limits}()}
#'
NULL

#' @rdname vbl_doc_plotting_regions
#' @export
layer_plot_limits <- function(color = alpha("yellow", 0.7), fill = NA, ...){

  vbl_layer(
    fun = function(vbl2D){

      .layer_plot_limits_impl(
        vbl2D = vbl2D,
        color = color,
        fill = fill,
        ...
      )

    }
  )

}

#' @keywords internal
.layer_plot_limits_impl <- function(vbl2D, color, fill, ...){

  data <- as_bb2D_df(plot_limits(vbl2D))

  list(
    ggplot2::geom_rect(
      data = data,
      mapping = ggplot2::aes(xmin = cmin, xmax = cmax, ymin = rmin, ymax = rmax),
      color = color,
      fill = fill,
      ...
    )
  )

}

#' @rdname vbl_doc_plotting_regions
#' @export
layer_screen_limits <- function(color = alpha("blue", 0.7),
                                fill = NA,
                                slices = NULL,
                                ...){

  if(is.numeric(slices)){

    vbl2D <- dplyr::filter(vbl2D, slice %in% slices)

    }

  vbl_layer(
    fun = function(vbl2D){

      .layer_screen_limits_impl(
        vbl2D = vbl2D,
        color = color,
        fill = fill,
        ...
      )

    }
  )

}

#' @keywords internal
#' @rdname layer_screen_limits
.layer_screen_limits_impl <- function(vbl2D,
                                      color,
                                      fill,
                                      ...){

  data <-
    purrr::map_df(
      .x = slices(vbl2D),
      .f = function(slice){

        ls <- screen_limits(vbl2D, slice = slice)

        tibble::tibble(
          slice = slice,
          cmin = ls$col[1],
          cmax = ls$col[2],
          rmin = ls$row[1],
          rmax = ls$row[2]
        )

      }
    )

  list(
    ggplot2::geom_rect(
      data = data,
      mapping = ggplot2::aes(xmin = cmin, xmax = cmax, ymin = rmin, ymax = rmax),
      color = color,
      fill = fill,
      ...
    )
  )

}

#' @rdname vbl_doc_plotting_regions
#' @export
layer_slice_limits <- function(color = alpha("red", 0.7),
                               fill = NA,
                               slices = NULL,
                               ...){

  if(is.numeric(slices)){

    vbl2D <- dplyr::filter(vbl2D, slice %in% slices)

  }

  vbl_layer(
    fun = function(vbl2D){

      .layer_bb_impl(
        vbl2D = vbl2D,
        color = color,
        fill = fill,
        ...
      )

    }
  )

}


#' @keywords internal
.layer_slice_limits_impl <- function(vbl2D,
                                     color,
                                     fill,
                                     ...){

  list(
    ggplot2::geom_rect(
      data = bb2D_df(vbl2D, expand = FALSE),
      mapping = ggplot2::aes(xmin = cmin, xmax = cmax, ymin = rmin, ymax = rmax),
      color = color,
      fill = fill,
      ...
    )
  )

}

#' @title Add slice-number labels
#' @description
#' Draw slice numbers as text labels on each slice of a `ggvibble`. Supports both
#' regular slice layouts and offset layouts produced by `offset_dist` / `offset_dir`
#' in the \link{ggplane}() call.
#'
#' @param anchor Anchor position for placing the label.
#'   Accepted inputs differ between offset and non-offset layouts:
#'
#'   \itemize{
#'     \item{ Offset layout:}{ Character anchors only.<br>
#'       If slices are offset left–right, valid anchors are `c("top", "bottom")`.<br>
#'       If slices are offset top–bottom, valid anchors are `c("left", "right")`.<br>
#'       Defaults are `"top"` for left–right offsets and `"left"` for top–bottom offsets.}<br>
#'     \item{Non-offset layout:}{ Any relative or absolute \link[=is_img_anchor]{image anchor}.
#'       Supports named anchors (e.g. `"top-left"`) and numeric anchors.}
#'   }
#'
#' @param align Logical. If `TRUE`, ensures slice numbers align across slices when offsets are applied.
#'
#' @param wrap Optional. Character template for constructing labels. Uses glue syntax, e.g. `"{slice}"`.
#' @param angle Rotation angle of the text.
#' @param alpha,color,size Passed to the respective standard parameters
#' of \link{geom_text}() to control opacity, color and size of the drawn labels.
#' @param ... Additional arguments passed to `ggplot2::geom_text()`.
#'
#' @inherit vbl_doc_layer return
#'
#' @details
#' Two internal implementations exist depending on whether the `vbl2D` has slice offsets
#' as determined by `offset_dist` and `offset_dir` in `ggplane()`.
#'
#' **Non-offset layout:**
#' \itemize{
#'   \item Uses the provided `anchor` directly (`rel` or `abs`).
#'   \item Converts it to absolute coordinates via `as_img_anchor_abs(anchor, lim(vbl2D))`.
#'   \item Places labels exactly at the slice-specific anchoring point.
#'   \item Simple, consistent placement for grid/faceted layouts.
#' }
#'
#' **Offset layout:**
#' \itemize{
#'   \item If `anchor` is missing, a default is chosen along the axis *orthogonal* to the offset direction
#'     (e.g. `"top"` for left–right offsets, `"left"` for top–bottom offsets).
#'   \item Anchors must be **character names**, not numeric.
#'   \item Anchor coordinates are derived slice-wise from `slice_bb()`.
#'   \item If `align = TRUE`, slice labels are aligned along the axis *not* affected by the offset
#'     to avoid staggered-looking label positions.
#'   \item Additional nudging is applied to place the label visually centered between the slice bounding box
#'     and the overall plotting region (`lim_plot()`).
#' }
#'
#' In short:
#' **non-offset = direct placement at a global anchor**,
#' **offset = anchor restricted to the offset geometry, with optional cross-slice alignment and nudging**.
#'
#' @export
layer_slice_number <- function(anchor,
                               to = "avg",
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
        to = to,
        align = align,
        wrap = wrap,
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
                                     anchor,
                                     to,
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

        if(to == "slice"){

          bb2D <- slice_limits(vbl2D, slice)

        } else if(to == "screen"){

          bb2D <- screen_limits(vbl2D, slice)

        } else if(to == "avg"){

          bb2D <- avg_bb2D(
            a = slice_limits(vbl2D, slice),
            b = screen_limits(vbl2D, slice)
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
                    no = ifelse(idx == 2, max, min)
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










