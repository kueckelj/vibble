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

#' @title Layer for displaying slice numbers
#' @description
#' Add slice numbers as text labels to a 2D vibble (\code{vbl2D}). Labels are
#' positioned using an anchor specification and can be computed relative to
#' slice limits, screen limits, or their average. Optional alignment ensures
#' consistent label placement across slices.
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
#' @inherit vbl_doc_layer param return
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

    }
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

          bb2D <- slice_limits(vbl2D, slice)

        } else if(space == "screen"){

          bb2D <- screen_limits(vbl2D, slice)

        } else if(space == "avg"){

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
#'     \item \code{"slice"} – use per-slice limits from \link{slice_limits}.
#'     \item \code{"screen"} – use screen-space limits from \link{screen_limits} (after offsets).
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
#' @seealso \code{\link{slice_limits}}, \code{\link{screen_limits}},
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

    }
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

              lim0 <- screen_limits(vbl2D_ref, slice = slice)

            } else if(space == "slice") {

              lim0 <- slice_limits(vbl2D_ref, slice)

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







