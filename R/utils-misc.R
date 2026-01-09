

descr_dim <- function(vbl, collapse = "; "){

  d <-
    ccs_limits(vbl) %>%
    purrr::map_chr(.f = ~ as.character(diff(.x))) %>%
    stringr::str_c(., collapse = " x ")

  s <-
    ccs_steps(vbl) %>%
    purrr::map(.f = ~ round(.x, 2)) %>%
    stringr::str_c(., collapse = " x ") %>%
    stringr::str_c(., " (mm)")

  paste0(d, " | ", s)

}


#' @title Compute the midpoint of a limit
#'
#' @description
#' Computes the numeric midpoint of a valid limit object.
#'
#' @param limit A \link[=is_limit]{limit}.
#'
#' @return A numeric scalar giving the midpoint of `limit`.
#'
#' @export
mid <- function(limit){

  .stop_if_not(is_limit(limit))

  min(limit) + diff(limit)/2

}


#' @title Generate slice sequences based on voxel-wise conditions
#' @description Produces a sequence of slice indices for use in \link{ggplane}()
#' and the `seq_*` family of helper functions.
#'
#' @param .cond Logical expression evaluated \link[rlang:args_data_masking]{tidyverse's data-masking}
#' semantics on the \link{vibble2D}() representation (e.g., `expr = tumor`,
#' `expr = raw_t1 > 0.5`). Determines which slices are selected.
#' @param buffer Numeric. If `buffer > 0`, the slice range is expanded before
#' generating the sequence. If `buffer < 1`, it is interpreted as a proportion
#' of the number of matched slices; if `buffer >= 1`, it is interpreted as an
#' absolute number of slices.
#' @param ... Additional arguments passed to \link{seq}(), such as `length.out`.
#' @inherit vbl_doc params
#'
#' @return An integer vector of slice indices.
#'
#' @details
#' The function works in three steps:
#' \itemize{
#'   \item Converts the vibble to 2D using \link{vibble2D}().
#'   \item Identifies all slices containing voxels that satisfy `cond`.
#'   \item Optionally expands this range using `buffer`, then generates a
#'   regular sequence using \link{seq}().
#' }
#'
#' `seq_slices()` is part of the `seq_*` family, which provides convenient
#' ways to generate axis-based sequences (e.g., for slicing, animation, or
#' stepping through the volume). This function focuses specifically on
#' condition-based slice selection.
#'
#' @seealso \link{ggplane}(), \link{vibble2D}(), \link{seq}
#'
#' @examples
#' vbl <- example_vbl()
#'
#' # All slices containing tumor voxels
#' seq_slices(vbl, plane = "axi", cond = tumor)
#'
#' # Expand by 3 slices on each side
#' seq_slices(vbl, "axi", cond = tumor, buffer = 3)
#'
#' # Use a proportional buffer and step through slices by 2
#' seq_slices(vbl, "axi", cond = tumor, buffer = 0.2, by = 2)
#'
#' @export

seq_slices <- function(vbl, plane, .cond = NULL, .by = NULL, buffer = 0, ...){

  vbl2D <- vibble2D(vbl, plane = plane)

  .by_quo <- rlang::enquo(.by)
  .cond_quo <- rlang::enquo(.cond)

  if(!rlang::quo_is_null(.cond_quo)){

    vbl2D <- dplyr::filter(vbl2D, !!.cond_quo, .by = {{ .by }})

  }

  seq_out <- slices(vbl2D)

  if(buffer > 0){

    if(buffer < 1){

      buffer <- ceiling(length(seq_out)*buffer)

    }

    front <- (min(seq_out)-buffer):(min(seq_out)-1)
    back <- (max(seq_out)+1):(max(seq_out)+buffer)

    seq_out <- c(front, seq_out, back)

    lim_slice <- var_smr(vbl2D, var = "slice")$limits
    seq_out <- seq_out[within_limits(seq_out, lim_slice)]

  }

  seq_out <- seq(from = min(seq_out), to = max(seq_out), ...)
  seq_out <- floor(seq_out)

  return(seq_out)

}

#' @export
seq_range <- function(x){

    r <- range(x)
    min(r):max(r)

}


#' @export
theme_vbl <- function(){

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
    strip.background = ggplot2::element_blank(),# ggplot2::element_rect(fill = "black", color = "black"),
    strip.text = ggplot2::element_blank()#ggplot2::element_text(color = "white")
  )

}


#' @title Use vibble default
#'
#' @description
#' Sentinel value indicating that a parameter should use the global vibble
#' default as defined via vbl_opts().
#'
#' @return
#' An internal sentinel object.
#'
#' @export
vbl_def <- function(){

  structure(list(), class = "vbl_def")

}

