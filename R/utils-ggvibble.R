# Utility functions within the ggvibble framework



# Public API --------------------------------------------------------------

#' @title Expanding limits
#' @description
#' Validate and classify `expand` specifications used to modify \link[=is_limit]{limits}.
#'
#' @param x Object to test for being a valid expand specification.
#'
#' @return
#' * `is_expand()`: Logical scalar, `TRUE` if `x` is a valid expand specification.
#' * `is_expand_lgl()`: Logical scalar, `TRUE` if `x` is interpreted as a logical expand rule.
#' * `is_expand_abs()`: Logical scalar, `TRUE` if `x` is interpreted as an absolute expand rule.
#' * `is_expand_rel()`: Logical scalar, `TRUE` if `x` is interpreted as a relative expand rule.
#'
#' @details
#' Expand specifications are vectors of length one or two. If length one, and a second value is
#' required (as in expansions along col/row) the value is recycled.
#'
#' Valid expand types are.
#' \itemize{
#'   \item Absolute: `is_expand_abs(x)` is `TRUE` for \link[=is_abs]{absolute} inputs of length one or two.
#'     Values are interpreted as absolute units added to both sides of the limits.
#'   \item Relative: `is_expand_rel(x)` is `TRUE` for  \link[=is_rel]{relative} inputs of length one or two.
#'     Values are interpreted as proportions of the current range added to both sides.
#'   \item Logical: `is_expand_lgl(x)` is `TRUE` for logical inputs of length one or two.
#'     A `TRUE` value expands each side by a fixed fraction of the current range. The factor
#'     is taken from `vbl_opts("expand.fraction")`.
#' }
#'
#' `is_expand(x)` returns `TRUE` if any of the above tests is satisfied.
#'
#' @examples
#' is_expand(TRUE)
#' is_expand(c(2L, 4L))
#' is_expand(c(0.1, 0.2))
#'
#' is_expand_lgl(TRUE)
#' is_expand_abs(2L)
#' is_expand_rel(0.05)
#'
#' @name vbl_doc_expand
NULL


#' @rdname vbl_doc_expand
#' @export
is_expand <- function(x){

  is_expand_lgl(x) |
  is_expand_abs(x) |
  is_expand_rel(x)

}

#' @rdname vbl_doc_expand
#' @export
is_expand_lgl <- function(x){

  length(x) %in% 1:2 && isTRUE(x) | isFALSE(x)

}

#' @rdname vbl_doc_expand
#' @export
is_expand_abs <- function(x){

  length(x) %in% 1:2 && is_abs(x)

}

#' @rdname vbl_doc_expand
#' @export
is_expand_rel <- function(x){

  length(x) %in% 1:2 && is_rel(x)

}



#' @rdname vbl_doc_img_anchors
#' @export
is_img_anchor <- function(x){

  is_img_anchor_abs(x) |
    is_img_anchor_chr(x) |
    is_img_anchor_rel(x)

}

#' @rdname vbl_doc_img_anchors
#' @export
is_img_anchor_abs <- function(x){

  is.numeric(x) && is.integer(x) && length(x) == 2

}

#' @rdname vbl_doc_img_anchors
#' @export
is_img_anchor_chr <- function(x){

  is.character(x) && length(x) == 1 && x %in% names(img_anchors)

}

#' @rdname vbl_doc_img_anchors
#' @export
is_img_anchor_rel <- function(x){

  is.numeric(x) && !is.integer(x) && length(x) == 2

}

#' @rdname vbl_doc_img_anchors
#' @export
as_img_anchor_abs <- function(anchor, bb2D){

  stopifnot(is_img_anchor(anchor))
  stopifnot(is_bb2D(bb2D))

  if(is_img_anchor_chr(anchor)){

    anchor <- match.arg(anchor, choices = names(img_anchors))
    anchor <- img_anchors[[anchor]]

  }

  out <- vector(mode = "numeric", length = 2)

  if(is_img_anchor_rel(anchor)){

    out[[1]] <- min(bb2D$col) + (diff(bb2D$col) * anchor[[1]])
    out[[2]] <- max(bb2D$row) - (diff(bb2D$row) * anchor[[2]])

  } else if(is_img_anchor_abs(anchor)){

    out[[1]] <- anchor[[1]]
    out[[2]] <- anchor[[2]]

  }

  names(out) <- c("col", "row")

  return(out)

}


# internal ----------------------------------------------------------------

#' @title Internal expand helpers
#' @description
#' Internal utilities to expand a 2D limit vector according to absolute, relative,
#' or logical expand rules. These functions form the low-level backend used by
#' higher-level helpers such as \link{expand_lim2D}().
#'
#' @param limit Numeric vector of length two giving the current axis limits.
#' @param expand Expand specification, validated by `is_expand()`.
#'   Interpreted as absolute, relative, or logical depending on
#'   `is_expand_abs()`, `is_expand_rel()`, and `is_expand_lgl()`.
#'
#' @return
#' A numeric vector of length two containing the updated limits.
#' `.apply_expand()` dispatches to one of the specialized helpers.
#'
#' @details
#' Expansion rules.
#' \itemize{
#'   \item **Logical** (`.apply_expand_lgl()`): Expands each side by a fixed fraction of the current range.
#'     The fraction is taken from `vbl_opts("expand.fraction")`.
#'   \item **Absolute** (`.apply_expand_abs()`): Adds `expand` units to each side.
#'   \item **Relative** (`.apply_expand_rel()`): Adds `expand * diff(limit)` units to each side.
#' }
#'
#' `.apply_expand()` acts as a simple dispatcher, selecting the correct rule based on
#' the type of `expand`.
#'
#' @keywords internal
#' @name vbl_doc_expand_apply
NULL

#' @rdname vbl_doc_expand_apply
.apply_expand <- function(limit, expand){

  stopifnot(is_expand(expand))

  if(is_expand_abs(expand)){

    .apply_expand_abs(limit, expand)

  } else if(is_expand_rel(expand)){

    .apply_expand_rel(limit, expand)

  } else if(is_expand_lgl(expand)){

    .apply_expand_lgl(limit, expand)

  }

}

#' @rdname vbl_doc_expand_apply
.apply_expand_lgl <- function(limit, expand){

  dl <- diff(limit)

  limit[1] <- ifelse(expand, limit[1] - dl * vbl_opts("expand.fraction"), limit[1])
  limit[2] <- ifelse(expand, limit[2] + dl * vbl_opts("expand.fraction"), limit[2])

  return(limit)

}

#' @rdname vbl_doc_expand_apply
.apply_expand_abs <- function(limit, expand){

  limit[1] <- limit[1] - expand
  limit[2] <- limit[2] + expand

  return(limit)

}

#' @rdname vbl_doc_expand_apply
.apply_expand_rel <- function(limit, expand){

  dl <- diff(limit)

  limit[1] <- limit[1] - dl * expand
  limit[2] <- limit[2] + dl * expand

  return(limit)

}



#' @keywords internal
.densify_poly <- function(df, n = 50){

  df_closed <- rbind(df, df[1, ])

  purrr::map_dfr(
    1:(nrow(df_closed)-1),
    function(i){
      col <- seq(df_closed$col[i], df_closed$col[i+1], length.out = n)
      row <- seq(df_closed$row[i], df_closed$row[i+1], length.out = n)
      tibble(col = col, row = row)
    }
  )
}

#' @keywords internal
.eval_tidy_opacity <- function(vbl2D, opacity, var){

  opacity_inp <- rlang::eval_tidy(opacity, data = vbl2D)

  if(is.numeric(opacity_inp)){

    if(length(opacity_inp) == 1){

      opacity_use <- opacity_inp

    } else if(length(opacity_inp) == 2) {

      if(!is_vartype(vbl2D, var, "numeric", fdb = NULL)){

        stop("If `opacity`is of length 2, `var` must be of type numeric.")

      }

      opacity_use <-
        scales::rescale(
          x = vbl2D[[var]],
          to = c(pmax(0, min(opacity_inp)), pmin(1, max(opacity_inp))),
          from = var_smr(vbl2D, var)$limits
        )

    } else if(length(opacity_inp) == nrow(vbl2D)) {

      opacity_lim <- range(opacity_inp, na.rm = TRUE)

      if(!within_limits(opacity_lim[1], c(0,1))){ opacity_lim[1] <- 0 }
      if(!within_limits(opacity_lim[2], c(0,1))){ opacity_lim[2] <- 1 }

      opacity_use <- scales::rescale(x = opacity_inp, to = opacity_lim)

    } else {

      stop("Invalid input for `opacity`.")

    }

  } else {

    stop("Invalid input for `opacity`.")

  }

  return(opacity_use)

}


#' @keywords internal
.filter_layer <- function(vbl2D, .cond, .by = NULL, layer = "layer()"){

  # cond is expected to be a quosure (possibly representing NULL)
  if(!rlang::quo_is_null(.cond)){

    vbl2D <- dplyr::filter(vbl2D, !!.cond, .by = {{ .by }})

  }

  if(nrow(vbl2D) == 0){

    expr_text <- rlang::as_label(rlang::get_expr(.cond))
    msg <- glue::glue("No voxels remain in {layer} after filtering with `{expr_text}`.")

    stop(msg, call. = FALSE)

  }

  return(vbl2D)

}


#' @keywords internal
.grid_intercepts <- function(input, limits){

  if(length(input) > 1){

    out <- input

  } else if(length(input) == 1){

    if(input < 1){

      input <- max(limits)*input

    }

    half <- max(limits)/2

    icpts1 <- half
    while(max(icpts1) < max(limits)){

      n <- length(icpts1)
      icpts1[n + 1] <- half + input*n

    }

    icpts2 <- half
    while(min(icpts2) > 0){

      n <- length(icpts2)
      icpts2[n + 1] <- half - input*n

    }

    out <- unique(c(icpts1, icpts2))

  } else {

    out <- NULL

  }

  return(out)

}


#' @keywords internal
.ratio2D <- function(vbl2D){

  axes <- req_axes_2D(plane(vbl2D))

  col <- max(ccs_limits(vbl2D)[[axes[["col"]]]])

  row <- max(ccs_limits(vbl2D)[[axes[["row"]]]])

  col/row

}

