#' @title Plot a ggvibble layout
#'
#' @description
#' S3 method for `plot()` that evaluates a `ggvibble_layout` by converting each
#' contained `ggvibble` to a ggplot (via `as_ggplot()`) and combining them using
#' the stored operators (`+`, `/`, `|`).
#'
#' @param x A `ggvibble_layout`.
#' @param ... Unused.
#'
#' @return Invisibly returns the composed plot object.
#' @export
plot.ggvibble_layout <- function(x, ...){

  stopifnot(inherits(x, "ggvibble_layout"))

  out <- as_ggplot(x)

  plot(out)

  invisible(out)

}


#' @title Initialize a ggvibble layout
#'
#' @description
#' Internal constructor for `ggvibble_layout`. Keeps only objects inheriting
#' from `ggvibble` and only operators in `c('+', '/', '|')`.
#'
#' @param plots A list of candidate plot objects; only `ggvibble` objects are kept.
#' @param ops A character vector of operators; only `+`, `/`, `|` are kept.
#'
#' @return A `ggvibble_layout` object (a list with `plots` and `ops`).
#' @keywords internal
.init_ggvibble_layout <- function(plots, ops){

  plots <- purrr::keep(plots, .p = ~ inherits(.x, "ggvibble"))
  ops <- purrr::keep(ops, .p = ~ .x %in% c("+", "/", "|"))

  if(length(plots) == 0){

    rlang::abort("No `ggvibble` objects supplied to `.init_ggvibble_layout()`.")

  }

  if(length(ops) != (length(plots) - 1)){

    rlang::abort(
      c(
        "Invalid `plots`/`ops` combination supplied to `.init_ggvibble_layout()`.",
        "i" = glue::glue("Expected `length(ops) == length(plots) - 1` but got {length(ops)} operators for {length(plots)} plots.")
      )
    )

  }

  out <- list(plots = plots, ops = ops)
  class(out) <- c("ggvibble_layout", "list")

  return(out)

}


#' @title Print a ggvibble layout
#'
#' @description
#' S3 method for `print()` that forwards to `plot()`.
#'
#' @param x A `ggvibble_layout`.
#' @param ... Passed to `plot()`.
#'
#' @return Invisibly returns `x`.
#' @export
print.ggvibble_layout <- function(x, ...){

  plot(x, ...)

  invisible(x)

}


#' @title Length of a ggvibble layout
#'
#' @description
#' S3 method for `length()` returning the number of contained plots.
#'
#' @param x A `ggvibble_layout`.
#'
#' @return An integer.
#' @export
length.ggvibble_layout <- function(x){

  length(x$plots)

}


#' @title Operators for ggvibble layouts
#'
#' @description
#' S3 method for the `Ops` group generic.
#'
#' Supported operators:
#' - `+` : add a `ggvibble` to the layout, or add `ggvibble_layer`(s) to the last plot.
#' - `/` : combine the layout with another `ggvibble` using `/`.
#' - `|` : combine the layout with another `ggvibble` using `|`.
#' - `*` : add a `ggvibble_layer` (or list of them) to all plots in the layout.
#'
#' @param e1 A `ggvibble_layout`.
#' @param e2 The right-hand side operand.
#'
#' @return A modified `ggvibble_layout`.
#' @export
Ops.ggvibble_layout <- function(e1, e2){

  x <- e1
  y <- e2

  if(!inherits(x, "ggvibble_layout")){

    rlang::abort("Left-hand side of `Ops.ggvibble_layout` must be a `ggvibble_layout`.")

  }

  if(!.Generic %in% c("+", "/", "|", "*")){

    rlang::abort(
      c(
        glue::glue("Operator `{.Generic}` is not implemented for `ggvibble_layout`."),
        "i" = "Supported operators are `+`, `/`, `|`, and `*`."
      )
    )

  }

  if(.Generic == "+"){

    if(inherits(y, "list")){

      y <- purrr::keep(y, .p = ~ inherits(.x, "ggvibble_layer"))

      if(length(y) == 0){

        rlang::abort(
          c(
            "No `ggvibble_layer` objects found in supplied list.",
            "i" = "Use `+` with a list of `ggvibble_layer` objects."
          )
        )

      }

      for(i in seq_along(y)){

        x$plots[[length(x)]]$layers <- c(x$plots[[length(x)]]$layers, list(y[[i]]))

      }

      return(x)

    } else if(inherits(y, "ggvibble_layer")){

      x$plots[[length(x)]]$layers <- c(x$plots[[length(x)]]$layers, list(y))
      return(x)

    } else if(inherits(y, "ggvibble")){

      x$plots <- c(x$plots, y)
      x$ops <- c(x$ops, "+")

      return(x)

    }

    rlang::abort(
      c(
        glue::glue("Invalid object supplied to `{.Generic}.ggvibble_layout`."),
        "i" = "Use `+` to add ggvibble layers (to the last plot) or add another ggvibble to the layout."
      )
    )

  }

  if(.Generic == "/"){

    if(inherits(y, "ggvibble")){

      x$plots <- c(x$plots, y)
      x$ops <- c(x$ops, "/")

      return(x)

    }

    ic <-
      stringr::str_c(class(y), collapse = "', '") %>%
      stringr::str_c("c('", ., "')")

    rlang::abort(
      c(
        glue::glue("Invalid object supplied of class {ic} to `/.ggvibble_layout`."),
        "i" = "Use `/` to combine a `ggvibble_layout` with a `ggvibble`."
      )
    )

  }

  if(.Generic == "|"){

    if(inherits(y, "ggvibble")){

      x$plots <- c(x$plots, y)
      x$ops <- c(x$ops, "|")

      return(x)

    }

    ic <-
      stringr::str_c(class(y), collapse = "', '") %>%
      stringr::str_c("c('", ., "')")

    rlang::abort(
      c(
        glue::glue("Invalid object supplied of class {ic} to `|.ggvibble_layout`."),
        "i" = "Use `|` to combine a `ggvibble_layout` with a `ggvibble`."
      )
    )

  }

  if(.Generic == "*"){

    if(inherits(y, "ggvibble_layer")){

      for(i in seq_along(x$plots)){

        x$plots[[i]] <- x$plots[[i]] + list(y)

      }

      return(x)

    } else if(inherits(y, "list")){

      y <- purrr::keep(y, .p = ~ inherits(.x, "ggvibble_layer"))

      if(length(y) == 0){

        rlang::abort(
          c(
            "No `ggvibble_layer` objects found in supplied list.",
            "i" = "Use `*` with a list of `ggvibble_layer` objects."
          )
        )

      }

      for(i in seq_along(x$plots)){

        for(ii in seq_along(y)){

          x$plots[[i]] <- x$plots[[i]] + y[[ii]]

        }

      }

      return(x)

    }

    ic <-
      stringr::str_c(class(y), collapse = "', '") %>%
      stringr::str_c("c('", ., "')")

    rlang::abort(
      c(
        glue::glue("Invalid object supplied of class {ic} to `*.ggvibble_layout`."),
        "i" = "Use `*` to add a `ggvibble_layer` (or a list of them) to all plots in the layout."
      )
    )

  }

}
