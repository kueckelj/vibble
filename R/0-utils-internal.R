# Misc internal helpers.

#' @keywords internal
#' @importFrom dplyr arrange filter group_by mutate rename select slice summarise summarize transmute ungroup
NULL

#' @keywords internal
#' @importFrom ggplot2 alpha

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`


#' @keywords internal
.get_var_limits <- function(vbl2D, var, verbose = vbl_opts("verbose")){

  out <- var_limits(vbl2D, var)

  if(is.null(out)){

    out <- var_range(vbl2D, var)

    rlang::inform(
      message = glue::glue("Set the limits of a numeric variable with `var_limits(vbl, var = '{var}') <- c(<min>, <max>)`."),
      .frequency = "regularly",
      .frequency_id = "get_var_limits"
    )

    ref <- stringr::str_c("c(", out[1], ", ", out[2], ")")
    .glue_message("No variable limits for '{var}'. Using current range {ref}.", verbose = verbose)

  }

  return(out)

}

#' @importFrom glue glue
#' @importFrom rlang caller_env
.glue_message <- function(..., defer = FALSE, verbose = vbl_opts("verbose")){

  if(verbose){

    msg <- glue::glue(..., .envir = caller_env(n = 1))

    if(is.numeric(defer)){

      withr::defer(message(msg), envir = caller_env(n = 1))

    } else {

      message(msg)

    }

  }


}


#' @importFrom glue glue
#' @importFrom rlang caller_env
.glue_warning <- function(..., .envir = caller_env(n=2), verbose = TRUE){

  if(verbose){

    .envir <- rlang::caller_env()
    msg <- glue::glue(..., .envir = .envir)

    warning(msg, call. = FALSE)

  }


}


#' @importFrom glue glue
#' @importFrom rlang caller_env
.glue_stop <- function(...){

  .envir <- rlang::caller_env()
  msg <- glue::glue(..., .envir = .envir)

  stop(msg, call. = FALSE)

}



#' @title Infer NIfTI datatype from vibble content.
#' @name vbl_doc_datatype
#'
#' @description
#' `.infer_datatype()` infers an appropriate NIfTI `datatype` code for a single
#' non-ccs variable in a vibble before writing it to disk.
#'
#' @details
#' The inference is based on the *actual values* of the variable, not on metadata.
#' `NA` values are ignored by default.
#'
#' The decision logic is:
#'
#' - **mask**: logical values or integer values restricted to `{0,1}`
#'   → `datatype = 2` (uint8)
#' - **categorical**: integer-like values beyond binary
#'   → `datatype = 2` (uint8) if ≤255 unique labels,
#'   → `datatype = 4` (int16) otherwise
#' - **numeric**: non–integer-like values
#'   → `datatype = 16` (float32)
#'
#' In non-strict mode, ambiguous or empty inputs fall back to `datatype = 16`.
#' In strict mode, unsupported or ambiguous inputs raise an error.
#'
#' @param var Character scalar. Name of a non-ccs variable in `vbl` whose content
#' is to be written into a NIfTI.
#' @param strict Logical. If `TRUE`, fail fast on empty, ambiguous, or unsupported
#' inputs instead of falling back to a conservative default.
#' @param na_rm Logical. If `TRUE`, ignore `NA` values when inferring the datatype.
#' @inherit vbl_doc params
#'
#' @return
#' An integer scalar giving the NIfTI datatype code (e.g. `2`, `4`, `16`).
#'
#' @keywords internal

#' @export
#' @rdname vbl_doc_datatype
.infer_datatype <- function(x, ...){

  UseMethod(".infer_datatype")

}

#' @export
#' @rdname vbl_doc_datatype
.infer_datatype.default <- function(x, ...){

  .infer_datatype_default(x, ...)

}

#' @export
#' @rdname vbl_doc_datatype
.infer_datatype.vbl <- function(x, var, ...){

  .infer_datatype.default(x[[var]], ...)

}

#' @keywords internal
.infer_datatype_default <- function(x,
                                    strict = FALSE,
                                    na_rm = TRUE){

  if(isTRUE(na_rm)){

    x <- x[!is.na(x)]

  }

  if(length(x) == 0){

    if(isTRUE(strict)){

      stop("Cannot infer datatype from empty variable.")

    }

    return(16L)

  }

  if(is.logical(x)) return(2L)

  if(is.integer(x)){

    ux <- unique(x)

    if(all(ux %in% c(0L,1L))) return(2L)

    if(length(ux) <= 255) return(2L)

    return(4L)

  }

  if(is.numeric(x)){

    if(isTRUE(strict)){

      # If it is numeric but integer-like throughout, treat as categorical/mask.
      is_int <- all(abs(x - round(x)) < .Machine$double.eps^0.5)

      if(isTRUE(is_int)){

        ux <- unique(as.integer(x))

        if(all(ux %in% c(0L,1L))) return(2L)

        if(length(ux) <= 255) return(2L)

        return(4L)

      }

    }

    return(16L)
  }

  if(isTRUE(strict)){

    stop("Cannot infer NIfTI datatype for variable (unsupported type).")

  }

  # Conservative fallback
  return(16L)

}






#' @keywords internal
.is_named <- function(x){

  names <- base::names(x)

  shiny::isTruthy(names)

}

#' @keywords internal
.keep_named <- function(x){

  x[purrr::map_lgl(.x = base::names(x), .f = ~ shiny::isTruthy(.x))]


}

#' @title Reorder CCS-LIP according to orientation.
#'
#' @description
#' Reorders a named object according to a given
#' orientation string by matching axis directions (R/L, S/I, A/P).
#'
#' @param x Named object (typically a list or vector) with names corresponding
#' to coordinate axes.
#' @param orientation Character. Three-letter orientation string (e.g. "RAS").
#'
#' @return
#' The input \code{x}, reordered to match the specified orientation.
.reorder_lip <- function(x, orientation){

  all(names(x) %in% vbl_ccs_axes)

  ccs_map <- ccs_orientation_mapping
  pointers <- split_orientation(orientation)

  order_out <- vector(mode = "character", length = 3)
  for(i in 1:3){

    order_out[i] <-
      purrr::keep(ccs_map, ~ pointers[i] %in% .x) %>%
      names()

  }

  order_out <- order_out[order_out %in% names(x)]
  x[order_out]

}


#' @keywords internal
.vbl_attr <- function(x, which){

  attr(x = x, which = which, exact = TRUE)

}

#' @title Internal confirmation helper for interactive operations
#' @description
#' Asks the user to confirm continuation of an operation and aborts on negative input.
#'
#' @param expr Logical condition that must be `TRUE` for the confirmation to be shown.
#' @param msg Character scalar with a message describing the operation to be confirmed.
#'
#' @return Invisibly returns `TRUE` if the operation may continue.
#'
#' @details
#' The helper checks the `vibble.ask` option via \link{vbl_opts}() before prompting.
#' If the user answers negatively, the function calls `rlang::abort()` and stops execution.
#'
#' @keywords internal
.vbl_confirm <- function(expr, msg){

  if(isTRUE(.vbl_opt("ask"))){

    if(isTRUE(expr)){

      answer <- askYesNo(msg = paste0(msg, " Do you want to proceed? Type either of"))

      if(!isTRUE(answer)){ rlang::abort("Operation aborted at user request.") }

    }

  }

  invisible(TRUE)

}


#' @keywords internal
.set_up_progress_bar <- function(total,
                                 format = "Progress: [:bar] :percent eta: :eta",
                                 clear = FALSE,
                                 width = 100,
                                 ...){

  progress::progress_bar$new(
    format = format,
    total = total,
    clear = clear,
    width = width,
    ...)

}

#' @title Split orientation string into axis letters.
#'
#' @description
#' Splits a 3-letter orientation code (e.g. "RAS", "LPI") into its individual
#' axis direction components after validation.
#'
#' @param orientation Character. Three-letter orientation string containing
#' exactly one of R/L, one of S/I, and one of A/P.
#'
#' @return
#' Character vector of length three with the orientation letters in order.
.split_orientation <- function(orientation){
  valid_orientation(orientation, error = TRUE)
  stringr::str_split_1(orientation, pattern = "")
}


#' @keywords internal
stopif <- function(cond) {
  if (isTRUE(cond)) {
    msg <- paste0(deparse(substitute(cond)), " is TRUE.")
    stop(msg, call. = FALSE)
  }
}



