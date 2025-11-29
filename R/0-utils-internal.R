# Misc internal helpers.

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @importFrom glue glue
#' @importFrom rlang caller_env
.glue_message <- function(..., defer = FALSE, verbose = TRUE){

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
.glue_warning <- function(..., .envir = caller_env(), verbose = TRUE){

  if(!verbose){

    return(invisible(NULL))

  }

  warning(glue(..., .envir = .envir), call. = FALSE)

}


#' @importFrom glue glue
#' @importFrom rlang caller_env
.glue_stop <- function(..., envir = caller_env()){

  stop(glue(..., envir = .envir), call. = FALSE)

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


#' @keywords internal
stopif <- function(cond) {
  if (isTRUE(cond)) {
    msg <- paste0(deparse(substitute(cond)), " is TRUE.")
    stop(msg, call. = FALSE)
  }
}



