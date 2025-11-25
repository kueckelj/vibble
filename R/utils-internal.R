# Misc internal helpers.


#' @importFrom glue glue
#' @importFrom rlang caller_env
glue_message <- function(..., defer = FALSE, verbose = TRUE){

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
glue_warning <- function(..., .envir = caller_env(), verbose = TRUE){

  if(!verbose){

    return(invisible(NULL))

  }

  warning(glue(..., .envir = .envir), call. = FALSE)

}


#' @importFrom glue glue
#' @importFrom rlang caller_env
glue_stop <- function(..., envir = caller_env()){

  stop(glue(..., envir = .envir), call. = FALSE)

}


rgx <- list(
  img_types = ".*_",
  vv_type = "_.*_"
)

set_up_progress_bar <- function(total,
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


stopif <- function(cond) {
  if (isTRUE(cond)) {
    msg <- paste0(deparse(substitute(cond)), " is TRUE.")
    stop(msg, call. = FALSE)
  }
}

vbl_ccs_axes <- c("x", "y", "z")

vbl_data_var_types <- c("label", "mask", "numeric")

vbl_planes <- c("Sagittal" = "sag", "Axial" = "axi", "Coronal" = "cor")

within_limits <- function(x, l){

  x > min(l) & x < max(l)

}
