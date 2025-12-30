# Specific options for global behaviour.

#' @title Internal helper to read vibble options
#' @description
#' Retrieves a single vibble option with an optional default value.
#'
#' @param name Character scalar with the option name without the `vibble.` prefix.
#' @param default Default value returned if the option is not set.
#'
#' @return The current value of the requested option or `default` if unset.
#'
#' @details
#' This function is a thin wrapper around `getOption()` for internal use.
#' It should not be called directly by users.
#'
#' @keywords internal
.vbl_opt <- function(name, default = NULL){

  getOption(paste0("vibble.", name), default)

}



#' @title Manage vibble options
#' @description
#' Sets or retrieves global options that control the behaviour of vibble functions.
#'
#' @param ... Named arguments to set options or a single unnamed character to retrieve a specific option.
#'
#' @return Returns a named list of current vibble options when called without arguments,
#' the value of one option when called with a single character input, or invisibly returns
#' previous values when setting options.
#'
#' @details
#' Options are stored using standard R options with the prefix `vibble.`.
#' `vbl_opts()` behaves differently depending on how it is called.
#'
#' \itemize{
#'   \item `vbl_opts()` returns all current `vibble.*` options.
#'   \item `vbl_opts("verbose")` retrieves the value of `vibble.verbose`.
#'   \item `vbl_opts(verbose = FALSE, ask = TRUE)` sets options and returns old values invisibly.
#' }
#'
#' @note
#' Options can also be set persistently via `.Rprofile` using `options(vibble.verbose = FALSE)`.
#'
#' @examples
#' vbl_opts()
#' vbl_opts("verbose")
#' vbl_opts(verbose = FALSE)
#'
#' @export

vbl_opts <- function(...){

  dots <- list(...)

  # no args → return all vibble.* options
  if(length(dots) == 0){

    op <- options()
    idx <- grep("^vibble\\.", names(op))
    return(op[idx])

  }

  # getter: vbl_opts("verbose")
  if(length(dots) == 1 && is.null(names(dots))){

    nm <- dots[[1]]
    stopifnot(is.character(nm))
    return(getOption(paste0("vibble.", nm)))

  }

  # setter: vbl_opts(verbose=FALSE, ask=TRUE)
  if(any(names(dots) == "")){

    stop("All arguments must be named when setting options.", call.=FALSE)

  }

  new <- stats::setNames(dots, paste0("vibble.", names(dots)))

  old <- options(new)

  invisible(old[names(new)])

}

#' @export
vbl_opts_default <-
  list(
    vibble.allow.write = FALSE,
    vibble.alpha.grid = 0.25,
    vibble.alpha.labels = 0.85,
    vibble.ask = TRUE,
    vibble.clip.offset = TRUE,
    vibble.clrp = "hue_pal",
    vibble.clrsp = c("black", "white"),
    vibble.expand.fraction = 0.05,
    vibble.expand.ggplane = 0.075,
    vibble.expand.data.bb = as_abs(0.5),
    vibble.expand.slice.bb = as_abs(0.5),
    vibble.label.spacer = 0.75,
    vibble.legend.name.bb = "Box",
    vibble.legend.name.mask = "Mask",
    vibble.legend.name.outline = "Outline",
    vibble.interpolate = TRUE,
    vibble.n.slices = 6,
    vibble.plane = "axi",
    vibble.size.ref = "device",
    vibble.size.text = 3.5,
    vibble.slices.n = 6,
    vibble.verbose = TRUE
  )
