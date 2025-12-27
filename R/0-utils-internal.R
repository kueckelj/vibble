# Misc internal helpers.

#' @keywords internal
#' @importFrom dplyr arrange filter group_by mutate rename select slice summarise summarize transmute ungroup
NULL

#' @keywords internal
#' @importFrom ggplot2 alpha
NULL

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @keywords internal
.check_allow_write <- function(){

  if(!isTRUE(vbl_opts("allow.write"))){

    rlang::abort(
      c(
        "This operation requires permission to write files.",
        "i" = "File system writes are disabled by default to avoid unintended side effects.",
        "i" = "Set `vbl_opts('allow.write' = TRUE)` to explicitly allow writing files."
      )
    )

  }

  invisible(TRUE)

}

#' @keywords internal
.calling_function_name <- function(n = 1){

  as.character(sys.call(-n)[[1]])

}

#' @keywords internal
.check_patchwork_attached <- function(){

  if(!.is_attached("patchwork")){

    rlang::abort(
      c(
        "Cannot combine `ggvibble` objects with `+`.",
        "i" = "The patchwork package must be attached for plot composition.",
        "i" = "Use `library(patchwork)`."
      )
    )

  }

  invisible(TRUE)

}

#' @keywords internal
.slice_collapse <- function(x, last = " or "){

  glue::glue_collapse(x, sep = ", ", last = last)

}

#' @keywords internal
.str_collapse <- function(x, last = "' or '"){

  glue::glue_collapse(x, sep = "', '", last = last)

}


#' @keywords internal#
#' @export
.check_input_slices <- function(x, ...){

  UseMethod(".check_input_slices")

}

#' @keywords internal
#' @export
.check_input_slices.vbl2D <- function(x, slices, layer_str = "this layer()"){

  if(!is.null(slices)){

    .stop_if_not(is_slice_set(slices))

    slices_missing <- slices[!slices %in% slices(x)]
    n_missing <- length(slices_missing)

    if(n_missing != 0){

      fill <- ifelse(n_missing == 1, "slice", "slices")
      slices_quo <- rlang::enquo(slices)
      slices_text <- rlang::quo_text(slices_quo)
      slices_d <- glue::glue_collapse(sort(slices(x)), sep = ", ", last = " and ")
      slices_m <- glue::glue_collapse(slices_missing, sep = ", ", last = " and ")

      rlang::abort(
        message = c(
          glue::glue("In `slices = {slices_text}`, no data for {fill} {slices_m} in {layer_str}."),
          i = glue::glue("The ggplane() to which this layer was added contains data for slices: {slices_d}")
          )
      )

    }

    x <- dplyr::filter(x, slice %in% {{ slices }})

  }

  return(x)

}




#' @keywords keyword
.check_input_var <- function(x, var, type = "all", call = rlang::caller_fn()){

  opts <- vars_type(x, type = type)

  if(!var %in% opts){

    ref <- class(x)[1]

    if(length(opts) == 0){

      info <- glue::glue("There are no variables of type {type} in this {ref}.")

    } else {

      info <- glue::glue("Valid input options are '{.str_collapse(opts)}'.")

    }

    rlang::abort(
      c(
        glue::glue("Input var = '{var}' is invalid. '{var}' not found among {type} variables in this {ref}."),
        i = info
        )
    )

  }

  return(var)

}

#' @keywords internal
.clip_offset <- function(vbl2D){

  length(slices(vbl2D)) > 1 &&
    is_offset(vbl2D) &&
    !isFALSE(vbl_opts("clip.offset"))

}


#' @keywords internal
.default_alpha <- function(alpha, key){

  if(.is_vbl_def(alpha)){

    alpha <- vbl_opts(key)

  } else {

    .stop_if_not(is_size(alpha))

  }

  return(alpha)

}

#' @keywords internal
.default_color <- function(color, key){

  if(.is_vbl_def(color)){

    color <- vbl_opts(key)

  } else {

    .stop_if_not(is_color(color))

  }

  return(color)

}

#' @keywords internal
.default_alpha_set <- function(alpha, key, ln = Inf){

  stopifnot(length(alpha) <= ln)

  if(.is_vbl_def(alpha)){

    alpha <- vbl_opts(key)

  } else {

    .stop_if_not(is_alpha_set(alpha))

  }

  return(alpha)

}

#' @keywords internal
.default_color_set <- function(color, key, ln = Inf){

  stopifnot(length(color) <= ln)

  if(.is_vbl_def(color)){

    color <- vbl_opts(key)

  } else {

    .stop_if_not(is_color_set(color))

  }

  return(color)

}

#' @keywords internal
.default_size <- function(size, key){

  if(.is_vbl_def(size)){

    size <- vbl_opts(key)

  } else {

    .stop_if_not(is_size(size))

  }

  return(size)

}

#' @keywords internal
.downsample <- function(x, n){

  stopifnot(length(n) == 1 && n > 0)

  if(identical(n, Inf)) return(x)

  if(n < 1) n <- round(length(x) * n)

  idx <- round(seq(1, length(x), length.out = n))
  idx <- pmax(1L, pmin(length(x), idx))

  out <- unique(x[idx])

  if(length(out) < n){

    rlang::warn(
      message = c(
        glue::glue("Attempting to downsample {length(x)} slice indices with n = {n}."),
        i = glue::glue("Returning {length(out)} slices indices.")
      )
    )

  }

  return(out)

}

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

    rlang::warn(message = msg)

  }

}


#' @importFrom glue glue
#' @importFrom rlang caller_env
.glue_stop <- function(...){

  .envir <- rlang::caller_env()
  msg <- glue::glue(..., .envir = .envir)

  rlang::abort(message = msg)

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
.is_attached <- function(pkg){

  paste0("package:", pkg) %in% search()

}

#' @keywords internal
.is_named <- function(x){

  names <- base::names(x)

  shiny::isTruthy(names)

}

#' @keywords internal
.is_vbl_def <- function(x) inherits(x, "vbl_def")

#' @keywords internal
.keep_named <- function(x){

  x[purrr::map_lgl(.x = base::names(x), .f = ~ shiny::isTruthy(.x))]


}

#' @keywords internal
.match_arg <- function(arg, choices, call = rlang::caller_env()){

  arg_quo  <- rlang::enquo(arg)
  arg_expr <- rlang::as_label(rlang::get_expr(arg_quo))

  val <- rlang::eval_tidy(arg_quo)

  if(!is.character(val) || length(val) != 1 || !val %in% choices){

    msg <- sprintf(
      "`%s` is '%s' but should be one of %s",
      arg_expr,
      val,
      paste(sprintf("'%s'", choices), collapse = ", ")
    )

    rlang::abort(msg, call = call)

  }

  val

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
  pointers <- .split_orientation(orientation)

  order_out <- vector(mode = "character", length = 3)
  for(i in 1:3){

    order_out[i] <-
      purrr::keep(ccs_map, ~ pointers[i] %in% .x) %>%
      names()

  }

  order_out <- order_out[order_out %in% names(x)]
  x[order_out]

}

#' @title Resolve parameters with vbl_def()
#' @name vbl_doc_resolve_def
#' @description
#' Resolving parameters that can have vbl_def() as default, which
#' draws from vbl_opts(). Functions should check whether the resolving
#' works and return an informative error message if not.
NULL

#' @rdname vbl_doc_resolve_def
#' @keywords internal
.resolve_label_spacer <- function(label_spacer){

  if(.is_vbl_def(label_spacer)){

    label_spacer <- vbl_opts("label.spacer")

    if(!is.numeric(label_spacer) | length(label_spacer) != 1){

      rlang::abort(
        c(glue::glue("Could not resolve `label_spacer` with global options."),
          i = glue("`label_spacer = {rlang::quo_text(label_spacer)}`, but should resolve to a numeric scalar."),
          i = glue::glue("Set your desired default(e.g. label_spacer = 0.75) globally with `vbl_opts('label_spacer' = 4)`"),
          i = "Alternatively, specify explicitly in the function call with `label_spacer = 0.75`."
        )
      )

    }

  } else {

    .stop_if_not(is.numeric(label_spacer) && length(label_spacer) == 1)

  }

  return(label_spacer)

}

#' @rdname vbl_doc_resolve_def
#' @keywords internal
.resolve_legend_label <- function(label, color, default){

  .stop_if_not(length(label) == 1)

  if(isTRUE(label)){

    legend_label <- purrr::set_names(color, default)

  } else if(is.character(label)){

    legend_label <- purrr::set_names(color, label[1])

  } else {

    legend_label <- NULL

  }

  return(legend_label)

}

#' @rdname vbl_doc_resolve_def
#' @keywords internal
.resolve_n <- function(n, opt){

  if(.is_vbl_def(n)){

    opt_string <- paste0("n.", opt)

    n <- vbl_opts(opt_string)

    if(length(n) != 1 | any(n != as.integer(n))){

      n_quo <- rlang::enquo(n)

      rlang::abort(
        c(glue::glue("Could not resolve `n` with global options."),
          i = glue("`n = {rlang::quo_text(n_quo)}`, but should resolve to a numeric scalar unambiguously interpretable as an integer."),
          i = glue::glue("Set your desired default(e.g. n = 4) globally with `vbl_opts({opt_string} = 4)`"),
          i = "Alternatively, specify explicitly in the function call with `n = 4`."
        )
      )

    }

  } else {

    .stop_if_not(length(n) == 1 && n == as.integer(n))

  }

  return(n)

}

#' @rdname vbl_doc_resolve_def
#' @keywords internal
.resolve_plane <- function(plane){

  if(.is_vbl_def(plane)){

    plane <- vbl_opts("plane")

    if(!plane %in% vbl_planes){

      plane_quo = rlang::enquo(plane)

      rlang::abort(
        message = c(
          "Could not resolve `plane` with global options.",
          i = glue::glue("`plane = {rlang::quo_text(plane_quo)}`, but should be one of 'sag', 'axi' or 'cor'."),
          i = "Set your desired default plane (e.g. plane = 'axi') in global options with `vbl_opts(plane = 'axi')`.",
          i = "Alternatively, specify explicitly in the function call with `plane = 'axi'`."
        )
      )

    }

  } else {

    .stop_if_not(is_plane(plane))

  }

  return(plane)

}

#' @rdname vbl_doc_resolve_def
#' @keywords internal
.resolve_slices <- function(slices, slices_data, layer_str){

  if(!is.null(slices)){

    .stop_if_not(is_slice_set(slices))

    slices_missing <- slices[!slices %in% slices_data]
    n_missing <- length(slices_missing)

    if(n_missing != 0){

      fill <- ifelse(n_missing == 1, "slice", "slices")
      slices_quo <- rlang::enquo(slices)
      slices_text <- rlang::quo_text(slices_quo)
      slices_d <- glue::glue_collapse(sort(slices_data), sep = ", ", last = " and ")
      slices_m <- glue::glue_collapse(slices_missing, sep = ", ", last = " and ")

      rlang::abort(
        message = c(
          glue::glue("In `slices = {slices_text}`, no data for {fill} {slices_m} in {layer_str}."),
          i = glue::glue("The ggplane() to which this layer was added contains data for slices: {slices_d}")
        )
      )

    }

    slices_data <- slices

  }

  return(slices_data)

}


#' @rdname vbl_doc_resolve_def
#' @keywords internal
.resolve_verbose <- function(verbose){

  if(.is_vbl_def(verbose)){

    verbose <- vbl_opts("verbose")

    if(length(verbose) != 1 || !verbose %in% c(TRUE, FALSE)){

      verbose_quo <- rlang::enquo(verbose)

      rlang::abort(
        message = c(
          "Could not resolve `verbose` with global options.",
          i = glue::glue("`verbose = {rlang::quo_text(verbose_quo)}`, but should be a logical scalar: either TRUE or FALSE."),
          i = "Set your desired default plane (e.g. verbose = TRUE) in global options with `vbl_opts(verbose = TRUE)`.",
          i = "Alternatively, specify explicitly in the function call with `verbose = TRUE`."
        )
      )

    }

  } else {

    verbose <- isTRUE(verbose)

  }

  return(verbose)
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
.stop_if_not <- function(test, call = rlang::caller_env()){

  test_quo <- rlang::enquo(test)

  res <- rlang::eval_tidy(test_quo)

  if(isTRUE(res)){

    return(invisible(TRUE))

  }

  # extract call like is_limit(xlim)
  call_expr <- rlang::get_expr(test_quo)

  if(rlang::is_call(call_expr) && length(call_expr) == 2){

    test_fn  <- rlang::as_label(call_expr[[1]])
    obj_expr <- rlang::as_label(call_expr[[2]])

    what <- sub("^is_|^is.", "", test_fn)
    what <- stringr::str_replace_all(what, pattern = "_", replacement = " ")

    rlang::abort(
      message = c(
        glue::glue("Input `{obj_expr}` is not a valid {what}."),
        i = glue::glue("See `?{test_fn}` for more information.")
      ),
      call = call
      )

  } else {

    text <- rlang::quo_text(test_quo)

    rlang::abort(
      message = c(
        "Invalid argument specification.",
        i = glue::glue("`{text}` is not TRUE.")
      ),
      call = call
    )

  }

}


#' @keywords internal
stopif <- function(cond) {
  if (isTRUE(cond)) {
    msg <- paste0(deparse(substitute(cond)), " is TRUE.")
    stop(msg, call. = FALSE)
  }
}


#' @keywords internal
qassign <- function(x, env = .GlobalEnv){ # debugging

  nm <- deparse(substitute(x))

  assign(nm, x, envir = env)

  invisible(x)

}



