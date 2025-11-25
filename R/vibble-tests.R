


is_label_candidate <- function(x){

  is.numeric(x) && all(x == as.integer(x)) && !is_mask_var(x)

}

is_mask_candidate <- function(x){

  is.numeric(x) && identical(sort(unique(x)), c(0, 1))

}

is_label_var <- function(x, fdb = NULL){

  out <- is.factor(x)

  is_vartype_fdb(x = out, fdb = fdb, type = "label")

  return(out)

}

is_mask_var <- function(x, fdb = NULL){

  out <- is.logical(x)

  is_vartype_fdb(x = out, fdb = fdb, type = "mask")

  return(out)

}

is_numeric_var <- function(x, fdb = NULL){

  out <- is.numeric(x) && !all(x == as.integer(x))

  is_vartype_fdb(x = out, fdb = fdb, type = "numeric")

  return(out)

}

is_vartype_fdb <- function(x, fdb, type){

  if(is.character(fdb) && !isTRUE(x)){

    fdb <- match.arg(fdb, choices = c("message", "warning", "stop"))

    msg <- glue::glue("Input is not a {type} variable.")

    if(fdb == "message"){

      message(msg)

    } else if(fdb == "warning"){

      warning(msg, call. = FALSE)

    } else if(fdb == "stop"){

      stop(msg, call. = FALSE)

    }

  }

  invisible(x)

}


test_ccs_limits <- function(lim_a, lim_b, fdb = FALSE){

  test_out <- purrr::map2_lgl(.x = lim_a, .y = lim_b, .f = identical)

  if(!all(test_out) & isTRUE(fdb)){

    ref <- stringr::str_c(names(test_out)[!test_out], collapse = ", ")
    stop(glue::glue("Different CCS-limits on axis {ref}."))

  }

  return(all(test_out))

}


test_ccs_mapping <- function(map_a, map_b, fdb = FALSE){

  test_out <- purrr::map2_lgl(.x = map_a, .y = map_b, .f = identical)

  if(!all(test_out) & isTRUE(fdb)){

    ref <- stringr::str_c(names(test_out)[!test_out], collapse = ", ")
    stop(glue::glue("Different CCS-mappings on axis {ref}."))

  }

  return(all(test_out))

}

#' @title Validate an anatomical orientation code.
#' @description
#' Check whether a three-letter orientation string (e.g. `"LIP"`, `"RAS"`)
#' contains exactly one left–right, one superior–inferior, and one anterior–posterior
#' direction code.
#'
#' @details
#' The function splits the input string into single letters and verifies:
#' \itemize{
#'   \item it consists of exactly three characters, and
#'   \item each anatomical axis is represented:
#'         \code{R/L}, \code{S/I}, \code{A/P}.
#' }
#' If \code{error = TRUE}, violations trigger an informative error message;
#' otherwise the function returns \code{TRUE} or \code{FALSE}.
#'
#' @param orientation
#' Character string specifying the desired anatomical orientation.
#'
#' @param error
#' Logical. If \code{TRUE}, throw an error on invalid input instead of
#' returning \code{FALSE}.
#'
#' @return
#' Logical value indicating whether the orientation string is valid.
#'
#' @examples
#' valid_orientation("LIP")
#' valid_orientation("RAS")
#' valid_orientation("LIA")
#' valid_orientation("LI") # FALSE
#' valid_orientation("LI", error = TRUE) # FALSE
#' valid_orientation("LIS", error = TRUE) # FALSE
#'
#' @export

valid_orientation <- function(orientation, error = FALSE){

  stopifnot(is.character(orientation))

  orientation <- stringr::str_split_1(orientation, "")

  exist <-
    purrr::map_lgl(
      .x = unname(ccs_orientation_mapping),
      .f = ~ any(.x %in% orientation)
    )

  if(!all(exist) & isTRUE(error)){

    stop("Input for `orientation` must contain R or L and S or I and A or P.")

  }

  l3 <- length(orientation) == 3

  if(!l3 & isTRUE(error)){

    stop("Input for `orientation` must contain exactly three letters.")

  }

  return(all(exist) & l3)

}
