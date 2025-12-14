

#' @title Check if object is a 2D bounding box
#' @description Test whether an object encodes a valid 2D bounding box.
#' Returns `TRUE` if the structure and values fulfill the requirements.
#'
#' @param x An object to be tested for being a valid 2D bounding box.
#'
#' @return A logical scalar.
#'
#' @details A valid 2D bounding box
#' \itemize{
#'   \item must be a list
#'   \item must be of length 1 or 2
#'   \item must be named, where names correspond to col and/or row
#'   \item must contain only valid \link[=is_limit]{limits}
#' }
#'
#' @seealso \link{filter_bb3D}()
#'
#' @importFrom purrr map_lgl
#' @importFrom dplyr n_distinct
#'
#' @export
is_bb2D <- function(x){

  is.list(x) &&
  length(x) %in% 1:2 &&
  all(names(x) %in% c("col", "row")) &&
  all(map_lgl(.x = x, .f = is_limit))

}


#' @title Check if object is a 3D bounding box
#' @description Test whether an object encodes a valid 3D bounding box.
#' Returns `TRUE` if the structure and values fulfill the requirements.
#'
#' @param x An object to be tested for being a valid 3D bounding box.
#'
#' @return A logical scalar.
#'
#' @details A valid 3D bounding box
#' \itemize{
#'   \item must be a list
#'   \item must be at least of length 1, max of length 3
#'   \item must be named, where names correspond to x, y, and/or z
#'   \item must contain only valid \link[=is_limit]{limits}
#' }
#'
#' @seealso \link{filter_bb3D}()
#'
#' @importFrom purrr map_lgl
#' @importFrom dplyr n_distinct
#'
#' @examples
#' # Example 1: Valid 3D bounding box
#' bb3D <- list(
#'   x = c(10, 20),
#'   y = c( 5, 30),
#'   z = c( 1, 10)
#' )
#' is_bb3D(bb3D)
#'
#' # Example 2: Invalid bounding box (contains zero)
#' bb_wrong <- list(
#'   x = c(10, 20),
#'   y = c( 0, 30),  # 0 violates the >= 1 rule
#'   z = c( 1, 10)
#' )
#' is_bb3D(bb_wrong)
#'
#' @export
is_bb3D <- function(x){

  is.list(x) &&
  length(x) %in% 1:3 &&
  all(names(x) %in% vbl_ccs_axes) &&
  all(map_lgl(.x = x, .f = is_limit))

}


#' @title Identify voxel-value type candidates
#' @description
#' These helper functions test whether a numeric vector extracted from a NIfTI
#' image can be interpreted as a mask, a label variable, or a continuous numeric
#' variable.
#'
#' @details
#' The functions apply simple rules based on unique values:
#' \itemize{
#'   \item `is_mask_candidate(x)` returns `TRUE` when `x` is numeric and its
#'   sorted unique values equal `c(0, 1)`
#'   \item `is_label_candidate(x)` returns `TRUE` when `x` is numeric, contains
#'   only integer values, and is not a mask candidate
#'   \item `is_numeric_candidate(x)` returns `TRUE` when `x` is numeric and is
#'   not a mask candidate (continuous or integer-valued data)
#' }
#'
#' These checks are used to determine how a voxel variable should be encoded
#' during NIfTI-to-vibble conversion, especially for deciding whether to keep a
#' variable numeric, convert it to logical, or interpret it as a categorical
#' label suitable for lookup-table mapping.
#'
#' @param x
#' A numeric vector extracted from a NIfTI volume.
#'
#' @return
#' A logical scalar indicating whether the input matches the tested type.
#'
#' @examples
#' is_mask_candidate(c(0, 0, 1, 1))
#' is_label_candidate(c(1, 2, 3, 3))
#' is_numeric_candidate(c(0.1, 0.5, 0.9))
#'
#' @name voxel_type_candidates
NULL

#' @rdname voxel_type_candidates
is_label_candidate <- function(x){

  is.numeric(x) && all(x == as.integer(x)) && !is_mask_candidate(x)

}


#' @rdname voxel_type_candidates
is_mask_candidate <- function(x){

  is.numeric(x) && identical(sort(unique(x)), c(0, 1))

}

#' @rdname voxel_type_candidates
is_numeric_candidate <- function(x){

  is.numeric(x) && !is_mask_candidate(x)

}


#' @title Identify var types in a vibble
#' @description
#' These functions test whether a variable inside a \link{vibble} satisfies the
#' structural requirements for mask, label, or numeric voxel variables.
#'
#' @details
#' Unlike the candidate checks used on raw NIfTI data, these functions operate
#' on variables \emph{after} they have been converted into vibble format.
#' They verify the expected final R types of voxel-wise columns:
#' \itemize{
#'   \item `is_mask_var(x)` returns `TRUE` when `x` is a logical vector
#'   \item `is_categorical_var(x)` returns `TRUE` when `x` is a factor
#'   \item `is_numeric_var(x)` returns `TRUE` when `x` is numeric
#' }
#'
#' These checks are used throughout the plotting and export framework to select
#' appropriate default scales, LUT behavior, interpolation strategies, and
#' layer specifications.
#'
#' @param x
#' A vector extracted from a vibble column.
#'
#' @return
#' A logical scalar indicating whether the variable matches the tested type.
#'
#' @examples
#' is_mask_var(c(TRUE, FALSE, TRUE))
#' is_categorical_var(factor(c("GM", "WM")))
#' is_numeric_var(c(1.2, 3.5, 7.9))
#'
#' @name var_type_checks
NULL

#' @rdname var_type_checks
is_categorical_var <- is.factor

#' @rdname var_type_checks
is_mask_var <- is.logical

#' @rdname var_type_checks
is_numeric_var <- is.numeric


#' @title Check validity of numeric limit specification
#' @description Validate whether an object represents a proper numeric limit.
#'
#' @param x An object to test.
#'
#' @return A logical scalar.
#'
#' @details
#' A valid limit
#' \itemize{
#'   \item is numeric,
#'   \item has length two,
#'   \item contains two distinct values,
#' }
#'
#' @seealso \link{within_limits}()
#'
#' @examples
#' # Example 1: Valid limit
#' is_limit(c(10, 20))
#'
#' # Example 2: Invalid limit (duplicate values)
#' is_limit(c(10, 10))
#'
#'
#' @export
is_limit <- function(x){

  a <- is.numeric(x)
  b <- length(x) == 2
  c <- dplyr::n_distinct(x) == 2

  a & b & c

}


#' @rdname vbl_doc_offset_utils
#' @export
is_offset <- function(x){

  stopifnot(is_vbl2D(x))

  isTRUE(offset_col(x) != 0 | offset_row(x) != 0)

}


#' @title This is a title.
#'
#' @description
#' Check whether a variable stored in a vibble has the expected variable type
#' (e.g. `"mask"`, `"label"`, `"numeric"`, `"score"`) and trigger feedback
#' through a user-supplied function (default: `base::stop()`).
#'
#' @param vbl A \link{vibble}.
#'
#' @param var Name of the variable in `vbl` to be checked.
#'
#' @param type
#' Expected variable type. Must be one of the entries in
#' `vbl_data_var_types`. Passed to `match.arg()`.
#'
#' @param fdb
#' A function used for feedback if the type does not match.
#' The function must accept a character message as its first argument.
#' Typical values are `base::stop`, `base::warning`, or `base::message`.
#' Defaults to `base::stop`.
#'
#' @details
#' The function compares the expected type (`type`) with the actual
#' variable type returned by `var_type()`.
#' If they differ, a message is constructed that includes:
#' - the name of the calling function,
#' - the argument name that was passed to `var`,
#' - the expected type and the actual type.
#' The message is then passed to the feedback function `fdb()`.
#'
#' @return
#' `TRUE` if the variable has the expected type, otherwise returns
#' `invisible(FALSE)` after calling the feedback function.
#'
#' @examples
#' \dontrun{
#' fn_x <- function(vbl, var_mask){
#'   is_vartype(vbl, var = var_mask, type = "mask")
#' }
#'
#' fn_x(vbl, "Score")  # triggers error via base::stop()
#' }
#'
#' @seealso
#' `var_type()`, `vbl_data_var_types`

is_vartype <- function(vbl, var, type, fdb = base::stop){

  type_req <- match.arg(type, choices = vbl_data_var_types)
  var <- match.arg(var, choices = vars_data(vbl))
  type_in <- var_type(vbl[[var]])
  res <- type_in == type_req

  if(is.function(fdb) && !isTRUE(res)){

    # determine calling function name
    parent_call <- sys.call(sys.parent())
    parent_fun  <- deparse(parent_call[[1]])

    var_arg <- deparse(substitute(var, env = parent.frame()))

    msg <- glue::glue("Input for `{var_arg}` should be a of type {type_req} but is of type {type_in}. (raised in {parent_fun}())")

    fdb(msg, call. = FALSE)

  }

  return(res)

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

test_ccs_steps <- function(steps_a, steps_b, fdb = FALSE){

  test_out <- purrr::map2_lgl(.x = steps_a, .y = steps_b, .f = identical)

  if(!all(test_out) & isTRUE(fdb)){

    ref <- stringr::str_c(names(test_out)[!test_out], collapse = ", ")
    stop(glue::glue("Different CCS-steps on axis {ref}."))

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
