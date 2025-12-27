# Spatial, grid, polygon and geometry helpers.



identify_nearest_voxel <- function(vbl_query, vbl_data){

  if(!"id" %in% colnames(vbl_query)){ vbl_query <- id_add(vbl_query) }

  if(!"id" %in% colnames(vbl_data)){ vbl_data <- id_add(vbl_data) }

  vbl_data <- dplyr::filter(vbl_data, !id %in% vbl_query$id)

  nn_out <-
    RANN::nn2(
      data = as.matrix(vbl_data[, ccs_labels]),
      query = as.matrix(vbl_query[, ccs_labels]),
      searchtype = "priority",
      k = 1
    )

  vbl_query$nn_id <- vbl_data$id[nn_out$nn.idx]
  vbl_query$nn_dist <- as.numeric(nn_out$nn.dists)

  return(vbl_query)

}


#' @title Absolute and relative numeric specifications
#' @description
#' Provide a unified mechanism to distinguish absolute and relative numeric specifications
#' for spatial parameters like `offset_dist`, `buffer` or `expand`.
#'
#' @param x A numeric vector to be classified or marked as absolute or relative.
#'
#' @return
#' * `is_abs()`: Logical scalar, `TRUE` if `x` is interpreted as an absolute specification.
#' * `is_rel()`: Logical scalar, `TRUE` if `x` is interpreted as a relative specification.
#' * `as_abs()`: Numeric vector equal to `x` but marked with an `"abs"` attribute set to `TRUE`.
#' * `as_rel()`: Numeric vector equal to `x` but marked with a `"rel"` attribute set to `TRUE`.
#'
#' @details
#' These helpers define a consistent convention for interpreting numeric magnitudes.
#' They are intended for arguments that describe spatial shifts or expansions such as `offset_dist`, `buffer`, or `expand`.
#'
#' Default classification rules are.
#' \itemize{
#'   \item `is_abs(x)` is `TRUE` if `x` is an integer vector or has an `"abs"` attribute set to `TRUE`.
#'   \item `is_rel(x)` is `TRUE` if `x` is a double vector and not absolute, or has a `"rel"` attribute set to `TRUE`.
#' }
#'
#' The constructor helpers explicitly mark intent.
#' \itemize{
#'   \item `as_abs(x)` coerces `x` to numeric and sets the `"abs"` attribute.
#'   \item `as_rel(x)` coerces `x` to numeric and sets the `"rel"` attribute.
#' }
#'
#' Attribute-based markings take precedence over the default type-based interpretation.
#'
#' @note
#' Use these helpers in functions that accept both absolute and relative magnitudes to keep the API predictable.
#' For example, a `buffer` argument may treat `2L` or `as_abs(2)` as an absolute shift in voxel units and `0.1` or `as_rel(0.1)` as a 10 % relative expansion.
#'
#' @examples
#' # Default behaviour: integers absolute, doubles relative
#' is_abs(2L)
#' is_rel(0.2)
#'
#' # Attribute-based overrides
#' x_abs <- as_abs(2)
#' x_rel <- as_rel(2)
#' is_abs(x_abs)
#' is_rel(x_rel)
#'
#' # Example usage in a buffer argument (conceptual)
#' buf_abs <- as_abs(3)   # expand by 3 units
#' buf_rel <- 0.1        # expand by 10 % of the range
#'
#' @name vbl_doc_abs_rel
NULL


#' @rdname vbl_doc_abs_rel
is_abs <- function(x){
  (is.integer(x)) ||
    isTRUE(attr(x, "abs", exact = TRUE))
}

#' @rdname vbl_doc_abs_rel
is_rel <- function(x){
  (is.double(x) && !is_abs(x)) ||
    isTRUE(attr(x, "rel", exact = TRUE))
}

#' @rdname vbl_doc_abs_rel
as_abs <- function(x){

  x <- as.numeric(x)
  attr(x, which = "abs") <- TRUE
  return(x)

}

#' @rdname vbl_doc_abs_rel
as_rel <- function(x){

  x <- as.numeric(x)
  attr(x, which = "rel") <- TRUE
  return(x)

}


#' @title Check whether values lie inside numeric limits
#' @description
#' Test whether numeric values fall inside a specified lower–upper interval.
#'
#' @param x A numeric vector to test.
#' @param l A numeric vector of length two giving lower and upper limits.
#'   Must satisfy the conditions checked by \link{is_limit}().
#' @param null_ok Logical. If \code{TRUE} and \code{l} is \code{NULL}, all
#'   elements of \code{x} are considered valid.
#'
#' @return
#' A logical vector indicating whether each element of \code{x} satisfies the
#' interval condition defined by the function.
#'
#' @details
#' If \code{l} is not \code{NULL}, it must be a valid limit according to
#' \link{is_limit}().
#' \itemize{
#'   \item \code{within_limits(x, l)} evaluates \code{min(l) <= x <= max(l)}.
#'   \item \code{inside_limits(x, l)} evaluates \code{min(l) <  x <  max(l)}.
#' }
#'
#' @seealso \link{is_limit}()
#'
#' @examples
#' x <- c(5, 10, 15, 20)
#'
#' # Inclusive interval
#' within_limits(x, l = c(10, 20))
#'
#' # Exclusive interval
#' inside_limits(x, l = c(10, 20))
#'
#' # NULL limits: keep all or reject all
#' within_limits(x, l = NULL, null_ok = TRUE)
#' within_limits(x, l = NULL, null_ok = FALSE)
#'
#' @export
within_limits <- function(x, l, null_ok = FALSE){
  if (isTRUE(null_ok) && is.null(l)) {
    rep(TRUE, length(x))
  } else {
    stopifnot(is_limit(l))
    x >= min(l) & x <= max(l)
  }
}

#' @rdname within_limits
#' @export
inside_limits <- function(x, l, null_ok = FALSE){
  if (isTRUE(null_ok) && is.null(l)) {
    rep(TRUE, length(x))
  } else {
    stopifnot(is_limit(l))
    x > min(l) & x < max(l)
  }
}






