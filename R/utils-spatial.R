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


#' @title Check whether values lie within numeric limits
#' @description Test whether numeric values fall strictly between a lower and
#' upper bound. Returns a logical vector with one entry per element of `x`.
#'
#' @param x A numeric vector to be tested.
#' @param l A numeric vector of length two specifying lower and upper limits.
#'   Must satisfy the conditions checked by \link{is_limit}().
#' @param null_ok Logical. If `TRUE` and l is `NULL`, the function returns `TRUE`
#' for all elements of `x`.
#'
#' @return A logical vector indicating whether each element of `x` lies within
#' the specified limits.
#'
#' @details
#' If limits are provided, they must be valid according to \link{is_limit}().
#' The function performs strict comparisons (`>=` and `=<`).
#'
#' @seealso \link{is_limit}()
#'
#' @examples
#' # Example 1: Basic usage
#' x <- c(5, 10, 15, 20)
#' within_limits(x, l = c(9, 18))
#'
#' # Example 2: Open limits when l = NULL
#' within_limits(x, l = NULL, null_ok = FALSE)
#' within_limits(x, l = NULL, null_ok = TRUE)
#'
#' @export
within_limits <- function(x, l, null_ok = FALSE){

  if(isTRUE(null_ok) && is.null(l)){

    rep(TRUE, length(x))

  } else {

    stopifnot(is_limit(l))

    x >= min(l) & x <= max(l)

  }

}





