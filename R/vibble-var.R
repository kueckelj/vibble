# Vibble attributes and variable metadata and summaries.

ccs_limits <- function(vbl){ attr(vbl, which = "ccs_limits") }

ccs_mapping <- function(vbl){ attr(vbl, which = "ccs_mapping") }




#' @title Create variable-level metadata
#' @description
#' Generate a metadata list for a single variable, including labels for
#' categorical data, counts for logical data, and summary statistics for
#' numeric data.
#'
#' @details
#' The function inspects the type of `dvar` and computes the corresponding
#' metadata elements. Fields not applicable to the input type are returned
#' as `NULL`.
#'
#' @param dvar
#' A vector of type character, factor, logical, or numeric.
#'
#' @return
#' A named list with the following elements:
#' \itemize{
#'   \item{\code{labels: }}{Sorted unique entries (character) or factor levels (factor).}
#'   \item{\code{nT: }}{Number of \code{TRUE} values (logical).}
#'   \item{\code{limits: }}{Numeric range: \code{c(min, max)}.}
#'   \item{\code{mean: }}{Mean of numeric values.}
#'   \item{\code{median: }}{Median of numeric values.}
#' }
#'
#' @examples
#' summarize_var(c(1, 3, 5))
#' summarize_var(c(TRUE, FALSE, TRUE))
#' summarize_var(c("a", "b", "b"))
#'
#' @export
summarize_var <- function(dvar){

  list(

    # character
    labels = if(is.character(dvar)){ sort(unique(dvar)) } else if(is.factor(dvar)){ levels(dvar) },

    # logical
    nT = if(is.logical(dvar)) sum(dvar, na.rm = TRUE),

    # numeric
    limits = if(is.numeric(dvar)) range(dvar, na.rm = TRUE),
    mean = if(is.numeric(dvar)) mean(dvar, na.rm = TRUE),
    median = if(is.numeric(dvar)) median(dvar, na.rm = TRUE)

  )

}

update_var_smr <- function(vbl, vars = NULL){

  vars <- vars[!vars %in% ccs_labels]

  if(is.character(vars)){

    stopifnot(all(vars %in% colnames(vbl)))

  } else {

    vars <- colnames(vbl)
    vars <- vars[!vars %in% ccs_labels]

  }

  vm <- var_smr(vbl)
  vm[vars] <- purrr::map(vbl[,vars], .f = summarize_var)
  attr(vbl, which = "var_smr") <- vm

  return(vbl)

}

#' @title Extract variable-level metadata
#' @description
#' Retrieve variable-level metadata stored in the `var_smr` attribute of a vibble.
#'
#' @details
#' The function returns either the full `var_smr` list or the metadata entry for a
#' specific variable. If a variable name is supplied but no corresponding metadata
#' exists, an error is raised.
#'
#' @param vbl
#' A vibble object containing a `var_smr` attribute.
#'
#' @param var
#' Optional character string. If provided, only the metadata for this variable is
#' returned. If `NULL`, the complete metadata list is returned.
#'
#' @return
#' A list containing either all variable metadata or the metadata for a single
#' variable.
#'
#' @examples
#' \dontrun{
#' var_smr(vbl)                 # full metadata
#' var_smr(vbl, var = "t1")     # metadata for one variable
#' }
#'
#' @export
var_smr <- function(vbl, var = NULL){

  if(is.character(var)){

    out <- attr(vbl, which = "var_smr")[[var]]

    if(is.null(out)){ stop(glue::glue("No meta data for variable '{var}'.")) }

  } else {

    out <- attr(vbl, which = "var_smr")

  }

  return(out)

}

`var_smr<-` <- function(vbl, var, what, value){

  vm <- var_smr(vbl)

  vm[[var]][[what]] <- value

  attr(vbl, which = "var_smr") <- vm

  return(vbl)

}



var_type <- function(x){

  if(is_label_var(x)){

    "label"

  } else if(is_mask_var(x)){

    "mask"

  } else if(is_numeric_var(x)){

    "numeric"

  } else {

    NA

  }

}

