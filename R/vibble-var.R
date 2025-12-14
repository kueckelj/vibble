# Vibble attributes and variable metadata and summaries.

#' @title Cartesian coordinate-space limits and steps
#' @name vbl_doc_ccs
#' @description
#' Helpers to get and set Cartesian coordinate-space metadata for vibbles:
#' \itemize{
#'   \item \code{ccs_limits()}: integer voxel index limits for \code{x}, \code{y}, \code{z}.
#'   \item \code{ccs_steps()}: physical step sizes (e.g. in mm) for \code{x}, \code{y}, \code{z}.
#'   \item \code{is_ccs_limits()}, \code{is_ccs_steps()}: validators for these structures.
#' }
#'
#' @param x A \code{vbl}, \code{vbl2D}, or compatible object.
#' @param value For setters, a value to assign:
#'   \itemize{
#'     \item \code{ccs_limits<-}: a valid \link{is_ccs_limits} object.
#'     \item \code{ccs_steps<-}: a valid \link{is_ccs_steps} object.
#'   }
#' @param ... Passed on to methods (currently unused).
#'
#' @details
#' \code{ccs_limits} are stored as a named list of length three (typically
#' \code{c("x","y","z")}), where each element is an integer vector of length
#' two with \code{min == 1} giving the index range along that axis.
#'
#' \code{ccs_steps} are stored as a named list of length three with the same
#' axis names, where each element is a positive double specifying the physical
#' step size along that axis.
#'
#' @return
#' For getters:
#' \itemize{
#'   \item \code{ccs_limits()}: a \link{is_ccs_limits}-compatible list.
#'   \item \code{ccs_steps()}: a \link{is_ccs_steps}-compatible list.
#' }
#' For setters: the modified \code{x}.
#'
#' @seealso \link{is_ccs_limits}, \link{is_ccs_steps}
#'
#' @export
ccs_limits <- function(x, ...){

  UseMethod("ccs_limits")

}

#' @rdname vbl_doc_ccs
#' @export
`ccs_limits<-` <- function(x, value, ...){

  UseMethod("ccs_limits<-")

}

#' @rdname vbl_doc_ccs
#' @export
ccs_limits.vbl <- function(x, ...){

  .vbl_attr(x, which = "ccs_limits")

}

#' @rdname vbl_doc_ccs
#' @export
ccs_limits.vbl2D <- function(x, ...){

  .vbl_attr(x, which = "ccs_limits")

}

#' @rdname vbl_doc_ccs
#' @export
`ccs_limits<-.vbl` <- function(x, value, ...){

  stopifnot(is_ccs_limits(value))

  attr(x, "ccs_limits") <- value

  return(x)

}

#' @rdname vbl_doc_ccs
#' @export
ccs_steps <- function(x, ...){

  UseMethod("ccs_steps")

}

#' @rdname vbl_doc_ccs
#' @export
`ccs_steps<-` <- function(x, value, ...){

  UseMethod("ccs_steps<-")

}

#' @rdname vbl_doc_ccs
#' @export
ccs_steps.vbl <- function(x, ...){

  .vbl_attr(x, which = "ccs_steps")

}

#' @rdname vbl_doc_ccs
#' @export
ccs_steps.vbl2D <- function(x, ...){

  .vbl_attr(x, which = "ccs_steps")

}

#' @rdname vbl_doc_ccs
#' @export
`ccs_steps<-.vbl` <- function(x, value, ...){

  stopifnot(is_ccs_steps(value))

  attr(x, "ccs_steps") <- value

  return(x)

}


#' @title Cartesian coordinate-space limits (ccs_limits)
#' @name vbl_doc_ccs_limits
#' @description
#' \code{ccs_limits} define the integer voxel-index boundaries of a vibble's
#' Cartesian coordinate system along the three spatial axes. They represent the
#' valid index ranges of \code{x}, \code{y}, and \code{z} in voxel space and
#' typically originate from the NIfTI header dimensions.
#'
#' A \code{ccs_limits} object is a named list of length three, one element per
#' axis, where each element is an integer vector of length two with
#' \code{min == 1} and \code{max > 1}.
#'
#' @section Validation:
#' \itemize{
#'   \item \code{is_ccs_limits(x)} returns \code{TRUE} if \code{x} is a valid
#'   3D bounding box (\link{is_bb3D}) and each axis entry is an integer limit
#'   \code{c(1L, n)}.
#' }
#'
#' @seealso \link{ccs_steps}, \link{is_bb3D}
#' @keywords internal
NULL

#' @rdname vbl_doc_ccs_limits
#' @export
is_ccs_limits <- function(x){

  a <- is_bb3D(x)
  b <- all(
    purrr::map_lgl(
      .x = x,
      .f = ~ is.integer(.x) && length(.x) == 2L && min(.x) == 1L
    )
  )

  a && b

}

#' @title Cartesian coordinate-space step sizes (ccs_steps)
#' @name vbl_doc_ccs_steps
#' @description
#' \code{ccs_steps} define the physical step size along each Cartesian axis of a
#' vibble's voxel grid. These values typically correspond to the voxel
#' dimensions (e.g., in millimeters) found in the source imaging data.
#'
#' A \code{ccs_steps} object is a named list of length three with entries
#' \code{x}, \code{y}, and \code{z}, each containing a positive double of length
#' one.
#'
#' @section Validation:
#' \itemize{
#'   \item \code{is_ccs_steps(x)} returns \code{TRUE} if \code{x} is a list of
#'   length three with names matching the Cartesian axes and strictly positive
#'   numeric step sizes.
#' }
#'
#' @seealso \link{ccs_limits}
#' @keywords internal
NULL

#' @rdname vbl_doc_ccs_limits
#' @export
is_ccs_steps <- function(x){

  a <- is.list(x)
  b <- length(x) == 3L
  c <- all(vbl_ccs_axes %in% names(x))
  d <- all(
    purrr::map_lgl(
      .x = x,
      .f = ~ is.double(.x) && length(.x) == 1L && .x > 0
    )
  )

  a && b && c && d

}


#' @title Get or set variable limits
#' @description
#' Access or modify the stored limit attribute of a variable in a \code{vbl} or
#' \code{vbl2D} object. Variable limits encode the original value range of a
#' numeric variable at creation time and remain fixed even after filtering.
#'
#' @param x A \code{vbl}, \code{vbl2D}, or numeric vector.
#' @param var Character string naming a variable in \code{x}.
#' @param value A valid \link[=is_limit]{limit} to assign to the variable.
#' @param ... Passed on to methods.
#'
#' @return
#' For getters: the \link[=is_limit]{limits} associated with the variable
#' (or numeric vector).
#' For setters: the modified object with updated variable limits.
#'
#' @details
#' \itemize{
#'   \item For \code{vbl} and \code{vbl2D} objects, \code{var_limits()} extracts
#'   the limit attribute stored on the underlying variable.
#'
#'   \item For numeric vectors, \code{var_limits()} retrieves or sets the
#'   \code{"limits"} attribute directly.
#'
#'   \item A limit must be a numeric vector of length two with increasing values.
#' }
#'
#' @seealso \link{is_limit}
#'
#' @export
var_limits <- function(x, ...){

  UseMethod("var_limits")

}

#' @rdname var_limits
#' @export
`var_limits<-` <- function(x, var, ...){

  UseMethod("var_limits<-")

}

#' @rdname var_limits
#' @export
var_limits.vbl <- function(x, var, ...){

  stopifnot(var %in% names(x))

  .vbl_attr(x[[var]], which = "var_limits")

}

#' @rdname var_limits
#' @export
var_limits.vbl2D <- function(x, var, ...){

  stopifnot(var %in% names(x))

  .vbl_attr(x[[var]], which = "var_limits")

}

#' @rdname var_limits
#' @export
var_limits.numeric <- function(x, ...){

  .vbl_attr(x, which = "var_limits")

}

#' @rdname var_limits
#' @export
`var_limits<-.numeric` <- function(x, value, ...){

  stopifnot(is_limit(value))

  attr(x, which = "var_limits") <- value

  return(x)

}

#' @rdname var_limits
#' @export
`var_limits<-.vbl` <- function(x, var, value, ...){

  stopifnot(var %in% names(x))
  stopifnot(is_limit(value))

  var_limits(x[[var]]) <- value

  return(x)

}

#' @rdname var_limits
#' @export
`var_limits<-.vbl2D` <- function(x, var, value, ...){

  stopifnot(var %in% names(x))
  stopifnot(is_limit(value))

  var_limits(x[[var]]) <- value

  return(x)

}


#' @export
var_range <- function(x, var, ...){

  UseMethod("var_range")

}

#' @export
var_range.vbl <- function(x, var, ...){

  stopifnot(var %in% names(x))
  range(x[[var]], na.rm = TRUE, finite = TRUE)

}

#' @export
var_range.vbl2D <- function(x, var, ...){

  stopifnot(var %in% names(x))
  range(x[[var]], na.rm = TRUE, finite = TRUE)

}

#' @keywords internal
#' @export
var_type <- function(x){

  if(is_categorical_var(x)){

    "categorical"

  } else if(is_mask_var(x)){

    "mask"

  } else if(is_numeric_var(x)){

    "numeric"

  } else {

    "unknown"

  }

}

