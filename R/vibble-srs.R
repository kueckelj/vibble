

#' @title vibble's Spatial reference system
#' @name vbl_doc_srs
#'
#' @description
#' Overview of how vibbles encode and translate spatial reference systems,
#' including Cartesian axes, anatomical planes, matrix indices, and
#' orientation labels.
#'
#' @details
#' Spatial information in 3D medical imaging data can be separated into four
#' related reference systems:
#'
#' \itemize{
#'
#'   \item \strong{Cartesian coordinate system (ccs):}
#'   Numeric voxel coordinates \code{x}, \code{y}, \code{z}. Used as the
#'   canonical 3D reference for vibbles.
#'
#'   \item \strong{Anatomical pointer:}
#'   Orientation labels \code{L/R}, \code{I/S}, \code{P/A} describing the
#'   direction of increasing \code{x}, \code{y}, \code{z}.
#'
#'   \item \strong{Anatomical planes:}
#'   Slice designation \code{"sag"}, \code{"axi"}, \code{"cor"} describing a
#'   2D view through the 3D volume.
#'
#'   \item \strong{Matrix 2D indices (m2D):}
#'   Image-like indices \code{col}, \code{row}, \code{slice} used for 2D
#'   plotting and matrix-style operations.
#'
#' }
#'
#' The \code{srs_labels} list defines the canonical labels for each system:
#' \code{"ccs"} (x, y, z), \code{"pointer"} (L, I, P),
#' \code{"plane"} (sag, axi, cor), and \code{"m2D"} (col, row, slice).
#'
#' @section Cartesian axes and orientation:
#'
#' \itemize{
#'
#'   \item \code{ccs_labels} defines the ordered Cartesian axes
#'   \code{c("x", "y", "z")}. These are the reference columns expected in a
#'   vibble for 3D operations.
#'
#'   \item \code{ccs_orientation_mapping} maps each axis to its anatomical
#'   orientation labels, e.g. \code{x = c("R", "L")} for left–right,
#'   \code{y = c("S", "I")} for superior–inferior, and
#'   \code{z = c("A", "P")} for anterior–posterior.
#'
#' }
#'
#' These objects are used to interpret \code{x}, \code{y}, \code{z} in
#' anatomical terms and to keep axis semantics consistent across functions.
#'
#' @section Plane–axis conversion:
#'
#' \itemize{
#'
#'   \item \code{ccs_to_plane()} converts a Cartesian axis label
#'   (\code{"x"}, \code{"y"}, \code{"z"}) to its corresponding anatomical
#'   plane (\code{"sag"}, \code{"axi"}, \code{"cor"}).
#'
#'   \item \code{plane_to_ccs()} converts an anatomical plane label
#'   (\code{"sag"}, \code{"axi"}, \code{"cor"}) back to the underlying
#'   Cartesian axis.
#'
#' }
#'
#' Both functions validate their input against the internal axis/plane
#' vocabularies (\code{vbl_ccs_axes}, \code{vbl_planes}) and are used
#' throughout plotting and slicing utilities to translate between user-facing
#' plane specifications and internal \code{x}, \code{y}, \code{z}.
#'
#' @section Required axes for 2D and 3D views:
#'
#' \itemize{
#'
#'   \item \code{req_axes_2D()} (internal) returns the mapping from
#'   2D matrix indices \code{col}, \code{row}, \code{slice} to Cartesian
#'   axes for a given plane. For example, in the sagittal plane
#'   \code{"sag"} the mapping is
#'   \code{c("col" = "z", "row" = "y", "slice" = "x")}.
#'
#'   \item \code{req_axes_3D()} returns the inverse mapping, i.e. for a
#'   given plane it returns which \code{col}/\code{row}/\code{slice}
#'   combination corresponds to each of \code{x}, \code{y}, \code{z},
#'   always ordered as \code{c("x", "y", "z")}.
#'
#'   \item \code{req_axes_2d()} is a deprecated, exported wrapper around
#'   \code{req_axes_2D()} kept for backward compatibility.
#'
#' }
#'
#' These helpers are used whenever a function needs to derive the required
#' 2D or 3D axes from a supplied plane (e.g. for slicing, 2D views, or
#' consistency checks).
#'
#' @section Label switching:
#'
#' \itemize{
#'
#'   \item \code{switch_axis_label()} accepts a character vector of labels
#'   and switches each element between Cartesian axes and anatomical planes:
#'   \itemize{
#'     \item if the label is a plane (\code{vbl_planes}), it is converted
#'     to the corresponding Cartesian axis via \code{plane_to_ccs()}.
#'     \item if the label is a Cartesian axis (\code{vbl_ccs_axes}), it is
#'     converted to the corresponding plane via \code{ccs_to_plane()}.
#'   }
#'
#' }
#'
#' This allows user-facing APIs to accept both plane labels and Cartesian
#' axis labels while keeping the internal representation consistent.
#'
#' @keywords internal
NULL

#' @rdname vbl_doc_srs
#' @keywords internal
#' @export
srs_labels <- list(
  "ccs" = c("x", "y", "z"),  # cartesian coordinates system
  "pointer" = c("L", "I", "P"), # anatomical pointer
  "plane" = c("sag", "axi", "cor"), # anatomical planes
  "m2D"  = c("col", "row", "slice") # Matrix 2D
)

#' @rdname vbl_doc_srs
#' @keywords internal
#' @export
ccs_labels <- c("x", "y", "z")

#' @rdname vbl_doc_srs
#' @keywords internal
#' @export
ccs_orientation_mapping <- list(x = c("R", "L"), y = c("S", "I"), z = c("A", "P"))
ccs_orientation_mapping_fix <- list(x = "L", y = "I", z = "P")

#' @rdname vbl_doc_srs
#' @keywords internal
#' @export
ccs_to_plane <- function(axis){

  axis <- match.arg(axis, choices = vbl_ccs_axes)

  out <- c("x" = "sag", "y" = "axi", "z" = "cor")[axis]

  unname(out)

}

#' @rdname vbl_doc_srs
#' @keywords internal
#' @export
plane_to_ccs <- function(plane){

  plane <- match.arg(plane, choices = vbl_planes)

  out <- c("sag" = "x", "axi" = "y", "cor" = "z")[plane]

  unname(out)

}


#' @rdname vbl_doc_srs
#' @keywords internal
#' @export
req_axes_2D <- function(plane){

  if(plane == "sag"){

    out <- c("col" = "z", "row" = "y", "slice" = "x")

  } else if(plane == "axi"){

    out <- c("col" = "x", "row" = "z", "slice" = "y")

  } else if(plane == "cor"){

    out <- c("col" = "x", "row" = "y", "slice" = "z")

  }

  return(out[c("col", "row", "slice")])

}

#' @keywords internal
.req_axis_2D_ref <- function(plane, plane_ref){

  req_axes <- req_axes_2D(plane)
  ccs_ref <- plane_to_ccs(plane_ref)

  names(req2D)[req2D == ccs_ref]

}

#' @rdname vbl_doc_srs
#' @keywords internal
#' @export
req_axes_3D <- function(plane){

  out1 <- req_axes_2D(plane)

  out2 <- names(out1)
  names(out2) <- unname(out1)

  return(out2[c("x", "y", "z")])

}

#' @rdname vbl_doc_srs
#' @keywords internal
#' @export
req_axes_2d <- function(...){

  caller_call <- rlang::caller_call()
  caller_name <- rlang::call_name(caller_call)

  warning(
    "req_axes_2d() is deprecated in favor of req_axes_2D(). ",
    "Called from `", caller_name, "()`.",
    call. = FALSE
  )

  req_axes_2D(...)

}

#' @rdname vbl_doc_srs
#' @keywords internal
#' @export
switch_axis_label <- function(label){

  purrr::map_chr(
    .x = label,
    .f = function(l){

      if(l %in% vbl_planes){

        plane_to_ccs(l)

      } else if(l %in% vbl_ccs_axes) {

        ccs_to_plane(l)

      }

    }
  )

}


