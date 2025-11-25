# Core data structure and constructors.


ccs_labels <- c("x", "y", "z")

ccs_orientation_mapping <- list(x = c("R", "L"), y = c("S", "I"), z = c("A", "P"))

ccs_to_plane <- function(axis){

  axis <- match.arg(axis, choices = vbl_ccs_axes)

  out <- c("x" = "sag", "y" = "axi", "z" = "cor")[axis]

  unname(out)

}


#' @title Add or reconstruct voxel IDs based on spatial coordinates
#' @description Create a unique integer voxel identifier from zero-padded `x`, `y`, `z` coordinates or reconstruct spatial coordinates from such an ID. These helpers provide a reversible mapping between 3D voxel positions and a single integer key.
#'
#' @param arrange Logical. If `TRUE`, rows are arranged by the newly created `id`.
#' @param rm_coords Logical. If `TRUE`, remove the original coordinate columns after creating the ID.
#' @inherit vbl_doc params
#'
#' @return
#' For `id_add()`: A tibble with a new integer column `id`.
#' For `id_split()`: A tibble in which `id` has been split back into `x`, `y`, and `z`.
#'
#' @details
#' The width of each coordinate block is defined by the maximum observed coordinate
#' in the respective axis using \link{ccs_limits}(). Zero-padding ensures fixed-width
#' encoding, enabling safe concatenation and later reversal.
#'
#' `id_add()`:
#' \itemize{
#'   \item Pads `x`, `y`, `z` to fixed widths.
#'   \item Concatenates them into a single string.
#'   \item Converts the result to an integer `id`.
#' }
#'
#' `id_split()`:
#' \itemize{
#'   \item Verifies that `id` has the expected padded length.
#'   \item Splits the ID into fixed-width substrings.
#'   \item Converts each back into integer coordinates.
#' }
#'
#' These functions provide a deterministic and reversible mapping useful for joins, indexing, hashing, and storage of voxel-based data.
#'
#' @note
#' IDs are not required for standard operations because each voxel is already uniquely
#' identified by its `x`, `y`, `z` coordinates. The integer ID is only a
#' convenience key and becomes useful when transferring information between
#' a \link{vibble} and its corresponding \link{vibble2D}, storing intermediate results,
#' or performing joins that benefit from a single-column identifier.
#'
#' An integer ID column as created with zero-padding has the advantage of more
#' efficient storage usage. See examples.
#'
#' @seealso \link{ccs_limits}()
#'
#' @examples
#' library(dplyr)
#' library(pryr)
#'
#' vbl <- example_vbl()
#'
#' object_size(vbl) # with x,y,z
#'
#' # Add ID
#' vbl_id <- id_add(vbl)
#'
#' object_size(vbl_id) # with id, x, y, z
#'
#' vbl_no_ccs <- select(vbl_id, -x, -y, -z)
#'
#' object_size(vbl_no_ccs) # only id
#'
#' # Reconstruct coordinates
#' vbl_split <- id_split(vbl_id)
#'
#' identical(vbl_split, vbl)
#'
#' @rdname id_add
#' @export
id_add <- function(vbl, arrange = TRUE, rm_coords = FALSE){

  lim <- ccs_limits(vbl)

  # Determine padding width for each axis based on max coordinate
  pad_x <- nchar(max(lim[["x"]]))
  pad_y <- nchar(max(lim[["y"]]))
  pad_z <- nchar(max(lim[["z"]]))

  # Create zero-padded strings for each coordinate
  sx <- sprintf(fmt = paste0("%0", pad_x, "d"), vbl$x)
  sy <- sprintf(fmt = paste0("%0", pad_y, "d"), vbl$y)
  sz <- sprintf(fmt = paste0("%0", pad_z, "d"), vbl$z)

  # Combine into one integer ID
  vbl$id <- as.integer(paste0(sx, sy, sz))

  if(isTRUE(rm_coords)){

    dplyr::select(vbl, id, dplyr::everything(), -x, -y, -z)

  } else {

    dplyr::select(vbl, id, dplyr::everything())

  }

}

#' @rdname id_add
#' @export
id_split <- function(vbl){


  lim <- ccs_limits(vbl)

  # Determine padding widths
  pad_x <- nchar(max(lim[["x"]]))
  pad_y <- nchar(max(lim[["y"]]))
  pad_z <- nchar(max(lim[["z"]]))

  # Convert IDs to character
  id_chr <- sprintf(fmt = "%s", vbl$id)

  # Total width of padded ID
  total_width <- pad_x + pad_y + pad_z

  # Safety check
  if(any(nchar(id_chr) != total_width)){

    stop("IDs do not match expected padded length.")

  }

  # Split into substrings
  vbl$x <- as.integer(substr(id_chr, start = 1, stop = pad_x))
  vbl$y <- as.integer(substr(id_chr, start = pad_x + 1, stop = pad_x + pad_y))
  vbl$z <- as.integer(substr(id_chr, start = pad_x + pad_y + 1, stop = total_width))

  dplyr::select(vbl, x, y, z, dplyr::everything(), -id)

}

#' @keywords internal
plane_to_ccs <- function(plane){

  plane <- match.arg(plane, choices = vbl_planes)

  out <- c("sag" = "x", "axi" = "y", "cor" = "z")[plane]

  unname(out)

}


req_axes_2d <- function(plane, mri = FALSE, ccs_val = TRUE){

  plane <- match.arg(plane, choices = vbl_planes)

  if(plane == "sag"){

    out <- c("col" = "z", "row" = "y")

  } else if(plane == "axi"){

    out <- c("col" = "x", "row" = "z")

  } else if(plane == "cor"){

    out <- c("col" = "x", "row" = "y")

  }

  if(isTRUE(mri)){

    out <- switch_axis_label(out)

  }

  if(!ccs_val){

    out <- purrr::set_names(x = names(out), nm = unname(out))

  }

  return(out)

}



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

