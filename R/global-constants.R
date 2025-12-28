




#' @title Default image anchors
#' @export
img_anchors <- list(
  "center" = c(0.5, 0.5),
  "top" = c(0.5, 0),
  "bottom" = c(0.5, 1),
  "left" = c(0, 0.5),
  "right" = c(1, 0.5),
  "top-left" = c(0, 0),
  "top-right" = c(1, 0),
  "bottom-left" = c(1, 0),
  "bottom-right" = c(0, 1)
)



#' @title Offset directions
#' @description
#' Contains the valid direction choices used for offsetting 2D vibbles
#' with `offset_dir`.
#'
#' @return A character vector with allowed offset directions.
#'
#' @export

#' @export
vbl_ccs_axes <- c("x", "y", "z")

#' @export
vbl_data_var_types <- c("categorical", "mask", "numeric")

#' @export
vbl_planes <- c("sag", "axi", "cor")

#' @export
vbl_planes_pretty <- c("sag" = "Sagittal", "axi" = "Axial", "cor" = "Coronal")

#' @export
vbl_protected_vars <- c("x", "y", "z", "slice", "slice_idx")



