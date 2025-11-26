#' @title Offset directions
#' @description
#' Contains the valid direction choices used for offsetting 2D vibbles
#' with `offset_dir`.
#'
#' @return A character vector with allowed offset directions.
#'
#' @export
offset_dir_choices <-
  c("left", "right", "top", "bottom") %>%
  c(., paste0(., "-flip"))


#' @export
vbl_ccs_axes <- c("x", "y", "z")

#' @export
vbl_data_var_types <- c("label", "mask", "numeric")

#' @export
vbl_planes <- c("Sagittal" = "sag", "Axial" = "axi", "Coronal" = "cor")

#' @export
vbl_planes_pretty <- c("sag" = "Sagittal", "axi" = "Axial", "cor" = "Coronal")
