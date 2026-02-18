

bitpix_mapping <- c(
  UINT8       = 8,
  INT8        = 8,
  INT16       = 16,
  UINT16      = 16,
  INT32       = 32,
  UINT32      = 32,
  FLOAT32     = 32,
  FLOAT64     = 64,
  INT64       = 64,
  UINT64      = 64,
  FLOAT128    = 128,
  COMPLEX64   = 64,
  COMPLEX128  = 128,
  COMPLEX256  = 256,
  RGB24       = 24
)

datatype_mapping <- c(
  UINT8       = 2,
  INT16       = 4,
  INT32       = 8,
  FLOAT32     = 16,
  COMPLEX64   = 32,
  FLOAT64     = 64,
  RGB24       = 128,
  INT8        = 256,
  UINT16      = 512,
  UINT32      = 768,
  INT64       = 1024,
  UINT64      = 1280,
  FLOAT128    = 1536,
  COMPLEX128  = 1792,
  COMPLEX256  = 2048
)


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
  "bottom-left" = c(0, 1),
  "bottom-right" = c(1, 1)
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
vbl_planes_full <- c("sag" = "sagittal", "axi" = "axial", "cor" = "coronal")

#' @export
vbl_planes_pretty <- c("sag" = "Sagittal", "axi" = "Axial", "cor" = "Coronal")

#' @export
vbl_protected_vars <- c("x", "y", "z", "slice", "slice_idx")



