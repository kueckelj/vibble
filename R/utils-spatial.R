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
