# Spatial reference and coordinate-system utilities.


bb2D <- function(vbl2D, var, slices = NULL, buffer = 0.05){

  stopifnot(is.logical(vbl2D[[var]]) && any(vbl2D[[var]]))

  stopifnot(length(buffer) %in% c(1,2) && all(buffer >= 0))
  if(length(buffer) == 1){ buffer <- rep(buffer, 2) }

  vbl_wm <- vbl2D[vbl2D[[var]], ]

  if(!is.numeric(slices)){ slices <- sort(unique(vbl_wm$slice)) }

  purrr::map_df(
    .x = slices,
    .f = function(slice){

      slice_df <- dplyr::filter(vbl_wm, slice == {{slice}})

      bcol <- if(buffer[1] < 0){ diff(range(slice_df$col))*buffer[1] } else { buffer[1] }

      brow <- if(buffer[2] < 0){ diff(range(slice_df$row))*buffer[2] } else { buffer[2] }

      tibble(
        mask = var,
        plane = attr(vbl_wm, "plane"),
        slice = slice,
        cmin = min(slice_df$col) - bcol,
        cmax = max(slice_df$col) + bcol,
        rmin = min(slice_df$row) - brow,
        rmax = max(slice_df$row) + brow
      )

    }
  )

}

bb3D <- function(vbl, var, buffer = 0){

  stopifnot(is.logical(vbl[[var]]))

  purrr::imap(
    .x = vbl[vbl[[var]], ccs_labels],
    .f = function(avar, axis){

      alim <- ccs_limits(vbl)[[axis]]
      r <- range(avar)

      if(buffer != 0){

        if(buffer < 1){

          buffer <- diff(r)*buffer

        }

        r[1] <- r[1]-buffer
        r[2] <- r[2]+buffer

        r[1] <- ifelse(r[1] < min(alim), min(alim), r[1])
        r[2] <- ifelse(r[2] > max(alim), max(alim), r[2])

      }

      return(r)

    })

}


identify_voxels_in_poly <- function(vbl2D,
                                    poly,
                                    strictly,
                                    opt = "keep"){

  res <-
    sp::point.in.polygon(
      point.x = vbl2D[["col"]],
      point.y = vbl2D[["row"]],
      pol.x = poly[["col"]],
      pol.y = poly[["row"]]
    )

  inside <- if(base::isTRUE(strictly)){ 1 } else { c(1,2,3) }

  if(opt == "keep"){

    vbl2D <- vbl2D[res %in% inside, ]

  } else if(opt == "remove"){

    vbl2D <- vbl2D[!res %in% inside, ]

  } else {

    vbl2D[[opt]] <- res %in% inside

  }

  return(vbl2D)

}
