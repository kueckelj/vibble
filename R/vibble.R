# generic and methods for the vibble constructor

#' @export
vibble <- function(x, ...){

  UseMethod(generic = "vibble")

}

#' @export
vibble.data.frame <- function(x, ccs_limits, ...){

  stopifnot(is_bb3D(ccs_limits))

  new_vbl(
    data = x,
    ccs_limits = ccs_limits
  )

}

#' @export
vibble.oro.nifti <- function(x,
                             var,
                             lut = NULL,
                             ordered = FALSE,
                             rm0 = TRUE,
                             verbose = vbl_opts("verbose"),
                             ...){

  nifti_to_vbl(x, var = var, lut = lut, rm0 = rm0, verbose = verbose)

}

#' @export
vibble.vbl2D <- function(x, ...){

  if(is_offset(x)){

    stop("Input 2D vibbles that are offset can not be converted to a 3D vibble.")

  }

  rn <- req_axes_3D(plane = plane(x))

  out <-
    dplyr::ungroup(x) %>%
    dplyr::rename(!!rn)

  class(out) <- stringr::str_replace(class(out), "vbl2D", "vbl")

  for(a in vbl_attr2D){

    attr(out, which = a) <- NULL

  }

  return(out)

}




