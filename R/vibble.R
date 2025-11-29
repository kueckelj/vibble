# generic and methods for the vibble constructor

vibble <- function(x, ...){

  UseMethod(generic = "vibble")

}

vibble.character <- function(x,
                             # single file
                             var,
                             lut = NULL,
                             ordered = FALSE,
                             # multiple files/folder,
                             rgx_fp = ".*",
                             rgx_var = ".*",
                             recursive = FALSE,
                             join = "full",
                             # all
                             rm0 = TRUE,
                             verbose = vbl_opts("verbose"),
                             ...){

  out <- NULL

  # single file handling
  if(length(x) == 1 && file.exists(x) && !dir.exists(x)){

    out <- nifti_to_vbl(x, var = var, lut = lut, rm0 = rm0, verbose = verbose)

  }

  # multiple files / folder handling
  folder_input <- length(x) == 1 && dir.exists(x)
  multiple_files <- (length(x) > 1)

  if(folder_input | multiple_files){ # -> multiple files


    out <-
      niftis_to_vbl(
        input = x,
        rgx_fp = rgx_fp,
        rgx_var = rgx_var,
        recursive = recursive,
        join = join,
        rm0 = rm0,
        verbose = verbose
        )

  }

  # check success
  if(!is_vbl(out)){

    stop("`x` failed to be interpreted as a single file, a single folder or multiple files.")

  }

  return(out)


}


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


