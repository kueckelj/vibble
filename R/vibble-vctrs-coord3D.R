# coord3D constructor -----------------------------------------------------

#' @export
new_coord3D <- function(x = integer(),
                        limits = c(0, 1),
                        pointer = c("L", "I", "P"),
                        step_mm = 1
                        ){

  if(!is.integer(x)){

    rlang::abort("`x` must be an integer vector for coord3D.")

  }

  if(!is_limit(limits) & is.integer(limits) & min(limits) == 1){

    rlang::abort("`limits` must be a valid limit (see `?is_limit`), consisting of integer values with min()==1.")

  }

  if(!all(within_limits(x, l = limits))){

    rlang::abort("All values of `x` must lie within limits.")

  }

  if(!is.numeric(step_mm) || !step_mm > 0){

    rlang::abort("`step_mm` must be a numeric value larger than 0.")

  }


  pointer <- rlang::arg_match0(pointer[1], value = c("L", "I", "P"))

  new_vctr(
    x,
    limits = limits,
    pointer = pointer,
    step_mm = step_mm,
    class = "coord3D"
  )

}

#' @export
coord3D <- function(x = integer(), limits, pointer, step_mm){

  if(!is.integer(x)){

    x <- vec_cast(x, integer())

  }

  new_coord3D(x = x, limits = limits, pointer = pointer, step_mm = step_mm)

}


# attribute accessors -----------------------------------------------------


#' @export
limits.coord3D <- function(x){

  attr(x, "limits", exact = TRUE)

}

#' @export
pointer.coord3D <- function(x){

  attr(x, "pointer", exact = TRUE)

}

#' @export
step_mm.coord3D <- function(x){

  attr(x, "step_mm", exact = TRUE)

}


# printing ----------------------------------------------------------------

#' @export
obj_sum.coord3D <- function(x){ "3D" }

#' @export
pillar_shaft.coord3D <- function(x, ...){

  # print as integer for the main column
  out <- format(vctrs::vec_data(x), trim = TRUE, ...)
  pillar::new_pillar_shaft_simple(out)

}

#' @export
format.coord3D <- function(x, ...){

  vals <- vctrs::vec_data(x)

  format(vals, ...)

}

#' @export
obj_print_footer.coord3D <- function(x, ...){

  lim  <- limits.coord3D(x)
  ptr  <- pointer.coord3D(x)
  stp  <- step_mm.coord3D(x)
  rng  <- range(vctrs::vec_data(x), na.rm = TRUE)

  # pointer
  part_pointer <- paste0("-> ", ptr)

  # limits
  if(!is.null(lim) && length(lim) == 2L && all(is.finite(lim))){

    part_limits <- paste0("[", lim[1], ", ", lim[2], "]")

  } else {

    part_limits <- "<none>"

  }

  if(all(is.finite(rng))){

    part_range <- paste0("[", rng[1], ", ", rng[2], "]")

  } else {

    part_range <- "[<NA>]"

  }

  # step
  if(is.finite(stp)){

    part_step <- paste0(stp, "mm")

  } else {

    part_step <- "<NA> mm"

  }

  line <- paste(part_limits, part_range, part_step, part_pointer, sep = " | ")

  cat("# ", line, "\n", sep = "")

}




