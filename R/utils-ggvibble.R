# Utility functions within the ggvibble framework

densify_poly <- function(df, n = 50){
  df_closed <- rbind(df, df[1, ])

  purrr::map_dfr(
    1:(nrow(df_closed)-1),
    function(i){
      col <- seq(df_closed$col[i], df_closed$col[i+1], length.out = n)
      row <- seq(df_closed$row[i], df_closed$row[i+1], length.out = n)
      tibble(col = col, row = row)
    }
  )
}

eval_tidy_alpha <- function(vbl2D, alpha){

  alpha_inp <- rlang::eval_tidy(alpha, data = vbl2D)

  if(is.numeric(alpha_inp)){

    if(length(alpha_inp) == 1){

      alpha_use <- alpha_inp

    } else if(length(alpha_inp) == 2) {

      alpha_use <-
        scales::rescale(
          x = vbl2D[[var]],
          to = c(pmax(0, min(alpha_inp)), pmin(1, max(alpha_inp))),
          from = var_smr(vbl2D, var)$limits
        )

    } else if(length(alpha_inp) == nrow(vbl2D)) {

      alpha_lim <- range(alpha_inp, na.rm = TRUE)

      if(!within_limits(alpha_lim[1], c(0,1))){ alpha_lim[1] <- 0 }
      if(!within_limits(alpha_lim[2], c(0,1))){ alpha_lim[2] <- 1 }

      alpha_use <- scales::rescale(x = alpha_inp, to = alpha_lim)

    } else {

      stop("Invalid input for `alpha`.")

    }

  } else {

    stop("Invalid input for `alpha`.")

  }

  return(alpha_use)

}


#' @importFrom rlang enquo
#' @export
filter_slices <- function(vbl2D, slices = NULL, cond = enquo()){

  if(is.numeric(slices)){

    slices <- unique(as.integer(slices))

    vbl2D <- dplyr::filter(vbl2D, slice %in% {{slices}})

  }

  if(!rlang::quo_is_missing(cond) && !rlang::quo_is_null(cond)){

    vbl2D <- dplyr::filter(vbl2D, !!cond)

  }

  return(vbl2D)

}


grid_intercepts <- function(input, limits){

  if(length(input) > 1){

    out <- input

  } else if(length(input) == 1){

    if(input < 1){

      input <- max(limits)*input

    }

    half <- max(limits)/2

    icpts1 <- half
    while(max(icpts1) < max(limits)){

      n <- length(icpts1)
      icpts1[n + 1] <- half + input*n

    }

    icpts2 <- half
    while(min(icpts2) > 0){

      n <- length(icpts2)
      icpts2[n + 1] <- half - input*n

    }

    out <- unique(c(icpts1, icpts2))

  } else {

    out <- NULL

  }

  return(out)

}



ratio2D <- function(vbl2D){

  axes <- req_axes_2d(plane(vbl2D))

  col <- max(ccs_limits(vbl2D)[[axes[["col"]]]])

  row <- max(ccs_limits(vbl2D)[[axes[["row"]]]])

  col/row

}
