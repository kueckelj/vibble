
slice_seq <- function(vbl, plane, expr, buffer = 0, ...){

  vbl2D <- vibble2D(vbl, plane = plane)

  seq_out <-
    dplyr::filter(vbl2D, !!rlang::enquo(expr)) %>%
    dplyr::pull(var = "slice") %>%
    unique() %>%
    sort()

  if(buffer > 0){

    if(buffer < 1){

      buffer <- ceiling(length(seq_out)*buffer)

    }

    front <- (min(seq_out)-buffer):(min(seq_out)-1)
    back <- (max(seq_out)+1):(max(seq_out)+n)

    seq_out <- c(front, seq_out, back)

    lim_slice <- var_smr(vbl2D, var = "slice")$limits
    seq_out <- seq_out[within_limits(seq_out, lim_slice)]

  }

  seq_out <- seq(from = min(seq_out), to = max(seq_out), ...)
  seq_out <- floor(seq_out)

  return(seq_out)

}
