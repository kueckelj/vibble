
#' @title Binarize foreground using Otsu thresholding.
#'
#' @description
#' Identifies foreground voxels using Otsu's method applied to **non-zero,
#' non-missing** intensity values. Intended for MRI intensities where zeros
#' represent background/air.
#'
#' @param x Numeric vector. Intensity values (may include zeros and NA).
#'
#' @return
#' Logical vector of same length as `x`, TRUE indicating foreground.
#'
#' @examples
#' x <- c(rep(0, 100), rnorm(1000, 100, 10), rnorm(50, 400, 20))
#' mask <- binarize_foreground_otsu(x)
binarize_foreground_otsu <- function(x){

  if(!is.numeric(x)){

    stop("`x` must be numeric.")

  }

  n <- length(x)
  nz <- x != 0 & !is.na(x)

  if(!any(nz)){

    return(rep(FALSE, n))

  }

  v <- x[nz]
  v <- v[is.finite(v)]

  if(length(v) == 0){

    return(rep(FALSE, n))

  }

  rng <- range(v)
  if(rng[1] == rng[2]){

    thr <- rng[1]

  } else {

    nbins <- 256
    h <- graphics::hist(v, breaks = nbins, plot = FALSE)

    p <- h$counts / sum(h$counts)
    mids <- h$mids

    omega <- cumsum(p)
    mu <- cumsum(p * mids)
    mu_t <- mu[length(mu)]

    sigma_b2 <- (mu_t * omega - mu)^2 / (omega * (1 - omega))
    sigma_b2[!is.finite(sigma_b2)] <- -Inf

    thr <- mids[which.max(sigma_b2)]

  }

  nz & (x >= thr)

}


#' @title Clip maximum intensity values by quantile (non-zero only).
#'
#' @description
#' Clips extreme high values at an upper quantile computed **only on non-zero
#' intensities**. Intended for MRI intensity vectors where zeros represent
#' background/air.
#'
#' @param x Numeric vector. Intensity values (may include zeros and NA).
#' @param p Numeric scalar. Upper quantile in (0, 1). Default is 0.995.
#'
#' @return
#' Numeric vector of clipped values.
#'
#' @examples
#' x <- c(0, 0, rnorm(1000, 100), 1e6)
#' y <- clip_max_quantile(x)
#'
#' @export
clip_max_quantile <- function(x,
                              p = 0.995){

  if(!is.numeric(x)){

    stop("`x` must be numeric.")

  }

  if(!is.numeric(p) || length(p) != 1 || is.na(p) || p <= 0 || p >= 1){

    stop("`p` must be a single number in (0, 1).")

  }

  x_nz <- x[x != 0 & !is.na(x)]

  if(length(x_nz) == 0){

    return(x)

  }

  thr <- stats::quantile(x_nz, probs = p, names = FALSE, type = 7)

  out <- pmin(x, thr)
  var_limits(out) <- range(out, na.rm = TRUE)

  return(out)

}



#' @importFrom rlang sym
identify_head <- function(vbl, var, var_out = "head", seed = 123, verbose = TRUE){

  stopifnot(is.numeric(vbl[[var]]))
  stopifnot(is.numeric(seed))

  # ID for subsetting later on
  id_exists <- "id" %in% colnames(vbl)
  if(!id_exists){ vbl <- id_add(vbl) }

  # kmeans cluster to separate based on intensity
  set.seed(seed)
  vbl[[var_out]] <- as.character(x = kmeans(x = vbl[[var]], centers = 2)$cluster)

  # foreground based on intensity only
  fg <-
    dplyr::group_by(vbl, !!sym(var_out)) %>%
    dplyr::summarise(avg = mean(!!sym(var), na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(avg == max(avg)) %>%
    dplyr::pull(var = {{var_out}})

  vbl2D_fg <- vibble2D(vbl[vbl[[var_out]] == fg, ], plane = "axi")

  # include everything 'inside' the foreground
  slice_iter <- sort(unique(vbl2D_fg$slice)) # slice seq

  pb <- set_up_progress_bar(length(slice_iter))

  vbl_fg_ids <-
    purrr::map(
      .x = slice_iter,
      .f = function(si){

        if(verbose){ pb$tick() }

        mask <- vbl2D_fg[vbl2D_fg$slice == si, ]

        if(nrow(mask) <= 3){

          out <- mask$id

        } else {

          poly <-
            concaveman::concaveman(
              points = as.matrix(mask[,c("col", "row")]),
              concavity = 1
            ) %>%
            as.data.frame() %>%
            magrittr::set_colnames(value = c("col", "row"))

          out <-
            identify_voxels_in_poly(
              vbl2D = vibble2D(vbl, plane = "axi", slices = si),
              poly = poly,
              strictly = TRUE,
              opt = "keep"
            )[["id"]]

        }

        return(unique(out, mask$id))

      }
    )

  vbl_fg_ids <- purrr::flatten_chr(vbl_fg_ids)

  vbl[[var_out]] <- vbl[["id"]] %in% vbl_fg_ids
  vbl <- update_var_smr(vbl, vars = var_out)

  if(!id_exists){ vbl$id <- NULL }

  return(vbl)

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


set_up_progress_bar <- function(total,
                                format = "Progress: [:bar] :percent eta: :eta",
                                clear = FALSE,
                                width = 100,
                                ...){

  progress::progress_bar$new(
    format = format,
    total = total,
    clear = clear,
    width = width,
    ...)

}



#' @title Variable selectors for vibble columns
#' @description
#' Helpers to retrieve column names from a vibble based on their type.
#'
#' @param type Character value. The data variable type of interest. Must be one of *c('cateogorical', 'mask', 'numeric')*.
#' @param ordered Logical or \code{NULL}. Controls how ordered factors are handled.
#'
#' \itemize{
#'   \item{\code{TRUE}: } Only ordered factors are returned.
#'   \item{\code{FALSE}: } Ordered factors are excluded.
#'   \item{\code{NULL}: } Both ordered and unordered factors are returned.
#' }
#'
#' @inherit vbl_doc params
#'
#' @return A character vector with column names:
#'
#' \itemize{
#'   \item \link{vars_data}() names of all non-spatial variables.
#'   \item \link{vars_categorical}() names of non-spatial variables stored as factors labels.
#'   \item \link{vars_mask}() names of non-spatial variables stored as logical masks.
#'   \item \link{vars_numeric}() names of non-spatial variables stored as numeric vectors.
#'   \item \link{vars_spatial}() names of spatial coordinate variables such as `x`, `y`, `z`, `col`, `row`, and `slice`.
#'   \item \link{vars_type}() names of the variables of the requested type.
#' }
#'
#' If no variables match the requirements the output is an empty character vector.
#'
#' @name vars_vibble
NULL

#' @rdname vars_vibble
#' @export
vars_data <- function(vbl){

  spatial <- vars_spatial(vbl)

  setdiff(colnames(vbl), spatial)

}

#' @rdname vars_vibble
#' @export
vars_categorical <- function(vbl, ordered = NULL){

  vars_type(vbl, type = "categorical", ordered = ordered)

}

#' @rdname vars_vibble
#' @export
vars_mask <- function(vbl){

   vars_type(vbl, type = "mask")

}

#' @rdname vars_vibble
#' @export
vars_numeric <- function(vbl){

  vars_type(vbl, type = "numeric")

}

#' @rdname vars_vibble
#' @export
vars_spatial <- function(vbl){

  spatial <- c("x", "y", "z", "col", "row", "slice", "slice_idx")

  intersect(colnames(vbl), spatial)

}

#' @rdname vars_vibble
#' @export
vars_type <- function(vbl, type = "all", ordered = NULL){

  type <- .match_arg(type, choices = c(vbl_data_var_types, "data", "all"))

  if(type == "categorical"){

    out <-
      names(purrr::keep(.x = vbl[,vars_data(vbl)], .p = is_categorical_var)) %>%
      intersect(x = ., y = vars_data(vbl))

    if(isTRUE(ordered) & length(out) != 0){

      out <- names(purrr::keep(.x = vbl[,out], .p = is.ordered))

    } else if(isFALSE(ordered)){

      out <- names(purrr::discard(.x = vbl[,out], .p = is.ordered))

    }

  } else if(type == "mask"){

    out <-
      names(purrr::keep(.x = vbl[,vars_data(vbl)], .p = is_mask_var)) %>%
      intersect(x = ., y = vars_data(vbl))

  } else if(type == "numeric"){

    out <-
      names(purrr::keep(.x = vbl[,vars_data(vbl)], .p = is_numeric_var))%>%
      intersect(x = ., y = vars_data(vbl))

  } else if(type == "data"){

    out <- vars_data(vbl)

  } else {

    out <- names(vbl)

  }

  return(out)

}




