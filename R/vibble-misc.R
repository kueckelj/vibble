





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




