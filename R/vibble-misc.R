

#' @importFrom glue glue
#' @importFrom rlang caller_env
glue_message <- function(..., defer = FALSE, verbose = TRUE){

  if(verbose){

    msg <- glue::glue(..., .envir = caller_env(n = 1))

    if(is.numeric(defer)){

      withr::defer(message(msg), envir = caller_env(n = 1))

    } else {

      message(msg)

    }

  }


}

#' @importFrom glue glue
#' @importFrom rlang caller_env
glue_warning <- function(..., .envir = caller_env(), verbose = TRUE){

  if(!verbose){

    return(invisible(NULL))

  }

  warning(glue(..., .envir = .envir), call. = FALSE)

}

#' @importFrom glue glue
#' @importFrom rlang caller_env
glue_stop <- function(..., envir = caller_env()){

  stop(glue(..., envir = .envir), call. = FALSE)

}

#' @importFrom rlang sym
identify_head <- function(vbl, var, var_out = "head", seed = 123, verbose = TRUE){

  stopifnot(is.numeric(vbl[[var]]))
  stopifnot(is.numeric(seed))

  # ID for subsetting later on
  id_exists <- "id" %in% colnames(vbl)
  if(!id_exists){ vbl <- id_add(vbl, arrange = FALSE) }

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





