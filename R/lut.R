#' @title Create a lookup table file
#' @description
#' Construct a two-column lookup table (LUT) from the levels of a factor
#' variable, assigning sequential integer indices to each label.
#'
#' @details
#' The function requires `vbl[[var]]` to be a factor. The returned LUT has:
#' \itemize{
#'   \item \code{index}: integer sequence \code{1:nlevels(var)}
#'   \item \code{label}: factor levels in order
#' }
#'
#' @inherit vbl_doc params
#' @inheritParams vbl_doc_var_label
#'
#' @return
#' A tibble with columns \code{index} and \code{label}.
#'
#' @examples
#' \dontrun{
#' make_lut(vbl, "tissue_class")
#' }
#'
#' @export
make_lut <- function(vbl, var){

  stopifnot(is.factor(vbl[[var]]))

  labels <- levels(vbl[[var]])

  tibble::tibble(
    index = 1:length(labels),
    label = labels
  )

}


#' @title Map integer-coded voxel values to factor labels using a lookup table
#' @description
#' Replace an integer-coded variable in a vibble with a factor whose levels
#' are defined by a lookup table (LUT). The LUT may be provided as a character
#' vector or as a two-column data.frame.
#'
#' @details
#' The function first verifies that the input variable contains integer-
#' interpretable values. A character LUT is interpreted as an ordered sequence
#' of labels (with indices `1:length(lut)`), whereas a data.frame LUT must have
#' a numeric integer first column and a character second column. All LUT labels
#' must be unique.
#'
#' Any integer values present in `vbl[[var]]` but missing from the LUT produce
#' a warning. The variable is then recoded by joining its integer values with
#' the LUT and converting the resulting labels into a factor with the specified
#' ordering. The recoded variable is stored in `var_out`, which defaults to
#' `var`.
#'
#' @section LUT handling:
#' \itemize{
#'   \item **Character LUT**
#'   Treated as:
#'   \preformatted{
#'   index = 1:length(lut)
#'   label = lut
#'   }
#'   All labels must be unique.
#'
#'   \item **data.frame LUT**
#'   Must contain at least two columns. The first must be numeric integer
#'   indices, the second unique character labels. Additional columns are
#'   ignored.
#'
#'   \item **Missing LUT entries**
#'   Integer values present in the variable but absent from the LUT trigger a
#'   warning; these entries receive `NA` labels after joining.
#' }
#'
#' @param var
#' Character. Name of the integer-coded variable to remap.
#'
#' @param ordered
#' Logical. If `TRUE`, the resulting factor is ordered. Defaults to `FALSE`.
#'
#' @param var_out
#' Character. Output variable name. Defaults to the same name as `var`.
#'
#' @inherit vbl_doc params
#'
#' @return
#' The input vibble with a new or replaced factor variable containing mapped
#' labels.
#'
#' @examples
#' \dontrun{
#' # Using a character LUT: indices 1,2,3 map to labels A,B,C
#' map_lut(vbl, var = "seg", lut = c("A", "B", "C"), ordered = TRUE)
#'
#' # Using a data.frame LUT
#' lut_df <- data.frame(
#'   index = c(1, 2, 5),
#'   label = c("GM", "WM", "CSF")
#' )
#' map_lut(vbl, "tissue", lut = lut_df, ordered = FALSE)
#' }
#'
#' @export

map_lut <- function(vbl, var, lut, ordered = FALSE, var_out = var, verbose = TRUE){

  # test if interpretable as an integer
  stopifnot(all(vbl[[var]] == as.integer(vbl[[var]])))

  # read lut if path is provided
  if(is.character(lut) && length(lut)==1){

    glue_message("Reading LUT from '{lut}'.", verbose = verbose)

    lut <- read_lut(lut)

  }

  # test lut
  if(is.character(lut)){

    if(!all(table(lut)==1)){

      stop("If character, `lut` must contain unique values.")

    }

    lut <-
      tibble::tibble(
        index = 1:length(lut),
        label = lut
      )

  } else if(is.data.frame(lut)){

    if(!is.numeric(lut[[1]]) || !all(lut[[1]] == as.integer(lut[[1]]))){

      stop("If data.frame, first column of `lut` must be numeric and interpretable as integer.")

    }

    if(!dplyr::n_distinct(lut[[2]]) == nrow(lut)){

      stop("If data.frame, second column of `lut` must contain unique values and must be interpretable as character.")

    }

    lut[[1]] <- as.integer(lut[[1]])
    lut[[2]] <- as.character(lut[[2]])

    names(lut)[1:2] <- c("index", "label")

  } else {

    stop("`lut` must be a character vector or a data.frame.")

  }

  int_values <- unique(vbl[[var]])
  missing <- int_values[!int_values %in% lut[[1]]]

  if(length(missing) != 0){

    missing <- stringr::str_c(missing, collapse = ", ")
    warning(glue::glue("Variable `{var}` contains values not found in `lut`: {missing}."))

  }

  # convert and join
  df <-
    dplyr::left_join(x = tibble::tibble(index = vbl[[var]]), y = lut, by = "index") %>%
    dplyr::mutate(label = factor(label, levels = lut$label, ordered = ordered))

  vbl[[var_out]] <- df$label

  return(vbl)

}


#' @title Read a LUT file
#'
#' @description
#' Reads a lookup table from a file with extension `.csv`, `.csv2`, `.tsv`, or `.tab`.
#' Uses the appropriate `readr` parser based on the file extension.
#'
#' @param path Path to the LUT file.
#'
#' @return A tibble containing the lookup table.
#'
#' @examples
#' \dontrun{
#' lut <- read_lut("atlas_lut.csv")
#' }

read_lut <- function(path){

  ext <- tolower(tools::file_ext(path))

  if(ext == "csv2"){

    readr::read_csv2(path, show_col_types = FALSE)

  } else if(ext == "csv"){

    readr::read_csv(path, show_col_types = FALSE)

  } else if(ext %in% c("tsv", "tab")){

    readr::read_tsv(path, show_col_types = FALSE)

  } else {

    stop("Unknown LUT file extension. Must be either .csv, .csv2, .tsv or .tab")

  }

}
