#' @title Utilities for mapping colors to categorical variables
#' @description
#' Helpers for managing categorical color palettes used for categorical variables.
#' They provide:
#' \itemize{
#'   \item grouped access to predefined palettes (`color_palettes()`),
#'   \item valid palette identifiers for the `clrp` argument of \link{scale_fill_categorical}() ,
#'   \item a constructor to obtain hex color vectors for factor/label levels (`color_vector()`).
#' }
#'
#' @details
#' `color_palettes()` returns a nested list of fixed-length categorical palettes:
#' \itemize{
#'   \item top-level names are palette families (currently `"ggsci"` and `"matplotlib"`),
#'   \item each family is a named list of palettes,
#'   \item each palette is a character vector of hex colors.
#' }
#'
#' `clrp_opts()` converts `color_palettes()` into a list of palette-name vectors per family and
#' adds an additional `"viridis"` entry containing the result of `color_opts_viridis()`.
#' This gives a grouped registry of categorical palette identifiers.
#'
#' `clrp_opts_vec()` flattens the list from `clrp_opts()` into a single sorted character
#' vector. It represents the full set of valid categorical palette names (`clrp`) that
#' \code{\link{color_vector}()} accepts (apart from `"default"`, which is handled specially).
#'
#' `color_vector()` is the workhorse for discrete color mappings. It turns a palette name
#' (`clrp`) and either a set of labels (`names`) or a requested size (`nc`) into a vector of
#' hex colors, optionally named and partially overridden via `clrp_adjust`.
#'
#' Palette resolution in `color_vector()`:
#' \itemize{
#'   \item The number of required colors `nc` is determined by:
#'     \itemize{
#'       \item `nc <- length(names)` if `names` is supplied, otherwise
#'       \item `nc` if numeric, otherwise
#'       \item a palette-specific default from `n_colors[[clrp]]`, unless `clrp` is
#'         `"default"` or a viridis option (then an explicit `names` or `nc` is required).
#'     }
#'   \item A base palette is chosen by:
#'     \itemize{
#'       \item `"default"`: `scales::hue_pal()(nc)`,
#'       \item viridis options: `viridis::viridis(n = nc, option = clrp)`,
#'       \item RColorBrewer options: `RColorBrewer::brewer.pal(n = nc, name = clrp)`,
#'       \item other: looking up `clrp_opts()[[clrp]]`.
#'     }
#'   \item If `names` is provided:
#'     \itemize{
#'       \item If `length(names)` exceeds the palette size, a warning is issued and a fallback
#'         qualitative palette is generated via `grDevices::hcl()`,
#'       \item the resulting color vector is named with `names`,
#'       \item if `clrp_adjust` is a named character vector, the corresponding entries are
#'         replaced while preserving the order of `names`.
#'     }
#' }
#'
#' @param clrp Character scalar naming the palette in `color_vector()`.
#'   Must be one of \code{clrp_opts_vec()} or `"default"` (handled specially).
#' @param names Optional character vector of label / factor levels.
#'   If supplied, its length defines the number of colors and the returned vector is named.
#' @param clrp_adjust Optional named character vector of hex colors used to override specific
#'   entries in the palette returned by `color_vector()`. Names must be a subset of `names`.
#' @param nc Optional integer specifying the number of colors to generate when `names` is not
#'   supplied.
#'
#' @return
#' \itemize{
#'   \item `color_palettes()`: Nested list of palette families and palettes (hex codes).
#'   \item `clrp_opts()`: List of palette-name vectors grouped by family and `"viridis"`.
#'   \item `clrp_opts_vec()`: Character vector of all valid `clrp` identifiers.
#'   \item `color_vector()`: Character vector of hex colors, optionally named by `names`.
#' }
#'
#' @examples
#' # Available palette families and palettes
#' cps <- color_palettes()
#' names(cps)
#' names(cps$ggsci)
#'
#' # Grouped and flattened categorical palette options
#' clrp_opts()
#' clrp_opts_vec()
#'
#' # Build a named color vector for label levels
#' labs <- c("A", "B", "C")
#' cols <- color_vector(clrp = "jco", names = labs)
#'
#' # Override individual levels
#' cols2 <- color_vector(
#'   clrp = "jco",
#'   names = labs,
#'   clrp_adjust = c(A = "#000000", C = "#FFFFFF")
#' )
#'
#' @name vbl_doc_colors_categorical
NULL


#' @rdname vbl_doc_colors_categorical
#' @export
clrp_opts <- function(){

  c(
    purrr::map(.x = color_palettes(), .f = names),
    list(
      "viridis" = color_opts_viridis(),
      RColorBrewer = c("Accent", "Dark2", "Greys", "Paired", "Pastel1", "Pastel2", "Set1", "Set2", "Set3")
    )
  )

}

#' @rdname vbl_doc_colors_categorical
#' @export
clrp_opts_vec <- function(){

  purrr::flatten_chr(clrp_opts()) %>%
    sort()

}

#' @rdname vbl_doc_colors_categorical
#' @export
color_palettes <- function(){

  list(

    ggsci = list(
      jco = c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF", "#7AA6DCFF", "#003C67FF", "#8F7700FF", "#3B3B3BFF", "#A73030FF", "#4A6990FF"),

      npg = c("#E64B35FF", "#4DBBD5FF", "#00A087FF", "#3C5488FF", "#F39B7FFF", "#8491B4FF", "#91D1C2FF", "#DC0000FF", "#7E6148FF", "#B09C85FF"),

      aaas = c("#3B4992FF", "#EE0000FF", "#008B45FF", "#631879FF", "#008280FF", "#BB0021FF", "#5F559BFF", "#A20056FF", "#808180FF", "#1B1919FF"),

      nejm = c("#BC3C29FF", "#0072B5FF", "#E18727FF", "#20854EFF", "#7876B1FF", "#6F99ADFF", "#FFDC91FF", "#EE4C97FF","#b15928","#ffff33"),

      lo = c("#00468BFF", "#ED0000FF", "#42B540FF", "#0099B4FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "#ADB6B6FF", "#1B1919FF", "#800000FF"),

      jama = c("#374E55FF", "#DF8F44FF", "#00A1D5FF", "#B24745FF", "#79AF97FF", "#6A6599FF", "#80796BFF", "#008B45FF", "#fdbf6f", "#377eb8"),

      uc = c("#800000FF", "#767676FF", "#FFA319FF", "#8A9045FF", "#155F83FF", "#C16622FF", "#8F3931FF", "#58593FFF", "#350E20FF", "#1F77B4FF")
    ),

    matplotlib = list(

      tab10 = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf"),

      tab20 = c("#1f77b4", "#aec7e8", "#ff7f0e", "#ffbb78", "#2ca02c", "#98df8a", "#d62728", "#ff9896", "#9467bd", "#c5b0d5", "#8c564b", "#c49c94", "#e377c2", "#f7b6d2", "#7f7f7f", "#c7c7c7", "#bcbd22", "#dbdb8d", "#17becf", "#9edae5"),

      tab20b = c("#393b79", "#5254a3", "#6b6ecf", "#9c9ede", "#637939", "#8ca252", "#b5cf6b", "#cedb9c", "#8c6d31", "#bd9e39", "#e7ba52", "#e7cb94", "#843c39", "#ad494a", "#d6616b", "#e7969c", "#7b4173", "#a55194", "#ce6dbd", "#de9ed6"),

      tab20c = c("#3182bd", "#6baed6", "#9ecae1", "#c6dbef", "#e6550d", "#fd8d3c", "#fdae6b", "#fdd0a2", "#31a354", "#74c476", "#a1d99b", "#c7e9c0", "#756bb1", "#9e9ac8", "#bcbddc", "#dadaeb", "#636363", "#969696", "#bdbdbd", "#d9d9d9")

    )

  )

}

#' @rdname vbl_doc_colors_categorical
#' @export
color_vector <- function(clrp, names = NULL, clrp_adjust = NULL, nc = NA){

  stopifnot(clrp %in% c("default", clrp_opts_vec()))

  if(is.character(names)){

    nc <- length(names)

  } else if(is.na(nc) | !is.numeric(nc)){

    if(clrp %in% c("default", color_opts_viridis)){

      stop("If `clrp` among 'default' or viridis options, please specify either `names` or `nc`.")

    } else {

      nc <- n_colors[[clrp]]

    }

  }

  if(clrp == "default"){

    clr_vector <- scales::hue_pal()(nc)

  } else if(clrp %in% clrp_opts()$viridis){

    clr_vector <- viridis::viridis(n = nc, option = clrp)

  } else if(clrp %in% clrp_opts()$RColorBrewer){

    clr_vector <- RColorBrewer::brewer.pal(n = nc, name = clrp)

  } else { # either matplotlib or ggsci

    clr_vector <- purrr::flatten(color_palettes())[[clrp]]

  }

  if(is.character(names)){

    n_names <- length(names)
    n_colors <- length(clr_vector)

    if(n_names > n_colors){

      warning(glue::glue("Chosen colorpalette '{clrp}' provides {n_colors} colors. Need {n_names} colors. Returning 'default' colorpalette."))

      hues <- seq(15, 375, length = n_names + 1)
      clr_vector <- grDevices::hcl(h = hues, l = 65, c = 100)[1:n_names]

    }

    clr_vector <- stats::setNames(object = clr_vector, nm = names)

    if(is.character(clrp_adjust) && .is_named(clrp_adjust)){

      clrp_adjust <- .keep_named(clrp_adjust)

      clrp_adjust_names <- names(clrp_adjust)

      clr_vector <- clr_vector[!names(clr_vector) %in% clrp_adjust_names]

      clr_vector <- c(clr_vector, clrp_adjust)[names]

    }

  }

  return(clr_vector)

}


#' @title Utilities for mapping colors to numeric variables
#' @description
#' Available color spectra for numeric voxel variables.
#' These helpers define and expose the full set of continuous color-spectrum
#' identifiers used by \link{scale_fill_numeric}().
#'
#' @details
#' `clrsp_opts()` returns a named list of spectrum groups.
#' Each entry is a character vector of valid spectrum names.
#' The groups correspond to standard categories used for numeric visualization:
#'
#' \itemize{
#'   \item \strong{sequential_single_hue}:
#'   single-hue gradients (e.g. "Blues", "Greens", "Purples")—best for monotonic data.
#'
#'   \item \strong{sequential_multi_hue}:
#'   multi-hue gradients and perceptually uniform colormaps (including viridis variants),
#'   suitable for a wide range of numeric intensities.
#'
#'   \item \strong{diverging}:
#'   colormaps designed to highlight deviation around a midpoint (e.g. negative/positive
#'   directionality).
#' }
#'
#' These spectrum names form the allowed input values for the `clrsp` argument in numeric
#' color scales in vibble/ggvibble.
#'
#' `clrsp_opts_vec()` flattens the grouped list into a single sorted character vector.
#' This is useful for validation, UI dropdowns, and error messages.
#'
#' @return
#' \itemize{
#'   \item `clrsp_opts()`: A named list of character vectors (three groups: sequential_single_hue,
#'     sequential_multi_hue, diverging).
#'   \item `clrsp_opts_vec()`: A sorted character vector of all valid spectrum names.
#' }
#'
#' @examples
#' # Grouped numeric spectra
#' opts <- clrsp_opts()
#' names(opts)
#' opts$sequential_multi_hue
#'
#' # Flattened numeric spectrum options
#' clrsp_opts_vec()
#'
#' # Examples for ggvibles:
#' vbl <- example_vbl()
#'
#' # Example 1: base color name
#' ggplane(vbl, plane = "axi", slice = 90, var = "raw_t1") +
#'   layer_numeric(var = "score", clrsp = "red")
#'
#' # Example 2: sequential colorspace palette
#' ggplane(vbl, plane = "axi", slice = 90, var = "raw_t1") +
#'   layer_numeric(var = "score", clrsp = "Inferno")
#'
#' # Example 3: diverging colorspace palette
#' ggplane(vbl, plane = "axi", slice = 90, var = "raw_t1") +
#'   layer_numeric(var = "score", clrsp = "Purple-Green")
#'
#' # Example 4: viridis option
#' ggplane(vbl, plane = "axi", slice = 90, var = "raw_t1") +
#'   layer_numeric(var = "score", clrsp = "magma")
#'
#' # Example 5: two-color gradient
#' ggplane(vbl, plane = "axi", slice = 90, var = "raw_t1") +
#'   layer_numeric(var = "score", clrsp = c("navy", "yellow"))
#'
#' # Example 6: three-color gradient
#' ggplane(vbl, plane = "axi", slice = 90, var = "raw_t1") +
#'   layer_numeric(var = "score", clrsp = c("blue", "white", "red"))
#'
#' # Example 7: multi-color gradient
#' ggplane(vbl, plane = "axi", slice = 90, var = "raw_t1") +
#'   layer_numeric(var = "score", clrsp = c("black", "purple", "orange", "yellow"))
#'
#' @name vbl_doc_colors_numeric
NULL


#' @rdname vbl_doc_colors_numeric
#' @export
clrsp_opts <- function(){

  list(
    sequential_single_hue =
      c("Grays", "Light Grays", "Blues 2", "Blues 3", "Purples 2", "Purples 3", "Reds 2", "Reds 3", "Greens 2",
        "Greens 3", "Oslo"),

    sequential_multi_hue =
      c("Purple-Blue", "Red-Purple", "Red-Blue", "Purple-Orange", "Purple-Yellow", "Blue-Yellow",
        "Green-Yellow", "Red-Yellow", "Heat", "Heat 2", "Terrain", "Terrain 2", "Viridis", "Plasma",
        "Inferno", "Dark Mint", "Mint", "BluGrn", "Teal", "TealGrn", "Emrld", "BluYl", "ag_GrnYl", "Peach",
        "PinkYl", "Burg", "BurgYl", "RedOr", "OrYel", "Purp", "PurpOr", "Sunset", "Magenta", "SunsetDark",
        "ag_Sunset", "BrwnYl", "YlOrRd", "YlOrBr", "OrRd", "Oranges", "YlGn", "YlGnBu", "Reds", "RdPu", "PuRd",
        "Purples", "PuBuGn", "PuBu", "Greens", "BuGn", "GnBu", "BuPu", "Blues", "Lajolla", "Turku", "inferno",
        "cividis", "viridis", "magma", "plasma", "turbo"),

    diverging =
      c("Blue-Red", "Blue-Red 2", "Blue-Red 3", "Red-Green", "Purple-Green", "Purple-Brown", "Green-Brown",
        "Blue-Yellow 2", "Blue-Yellow 3", "Green-Orange", "Cyan-Magenta", "Tropic", "Broc", "Cork",
        "Vik", "Berlin", "Lisbon", "Tofino")
  )

}

#' @rdname vbl_doc_colors_numeric
#' @export
clrsp_opts_vec <- function(){

  purrr::flatten_chr(clrsp_opts()) %>%
    sort()

}




#' @title Viridis options for numeric and discrete color mapping
#' @description
#' Returns a character vector of valid input options for both `clrp` and
#' `clrsp` arguments.
#' @return Character vector.
#' @export
color_opts_viridis <- function(){

  c(
    "magma",
    "inferno",
    "plasma",
    "viridis",
    "cividis",
    "rocket",
    "mako",
    "turbo"
  )

}



#' @title Categorical fill scale for ggvibble plots
#' @description This function centralizes categorical color handling in vibbles plotting
#' \link[vbl_doc_ggvibble]{framework} and creates a discrete fill scale from a flexible
#' \link[=vbl_doc_colors_categorical]{color specification}.
#'
#' @details
#' `scale_fill_categorical()` selects colors via \code{\link{color_vector}()},
#' using the palette specified by `clrp`.
#'
#' All valid palette identifiers are listed in
#' \code{\link{clrp_opts_vec}()}, and palette families/palettes are documented in
#' \code{\link{vbl_doc_colors_categorical}}.
#'
#' @param ... Additional arguments passed to \link{scale_fill_manual()}.
#' @inheritParams vbl_doc
#'
#' @return A \code{ggplot2} discrete fill scale.
#' @export
scale_fill_categorical <- function(clrp, names, clrp_adjust = NULL, ...){

  values <- color_vector(clrp = clrp, names = names, clrp_adjust = clrp_adjust)

  ggplot2::scale_fill_manual(values = values, ...)

}

#' @title Numeric fill scale for ggvibble plots
#' @description This function centralizes numeric color handling in vibbles plotting
#' \link[vbl_doc_ggvibble]{framework} and creates a numeric fill scale from a flexible
#' \link[=vbl_doc_colors_numeric]{color specification}. Depending on length and
#' content, the input is mapped to an appropriate gradient scale.
#'
#' @param clrsp Character vector specifying the color mapping:
#' \itemize{
#'   \item Base color name:
#'   A single string matching a base R color. Produces a two-color gradient from
#'   `"white"` to that color. See Example 1.
#'
#'   \item Sequential colorspace palette:
#'   A single string matching a sequential palette in
#'   \link{scale_fill_continuous_sequential}(). See Example 2.
#'
#'   \item Diverging colorspace palette:
#'   A single string matching a diverging palette in
#'   \link{scale_fill_continuous_diverging}(). See Example 3.
#'
#'   \item Viridis option:
#'   A single string not matching the above, interpreted as a viridis option and
#'   passed to \link{scale_fill_viridis_c}(). See Example 4.
#'
#'   \item Two-color vector:
#'   Interpreted as `low` and `high` colors in \link{scale_fill_gradient}().
#'   See Example 5.
#'
#'   \item Three-color vector:
#'   Interpreted as `low`, `mid`, `high` in \link{scale_fill_gradient2}().
#'   See Example 6.
#'
#'   \item Multi-color vector (≥4 colors):
#'   Passed to \link{scale_fill_gradientn}(). See Example 7.
#' }
#'
#' @param ... Additional arguments forwarded to the underlying scale function.
#' @inheritParams vbl_doc
#'
#' @return A ggplot2 fill scale for numeric variables.
#'
#' @export
scale_fill_numeric <- function(clrsp, ...){

  stopifnot(is.character(clrsp) & length(clrsp) >= 1)

  if(length(clrsp) == 1){

    if(clrsp %in% colors()){

      ggplot2::scale_fill_gradient(low = "white", high = clrsp, ...)

    } else if(clrsp %in% rownames(hcl_palettes(type = "sequential"))){

      colorspace::scale_fill_continuous_sequential(palette = clrsp, ...)

    } else if(clrsp %in% rownames(hcl_palettes(type = "diverging"))){

      colorspace::scale_fill_continuous_diverging(palette = clrsp, ...)

    } else {

      ggplot2::scale_fill_viridis_c(option = clrsp, ...)

    }

  } else if(length(clrsp) == 2){

    ggplot2::scale_fill_gradient(low = clrsp[1], high = clrsp[2], ...)

  } else if(length(clrsp) == 3){

    ggplot2::scale_fill_gradient2(low = clrsp[1], mid = clrsp[2], high = clrsp[3], ...)

  } else if(length(clrsp) >= 4){

    ggplot2::scale_fill_gradientn(colors = clrsp, ...)

  }

}




