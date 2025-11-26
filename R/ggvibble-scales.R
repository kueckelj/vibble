#' @title Numeric fill scale for ggvibble plots
#' @description Create a numeric fill scale from a flexible color specification
#' (`clrsp`). Depending on length and content, the input is mapped to an
#' appropriate gradient scale.
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
#'
#' @return A ggplot2 fill scale for numeric variables.
#'
#' @details
#' `scale_fill_numeric()` centralizes numeric color handling for the
#' `ggplane()` framework.
#'
#' @seealso \link{layer_numeric}(), \link{ggplane}()
#'
#' @examples
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
