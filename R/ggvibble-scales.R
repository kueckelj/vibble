# Scales and color utilities


#' @importFrom colorspace hcl_palettes
#' @export
scale_fill_numeric <- function(clrsp, ...){

  require(colorspace)
  require(ggplot2)

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
