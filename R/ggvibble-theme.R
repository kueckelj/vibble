# Predefined themes and utilities

#' @export
theme_clean <- function(){

  ggplot2::theme(
    axis.ticks.x = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    legend.position = "none"
  )

}
