# Predefined themes and utilities

#' @export
theme_clean <- function(){

  ggplot2::theme(
    axis.text.x = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    axis.title.x = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_blank(),
    legend.position = "none",
    plot.background = ggplot2::element_rect(fill = "white", color = "white"),
  )

}
