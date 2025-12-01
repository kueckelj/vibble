#' Internal vctrs methods
#'
#' @import vctrs
#' @keywords internal
#' @name vbl_vctrs
NULL

# var_label ---------------------------------------------------------------

#' @export
new_factor <- function(x = factor(), cpal = character(0)){

  if(!is.factor(x)){
    rlang::abort("`x` must be a factor.")
  }

  structure(
    x,
    cpal = cpal,
    class = c("vbl_fct", "factor")
  )

}


#' @export
cpal <- function(x) attr(x, "cpal")

#' @export
`cpal<-` <- function(x, value){
  attr(x, "cpal") <- value
  x
}


#' @export
type_sum.vbl_fct <- function(x) "vbl_fct"

#' @export
pillar_shaft.vbl_fct <- function(x, ...){

  # underlying factor
  f <- as.factor(x)

  # formatted text
  txt <- format(f)

  pillar::pillar_shaft(txt, ...)
}

