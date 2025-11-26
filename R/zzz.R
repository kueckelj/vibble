
.onLoad <- function(libname, pkgname){

  op_def <- vbl_opts_default

  current <- options()

  to_set <- !(names(op_def) %in% names(current))

  if(any(to_set)){
    options(op_def[to_set])
  }

}
