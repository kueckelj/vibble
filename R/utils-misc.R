
#' @title Generate slice sequences based on voxel-wise conditions
#' @description Produces a sequence of slice indices for use in \link{ggplane}()
#' and the `seq_*` family of helper functions.
#'
#' @param cond Logical expression evaluated \link[rlang:args_data_masking]{tidyverse's data-masking}
#' semantics on the \link{vibble2D}() representation (e.g., `expr = tumor`,
#' `expr = raw_t1 > 0.5`). Determines which slices are selected.
#' @param buffer Numeric. If `buffer > 0`, the slice range is expanded before
#' generating the sequence. If `buffer < 1`, it is interpreted as a proportion
#' of the number of matched slices; if `buffer >= 1`, it is interpreted as an
#' absolute number of slices.
#' @param ... Additional arguments passed to \link{seq}(), such as `length.out`.
#' @inherit vbl_doc params
#'
#' @return An integer vector of slice indices.
#'
#' @details
#' The function works in three steps:
#' \itemize{
#'   \item Converts the vibble to 2D using \link{vibble2D}().
#'   \item Identifies all slices containing voxels that satisfy `cond`.
#'   \item Optionally expands this range using `buffer`, then generates a
#'   regular sequence using \link{seq}().
#' }
#'
#' `seq_slices()` is part of the `seq_*` family, which provides convenient
#' ways to generate axis-based sequences (e.g., for slicing, animation, or
#' stepping through the volume). This function focuses specifically on
#' condition-based slice selection.
#'
#' @seealso \link{ggplane}(), \link{vibble2D}(), \link{seq}
#'
#' @examples
#' vbl <- example_vbl()
#'
#' # All slices containing tumor voxels
#' seq_slices(vbl, plane = "axi", cond = tumor)
#'
#' # Expand by 3 slices on each side
#' seq_slices(vbl, "axi", cond = tumor, buffer = 3)
#'
#' # Use a proportional buffer and step through slices by 2
#' seq_slices(vbl, "axi", cond = tumor, buffer = 0.2, by = 2)
#'
#' @export

seq_slices <- function(vbl, plane, cond, buffer = 0, ...){

  vbl2D <- vibble2D(vbl, plane = plane)

  seq_out <-
    dplyr::filter(vbl2D, !!rlang::enquo(cond)) %>%
    dplyr::pull(var = "slice") %>%
    unique() %>%
    sort()

  if(buffer > 0){

    if(buffer < 1){

      buffer <- ceiling(length(seq_out)*buffer)

    }

    front <- (min(seq_out)-buffer):(min(seq_out)-1)
    back <- (max(seq_out)+1):(max(seq_out)+buffer)

    seq_out <- c(front, seq_out, back)

    lim_slice <- var_smr(vbl2D, var = "slice")$limits
    seq_out <- seq_out[within_limits(seq_out, lim_slice)]

  }

  seq_out <- seq(from = min(seq_out), to = max(seq_out), ...)
  seq_out <- floor(seq_out)

  return(seq_out)

}
