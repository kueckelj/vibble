# Documentation dummies

# classes and concepts ----------------------------------------------------

#' @title Vibble objects
#' @name vibble
#' @docType class
#' @description
#' A vibble is a voxel-level tidy-data structure (tibble/data.frame) that represents
#' one or more 3D volumes registered to the same space.
#'
#' \itemize{
#'  \item{Rows}{ correspond to voxels, identified by integer coordinates.}
#'  \item{Columns}{ store voxel-wise values (e.g. intensities, masks, labels, scores).}
#' }
#'
#' Every column is referred to as variable in tidy-data terms. A vibble knows four types of variables:
#'
#' \itemize{
#'  \item{Spatial:}{ Variables with *fixed naming* x, y and z give the unique position of each voxel in the 3D space.
#'  They serve as both, for spatial localization and as a unique identifier.}
#'  \item{Label:}{ Variables of class \link{factor} storing categorical classifications or ordinal scores.}
#'  \item{Mask:}{ Variables of class \link{logical} storing binary information.}
#'  \item{Numeric:}{ Variables of class \link{numeric} storing information that can be interpreted on a continuous numeric scale.}
#'  }
#'
#' Label, mask and numeric variables are referred to as \link[=vars_data]{data variables}. Spatial variables
#' x, y, and z are referred to as \link[=vars_spatial]{spatial variables}.
#'
#' For a vibble to be valid, the following requirements must hold.
#'
#' @section Required class:
#' Vibbles extent the \link[tibble:tibble]{tbl} class with \code{c("vbl", <underlying class>)}.
#'
#' @section Required columns:
#' A vibble must contain at least the following columns:
#' \itemize{
#'   \item{ \code{x}, \code{y}, \code{z}: integer voxel coordinates in a
#'   Cartesian grid. This coordinate triplet must uniquely identify each row.}
#'   \item{ One additional column with voxel values (label, mask or numeric) }.
#' }
#'
#' @section Required spatial mapping:
#' The spatial information hold by the `x`, `y` and `z` variables is fixed and
#' maps to the anatomical axes as follows:
#'
#' \itemize{
#'  \item{x:}{ Gives the integer position of a voxel along the right-left-axis. Increasing values move
#'  the voxel towards the **left**.}
#'  \item{y:}{ Gives the integer position of a voxel along the superior-inferior-axis. Increasing values move
#'  the voxel towards **inferior**.}
#'  \item{z:}{ Gives the integer position of a voxel along the anterior-posterior-axis. Increasing values move
#'  the voxel towards **posterior**.}
#'  }
#'
#' Accordingly, XYZ in a Cartesian coordinate system map to LIP in voxel orientation code.
#'
#' @section Required attributes:
#' A vibble must contain the following attributes:
#' \itemize{
#'   \item \code{ccs_mapping}: named list mapping \code{x}, \code{y}, \code{z}
#'   to anatomical axes (x = L, y = I, z = P). This is fixed and cannot be changed.
#'   \item \code{ccs_limits}: list of length three containing the coordinate
#'   ranges for \code{x}, \code{y}, \code{z} as valid \link[=is_limit]{limits}. They define
#'   the space in which the volume-data lives. Vibbles can only be \link[=join_vibbles]{joined} if their space is equal.
#'   \item \code{var_smr}: named list with per-variable summaries; each entry
#'   is created by \link{summarize_var}() upon initial creation of the variable
#'   and is used for comprehensive mapping of color schemes and statistical
#'   analysis in the absence of the whole data set.
#' }
#'
#' @section Optional attributes:
#' A vibble usually contains the following attributes:
#' \itemize{
#'   \item \code{orientation_orig}: character string describing the original
#'   image orientation (e.g. from \code{RNifti::orientation()}).
#'   \item \code{nifti}: the NIfTI object used to create the vibble without
#'   slot @.Data for facilitated backwards compatibility.
#' }
#'
#' @section Behaviour:
#' A vibble behaves in almost all regards like a tibble. There are only two aspects
#' that are hardcoded:
#' \enumerate{
#'  \item{Spatial variables x, y and z can not be renamed, removed or manipulated.}
#'  \item{Vibble attributes remain unchanged / are restored after grouped dplyr operations.}
#'  }
#'
#' @seealso
#' \link{nifti_to_vbl}() for creating a vibble from a NIfTI image,
#' \link{update_var_smr}() for refreshing \code{var_smr}.
NULL


#' @title Transparancy in ggvibble plots using alpha
#' @name ggvible_doc_alpha
#' @section Input options:
#' The `alpha` argument supports constant, ranged, and voxel-wise transparency.
#' It follows tidyverse data-masking semantics and is interpreted as follows:
#'
#' \itemize{
#'
#'   \item \strong{Single numeric value: }
#'   A constant transparency in `[0,1]` applied to all voxels.
#'   No scaling is performed. Example A.
#'
#'   \item \strong{Numeric vector of length 2: }
#'   Requires a numeric variable mapped to the coloring. Interpreted as a
#'   lower and upper transparency bound. The numeric variable being plotted
#'   is rescaled into this interval using \link{scales::rescale}(), with the
#'   input range given by `var_smr(vbl2D, var)$limits`. Example B.
#'
#'   \item \strong{Data-masked expressions: }
#'   Any expression using column names of `vbl2D` (e.g., `alpha = score`,
#'   `alpha = if_else(tumor, 0.5, 0.2)`. are evaluated with
#'   \link{eval_tidy}(), meaning variable names refer to columns rather than
#'   R objects. See \link[rlang:args_data_masking]{tidyverse's data-masking}.
#'   Example C.
#'
#'   \item \strong{Invalid input: }
#'   Non-numeric inputs or numeric vectors of unsupported length cause an error.
#'   Example D.
#'
#' }
#'
#' In all cases, the returned values are guaranteed to lie within `[0,1]`.
NULL


# params ------------------------------------------------------------------

#' @title Dummy documentation for recurring parameters
#' @name vbl_doc
#' @param alpha Controls the fill transparency. May be a single value in `[0,1]`,
#' a numeric vector of length two, or an expression evaluated with
#' \link[rlang:args_data_masking]{tidyverse's data-masking} semantics.
#' See \link[=ggvible_doc_alpha]{Details} for more information.
#' @param bb3D A named list that defines a \link[=is_bb3D]{3D bounding box} with numeric vectors for `x`, `y`, and/or `z`.
#' @param lut Either a file path to a LUT, character vector of labels or a data.frame with integer indices
#' and character labels.
#' @param offset_dir Direction in which to shift voxels when `offset_dist > 0`.
#' Valid options are *c('left', 'right', 'top', 'bottom')*. A *'-flip'* suffix as
#' in *'left-flip'* is also valid and flips the slice ordering.
#' @param offset_dist Distance by which voxels are shifted along `offset_dir` in the 2D plane.
#' Values `< 1` are interpreted as a proportion of the maximal coordinate,
#' values `>= 1` as an absolute distance in coordinate units.
#' @param plane The anatomical orientation. Valid options are *c('sag', 'axi', 'cor')*.
#' @param slices Numeric vector. The slice indices of interest.
#' @param vbl A \link{vibble}.
#' @param vbl2D A \link{vibble2D}.
#'
#' @keywords internal
NULL

#' @title Dummy documentation for labeled voxel variables
#' @name vbl_doc_var_label
#'
#' @param var
#' Character. Must refer to a factor column in the
#' \link{vibble} encoding categorical voxel classes.
#'
#' @keywords internal
NULL

#' @title Dummy documentation for logical mask variables
#' @name vbl_doc_var_mask
#'
#' @param var
#' Character. Must refer to a column in the
#' \link{vibble} containing `TRUE`/`FALSE` mask values.
#'
#' @keywords internal
NULL

#' @title Dummy documentation for numeric voxel variables
#' @name vbl_doc_var_numeric
#'
#' @param var
#' Character. Must refer to a column in the
#' \link{vibble} containing continuous or integer-valued numeric data.
#'
#' @keywords internal
NULL

#' @title Dummy documentation for vibble layers
#' @name vbl_doc_vbl_layer
#'
#' @param cond Optional logical filter expression that determines the specific
#' voxels on which this layer is drawn. This allows fine-grained, voxel-level
#' filtering beyond whole-slice selection. The expression is evaluated with
#' \link[rlang:args_data_masking]{tidyverse's data-masking} semantics
#' on the \link{vibble2D}() passed into the layer by the \link{ggplane}() call.
#' See \link[=ggvibble_doc_cond]{Details} for more information and examples.
#'
#' If `NULL`, the layer is drawn for all voxels included in the corresponding
#' \link{ggplane}() call.
#'
#' @param slices Optional numeric vector of slice indices on which this layer
#' should be drawn. This restricts the layer at the slice level, in contrast to
#' `cond`, which restricts the layer at the voxel level.
#'
#' If `NULL`, the layer is applied to all slices included in the corresponding
#' \link{ggplane}() call.
#'
#' @return A `ggvibble_layer` object containing the supplied function. When
#' added to a `ggvibble`, the function is executed and its returned layers are
#' inserted into the plot.
#'
#' @keywords internal
NULL




# sections ----------------------------------------------------------------

#' @title Dummy documentation for sections
#' @name vbl_doc_sections



