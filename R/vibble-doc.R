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
#' Every column is referred to as variable in tidy-data terms.
#'
#' @section Variables:
#' A vibble knows four types of variables:
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

# params ------------------------------------------------------------------

#' @title Dummy documentation for recurring parameters
#' @name vbl_doc
#' @param alpha Controls the fill transparency. May be a single value in `[0,1]`,
#' a numeric vector of length two, or an expression evaluated via
#' \link[rlang:args_data_masking]{data-masking} semantics with the 2D vibble
#' underlying this layer.
#'
#' See \link[=ggvible_doc_alpha]{Details} for more information.
#'
#' @param .by Optional. A \link[dplyr:dplyr_tidy_select]{tidy-selection} of columns to
#' group by before applying the filtering logic of `.cond`.
#'
#' @param bb2D A named list that defines a \link[=is_bb2D]{2D bounding box} with \link[=is_limit]{limits} for col and row.
#' @param bb3D A named list that defines a \link[=is_bb3D]{3D bounding box} with \link[=is_limit]{limits} for x, y, z.
#' @param clrp Character scalar specifying the categorical palette.
#' Must be one of \code{\link{clrp_opts_vec}()}. Defaults to \code{vbl_opts("clrp")}.
#' @param clrp_adjust Optional named vector of named hex colors used to override
#' colors of specific labels (see \link{color_vector}()).
#' @param clrsp Character scalar specifying the numeric color palette used in \link{scale_fill_numeric}()`.
#' @param interpolate Logical scalar indicating whether to interpolate raster tiles.
#' @param lut Either a file path to a LUT, character vector of labels or a data.frame with integer indices
#' and character labels.
#' @param offset_dir Direction in which to shift voxels when `offset_dist` is not 0.
#' Valid options are *c('left', 'right', 'top', 'bottom')*.
#' @param offset_dist If not 0, shifts the slices in the 2D layout.
#'   Interpreted as:
#'   * **absolute** when supplied as an integer (e.g. `2L`) or marked via `as_abs()`.
#'   * **relative** when supplied as a double (e.g. `0.1`) or marked via `as_rel()`,
#'     in which case the shift is computed as a proportion of the full axis range.
#' @param opacity Controls voxel transparency. Accepts constants, ranges, or
#' data-masked expressions. See section *Opacity options* for details.
#' @param plane The anatomical orientation. Valid options are *c('sag', 'axi', 'cor')*.
#' @param slice Integer value. The slice of interest.
#' @param slices Integer vector. The slices of interest.
#' @param vbl A \link{vibble}.
#' @param vbl2D A \link{vibble2D}.
#'
#' @section Opacity options:
#' The `opacity` parameter supports constant, ranged, and voxel-wise inputs.
#' In all cases, the used values are guaranteed to lie within `[0,1]`,
#' where `0` corresponds to full opacity (completely invisible) and `1`
#' corresponds to full opacity (no shine-through).
#'
#' It is interpreted as follows:
#'
#' \itemize{
#'
#'   \item \strong{Single numeric value: }
#'   A constant opacity in `[0,1]` applied to all voxels.
#'   No scaling is performed. Example A.
#'
#'   \item \strong{Numeric vector of length 2: }
#'   Interpreted as a lower and upper opacity bound. The numeric variable being plotted
#'   is rescaled into this interval, with the input range given by \link{var_limits}`(vbl2D, var)`.
#'   Restricted to \link{layer_numeric_var()}. Example B.
#'
#'   \item \strong{Data-masked expressions: }
#'   Any expression using column names of `vbl2D` are evaluated with
#'   \link{eval_tidy}(), meaning variable names refer to columns rather than
#'   R objects. See \link[rlang:args_data_masking]{data-masking}. Example C.
#'
#'   \item \strong{Invalid input: }
#'   Non-numeric inputs or numeric vectors of unsupported length cause an error.
#'   Example D.
#'
#' }
#'
#' @keywords internal
NULL

#' @title Dummy documentation for labeled voxel variables
#' @name vbl_doc_var_label
#'
#' @param var
#' Character. The name of a label (factor) column.
#'
#' @keywords internal
NULL

#' @title Dummy documentation for logical mask variables
#' @name vbl_doc_var_mask
#'
#' @param var
#' Character. The name of a mask (logical) column.
#'
#' @keywords internal
NULL

#' @title Dummy documentation for numeric voxel variables
#' @name vbl_doc_var_numeric
#'
#' @param var
#' Character. The name of a numeric column.
#'
#' @keywords internal
NULL

#' @title Dummy documentation for vibble layers
#' @name vbl_doc_layer
#'
#' @param .cond Optional. An additional logical filter expression that determines the specific
#' voxels for which this layer is drawn. The expression is evaluated via
#' \link[rlang:args_data_masking]{data-masking} semantics with the 2D vibble passed
#' into this layer by \link{ggplane}(). See \link[=ggvibble_doc_cond]{Details} for more information and examples.
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



