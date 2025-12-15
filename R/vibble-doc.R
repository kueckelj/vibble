# Documentation dummies

# classes and concepts ----------------------------------------------------

#' @title Vibble objects
#' @name vbl_doc_vbl
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
#'  \item{Labels:}{ Variables of class \link{factor} storing categorical classifications or ordinal scores.}
#'  \item{Mask:}{ Variables of class \link{logical} storing binary information.}
#'  \item{Numeric:}{ Variables of class \link{numeric} storing information that can be interpreted on a continuous numeric scale.}
#'  }
#'
#' Labels, mask and numeric variables are referred to as \link[=vars_data]{data variables}. Spatial variables
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
#'   \item{ One additional column with voxel values (labels, mask or numeric) }.
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
#'
#' \itemize{
#'   \item \code{ccs_limits}: A list of length three containing the valid
#'   \link[=is_limit]{limits} for \code{x}, \code{y}, and \code{z}. These define the spatial
#'   extent of the Cartesian coordinate system (the “space”) in which the
#'   volume data resides. Vibbles can only be \link[=join_vibbles]{joined} when their
#'   spatial limits are identical.
#'
#'   \item \code{ccs_steps}: A list of length three containing the physical step
#'   size (in millimeters) corresponding to an increase of 1 unit along each of
#'   the three axes.
#' }
#'
#' @section Behaviour:
#' A vibble behaves like a tibble, except for one thing - Spatial variables x, y and z can not be renamed, removed or manipulated.
NULL

#' @title 2D vibble objects
#' @name vbl_doc_vbl2D
#' @docType class
#' @description
#' A 2D vibble is a voxel-level tidy-data structure (tibble/data.frame) that
#' represents a \link[=vbl_doc_vbl]{vibble} prepared for visualization in 2D space. It is
#' oriented along one anatomical plane: Sagittal (*"sag"*), Axial (*"axi"*),
#' or Coronal (*"cor"*).
#'
#' Analogous to 3D vibbles:
#'
#' \itemize{
#'   \item{Rows (observations):}{ correspond to voxels, identified by integer coordinates.}
#'   \item{Columns (variables):}{ store voxel-wise values (e.g. intensities, masks, labels, scores).}
#' }
#'
#' @section Variables:
#' A 2D vibble defines five types of variables:
#'
#' \itemize{
#'   \item{Spatial:}{ Variables with *fixed naming* \code{col} and \code{row}
#'   specify the voxel position in the 2D layout according to a
#'   \link[vbl_doc_plot_layouts]{plotting layout}.}
#'
#'   \item{Slice:}{ A variable with *fixed naming* \code{slice} defines the slice
#'   index along the anatomical axis to which the 2D vibble corresponds.}
#'
#'   \item{Categorical:}{ Variables of class \link{factor} storing categorical or ordinal information.}
#'
#'   \item{Mask:}{ Variables of class \link{logical} storing binary voxel-wise information.}
#'
#'   \item{Numeric:}{ Variables of class \link{numeric} storing continuous-valued information.}
#' }
#'
#' @section Required class:
#' 2D vibbles extend the \link[tibble:tibble]{tbl} class with
#' \code{c("vbl2D", <underlying class>)}.
#'
#' @section Required columns:
#' A 2D vibble must contain at least the following columns:
#'
#' \itemize{
#'   \item{\code{col}, \code{row}:}{ Integer coordinates in a 2D grid.}
#'   \item{\code{slice}:}{ An integer identifying the anatomical slice.}
#'   \item{One additional column storing voxel values (label, mask, or numeric).}
#' }
#'
#' @section Required attributes:
#' A 2D vibble must contain the following attributes:
#'
#' \itemize{
#'
#'   \item \code{data_bb}: Global data bounding box defined by the minimum and
#'   maximum of \code{col} and \code{row} across all slices, in native
#'   (pre-offset) coordinates. Represents the full spatial extent of the data
#'   and serves as the default reference window when no explicit screen window
#'   is specified.
#'
#'   \item \code{offset_col, offset_row}: Integer offsets applied to a slice based
#'   on its \link[=slice_offset_index]{offset index} \code{[0,n]}. For a voxel in
#'   slice \code{n}, the 2D position is computed as
#'   \code{col + offset_col * n} and \code{row + offset_row * n}.
#'
#'   \item \code{plane}: A character value indicating the anatomical plane
#'   represented by the 2D vibble.
#'
#'   \item \code{screen_bb}: Common screen bounding box applied uniformly to all
#'   slices in native (pre-offset) coordinates. Defines the shared viewing window
#'   used for cropping data and anchoring annotations consistently across slices.
#'   Defaults to \code{data_bb} if not explicitly specified.
#'
#' }
#'
#' @section Behaviour: A 2D vibble behaves like a tibble.
NULL


# variable classes ----------------------------------------------------------

#' @title Variables in the vibble package
#' @name vbl_doc_vars
#' @keywords internal
#' @description
#' Vibble variables are organised into three major groups based on their role
#' within a voxel-level tidy-data structure: Spatial 3D variables, Spatial 2D
#' variables, and Data variables. Each group corresponds to a distinct semantic
#' layer in the vibble framework and is implemented through dedicated vctrs
#' classes. The sections below outline the conceptual purpose of each group and
#' link to the respective detailed documentation.
#'
#' @section Spatial 3D:
#' Spatial 3D variables represent immutable voxel coordinates in the original
#' 3D Cartesian index space of the volume. They define voxel identity and ensure
#' spatial alignment across all operations.
#' See \link{vbl_doc_var_spatial3D} for detailed specification of the
#' \code{vbl_spat3D} class.
#'
#' @section Spatial 2D:
#' Spatial 2D variables represent slice-level or layout-level coordinates used
#' when projecting 3D data onto 2D planes. They include screen-space positions
#' for visualisation as well as slice indices that reference anatomical planes.
#' See \link{vbl_doc_var_spatial2D} for details on \code{vbl_screen} and
#' \code{vbl_slice}.
#'
#' @section Data variables:
#' Data variables encode voxel-wise measurements such as categorical labels,
#' binary masks, and continuous numeric values. They use specialised vctrs
#' classes to retain plotting metadata (colour palettes, limits) while
#' preserving natural R behaviour for factors, logicals, and numerics.
#' See \link{vbl_doc_var_data} for detailed information on
#' \code{vbl_labels}, \code{vbl_mask}, and \code{vbl_num}.
#'
NULL

#' @title Variables for 3D spatial representation
#' @name vbl_doc_var_spatial3D
#' @keywords internal
#' @description
#' Spatial 3D variables represent immutable voxel coordinates in a 3D Cartesian
#' grid and form the structural backbone of every vibble. They uniquely identify
#' each voxel and ensure that all operations preserve anatomical alignment and
#' positional consistency across volumes.
#'
#' @section Concept:
#' Spatial 3D variables encode the fixed index-space coordinates of the volume.
#' They correspond to the mandatory vibble columns \code{x}, \code{y}, and
#' \code{z} and are implemented through the \code{vbl_spat3D} vctrs class.
#' Their values must remain unchanged throughout all data transformations.
#'
#' @section Backing type:
#' \itemize{
#'   \item Integer vector.
#' }
#'
#' @section Behaviour:
#' \itemize{
#'   \item Immutable: arithmetic operations are disallowed.
#'   \item Only subsetting and comparisons are permitted.
#'   \item In vibbles, the \code{x}, \code{y}, and \code{z} columns cannot be
#'   renamed, removed, or overwritten.
#' }
#'
#' @section Required attributes:
#' \itemize{
#'   \item \code{axis}: One of \code{"x"}, \code{"y"}, or \code{"z"} identifying
#'   the axis represented by the variable.
#'   \item \code{limits}: Integer vector \code{c(1L, dim_axis)} specifying the
#'   full coordinate range for this axis.
#'   \item \code{pointer}: Anatomical direction towards which the index increases,
#'   such as \code{"L"} (left), \code{"I"} (inferior), or \code{"P"} (posterior).
#'   \item \code{step_si}: Physical spacing (in millimetres) per index step,
#'   derived from NIfTI header information.
#' }
#'
#' @section Derived coordinate space:
#' The physical (“world-space”) coordinate for a Spatial 3D variable can be
#' obtained via \link{in_mm}(), which returns values in millimetres as a
#' standard double vector. These derived vectors may be used freely for
#' arithmetic and visualisation without affecting the immutable index-space
#' coordinates.
#'
#' @seealso
#' \link{vbl_doc_vars} for an overview of all vibble variable types.
#'
NULL

#' @title Variables for 2D spatial representation
#' @name vbl_doc_var_spatial2D
#' @keywords internal
#' @description
#' Spatial 2D variables represent slice-level and layout-level coordinates used
#' when projecting 3D voxel data onto 2D planes. They provide the positional
#' framework for arranging voxels in panel displays and for identifying the
#' anatomical slice and plane from which a 2D vibble is derived.
#'
#' @section Concept:
#' Spatial 2D variables come in two forms:
#'
#' \itemize{
#'   \item \strong{\code{vbl_screen}}: 2D screen or panel coordinates used to
#'   arrange voxels within a single slice image (e.g. \code{col} and \code{row}).
#'
#'   \item \strong{\code{vbl_slice}}: Discrete slice indices specifying the
#'   anatomical plane from which the 2D representation originates (e.g.
#'   sagittal, axial, coronal).
#' }
#'
#' Both types carry a \code{plane} attribute that encodes the anatomical plane
#' (\code{"sag"}, \code{"axi"}, or \code{"cor"}) of the 2D representation.
#'
#' @section vbl_screen:
#'
#' \subsection{Concept}{
#' 2D screen or panel coordinates used to arrange voxels within a slice-level
#' visualisation. They provide an integer grid layout (e.g. \code{col} and
#' \code{row}) for rasterised 2D representations.
#' }
#'
#' \subsection{Backing type}{
#' Integer vector.
#' }
#'
#' \subsection{Behaviour}{
#' \itemize{
#'   \item Supports arithmetic operations (\code{+}, \code{-}, \code{*}, \code{/})
#'   intended for layout construction (offsets, simple scaling, flipping).
#'
#'   \item Results of any arithmetic operation are automatically truncated
#'   to integer values to ensure that screen coordinates always remain valid
#'   grid indices for raster display.
#'
#'   \item Operations that produce non-finite values (\code{NA}, \code{Inf},
#'   \code{NaN}) are not allowed.
#'
#'   \item Represents 2D arrangement only; anatomical meaning is derived via
#'   the \code{plane} and \code{pointer} attributes inherited from the
#'   underlying Spatial 3D axes.
#'
#'   \item Supports subsetting and comparisons.
#' }
#' }
#'
#' \subsection{Required attributes}{
#' \itemize{
#'   \item \code{axis2D}: \code{"col"} or \code{"row"}.
#'
#'   \item \code{plane}: Anatomical plane labels, one of \code{"sag"},
#'   \code{"axi"}, or \code{"cor"}, indicating which 3D plane this 2D view
#'   represents.
#'
#'   \item \code{limits}: Integer vector giving the nominal grid range for the
#'   2D layout; may change when layout recalculations occur.
#'
#'   \item \code{pointer}: Anatomical direction towards which the index
#'   increases within the 2D plane (inherited from the corresponding
#'   \code{vbl_spat3D} axis).
#'
#'   \item \code{step_si}: Physical spacing in millimetres per index step
#'   along this 2D axis, inherited from the relevant \code{vbl_spat3D} variable.
#' }
#' }
#'
#' @section vbl_slice:
#'
#' \subsection{Concept}{
#' Discrete slice indices identifying the anatomical slice from which a 2D
#' representation is derived. A \code{vbl_slice} variable specifies the position
#' of each voxel within an ordered stack of slices orthogonal to a 3D axis.
#' }
#'
#' \subsection{Backing type}{
#' Integer vector.
#' }
#'
#' \subsection{Behaviour}{
#' \itemize{
#'   \item Arithmetic operations are not supported; slice indices define fixed
#'   anatomical positions and must not be shifted or rescaled.
#'
#'   \item Valid operations include filtering, grouping, and comparisons.
#'
#'   \item Slice indices express ordinal progression along the axis orthogonal
#'   to the 2D plane and thus retain anatomical meaning (e.g. left→right for
#'   sagittal slices, superior→inferior for axial slices).
#'
#'   \item \code{vbl_slice} does not encode physical spacing; spacing in
#'   millimetres is defined by the corresponding \code{vbl_spat3D} axis and is
#'   not stored here.
#' }
#' }
#'
#' \subsection{Required attributes}{
#' \itemize{
#'   \item \code{plane}: Anatomical plane labels, one of \code{"sag"},
#'   \code{"axi"}, or \code{"cor"}, indicating the orientation of the slice
#'   stack.
#'
#'   \item \code{limits}: Integer vector \code{c(min_slice, max_slice)}
#'   representing the full slice index range along the axis orthogonal to the
#'   displayed plane.
#'
#'   \item \code{pointer}: Anatomical direction towards which the slice index
#'   increases (e.g. moving posteriorly in an axial stack).
#' }
#' }
NULL

#' @title Variables for voxel-wise measurements
#' @name vbl_doc_var_data
#' @keywords internal
#' @description
#' Data variables encode voxel-wise measurements in a vibble. They include
#' categorical labels, binary masks, and continuous numeric values.
#'
#' @section Concept:
#' Data variables fall into three semantic groups:
#'
#' \itemize{
#'   \item Categorical voxel annotations backed by factors.
#'   \item Binary logical variables indicating voxel membership in a region or set.
#'   \item Continuous numeric measurements defined per voxel, such as intensities or statistical values.
#' }
#'
NULL



# concepts ----------------------------------------------------------------


#' @title Plotting voxel data with ggvibble
#' @name vbl_doc_ggvibble
#' @description
#' ggvibble provides a user-oriented interface for visualizing voxel-based medical
#' imaging data stored as tidy data. It allows users to create 2D slice-based
#' plots that behave like ggplot2 objects, while handling voxel-specific details
#' such as slicing, spatial layout, layer ordering, and multiple color scales
#' automatically.
#'
#' @details
#' From a user perspective, ggvibble works similarly to ggplot2:
#'
#' \itemize{
#'   \item A plot is initialized from a \link{vibble} using \link{ggplane}().
#'   \item Additional visual elements are added using the \code{+} operator.
#'   \item The plot is rendered automatically when printed.
#' }
#'
#' Internally, ggvibble separates plotting into three steps, but users usually do
#' not need to be aware of this:
#'
#' \itemize{
#'   \item{Data preparation}: {Voxel data are converted to a 2D representation
#'   (\code{vbl2D}) by selecting an anatomical plane and one or more slices.}
#'
#'   \item{Plot specification}: {A \code{ggvibble} object stores the prepared data,
#'   base plotting options, and a list of layers added by the user.}
#'
#'   \item{Rendering}: {The final ggplot object is built lazily when the plot is
#'   printed, ensuring consistent layer ordering and correct legend handling.}
#' }
#'
#' Users can think of ggvibble layers as semantic plotting actions such as
#' "show voxel intensities", "overlay a mask", or "draw region outlines".
#' The internal ordering of layers (e.g. raster first, annotations last) is
#' handled automatically and does not depend on the order in which layers are
#' added.
#'
#' @section Basic usage:
#' The plot is initialized with \link{ggplane}() for a set of slices. While the variable
#' used for the coloring should be an intensity like T1, FLAIR or T2, data-driven variables
#' like masks or categorical labels can also be used.
#' \preformatted{
#' vbl <- example_vbl()
#'
#' # start with an intensity
#' p <-
#'  ggplane(
#'    vbl = vbl,
#'    var = "t1",
#'    plane = "axi",
#'    slices = c(90, 100, 110),
#'    clrsp = c("black", "white")
#'  )
#'
#'  p
#'
#'  # start with categorical label data
#'  p_cat <-
#'    ggplane(
#'     vbl = vbl,
#'     var = "macro",
#'     plane = "axi",
#'     slices = c(90, 100, 110),
#'     clrp = "default"
#'     )
#'
#'  p_cat
#'
#' }
#'
#' @section Adding layers:
#' Similar to ggplot2, ggvibbles are rendered during printing. So `p` itself is
#' a lightweight container with data and instructions that can be complemented
#' with additional layers that built on the underlying data.
#'
#' \preformatted{
#' p_with_layers <- p +
#'   layer_mask(.cond = tumor, color = "red", opacity = 0.6) +
#'   layer_outline(.cond = tumor, color = "red") +
#'   layer_labels(var = "macro")
#'
#' p_with_layers
#'
#' }
#'
#' @section ggplot2 compatibility:
#' A ggvibble plot can be treated like a ggplot object **after explicit building** with
#' \link{as_ggplot}(). Standard ggplot2 themes and modifications can be added directly:
#'
#' \preformatted{
#'
#'  ggp <- as_ggplot(p)
#'
#'  ggp + ggplot2::theme_void()
#'
#' }
#'
#' @note
#' ggvibble is designed to remove common sources of friction when plotting voxel
#' data (slice selection, raster alignment, scale conflicts), while remaining
#' familiar to users who already know ggplot2.
#'
#' @seealso
#' \link{ggplane}(),
#' \link{layer_mask}(),
#' \link{layer_outline}(),
#' \link{layer_labels}(),
#' \link{as_ggplot}()
NULL



#' @title Image anchors for positioning elements in ggvibble plots
#' @name vbl_doc_img_anchors
#' @description
#' Defines a set of reference anchors used to position text or graphical
#' elements relative to 2D bounding boxes in \link[=vbl_doc_ggvibble]{ggvibble plots}.
#'
#' @details
#' Image anchors specify where, within a bounding box, an element
#' should be placed. Anchors can be supplied as named character identifiers or as numeric
#' relative coordinates of length two, each value in `[0,1]`.
#'
#' Character anchors map to predefined relative positions:
#' \itemize{
#'   \item `"left"`   -> (0, 0.5)
#'   \item `"right"`  -> (1, 0.5)
#'   \item `"top"`    -> (0.5, 1)
#'   \item `"bottom"` -> (0.5, 0)
#' }
#'
#' A numeric anchor `(u, v)` is interpreted relative to the bounding box of the
#' current slice:
#' \preformatted{
#'   col = cmin + u * (cmax - cmin)
#'   row = rmin + v * (rmax - rmin)
#' }
#'
#' These anchors are used by utilities such as `as_img_anchor_abs()`,
#' `layer_slice_numbers()`, and other layout components that require consistent
#' positioning within slice extents or screen limits.
#'
#' @return
#' A named list of predefined anchors (character → numeric mapping), or a numeric
#' vector of length two when user-supplied anchors are validated.
#'
#' @seealso \link[=vbl_doc_ref_bb]{2D reference bounding boxes}.
#'
#' @keywords internal
NULL


#' @title 2D reference bounding boxes
#' @name vbl_doc_ref_bb
#' @description
#' This documentation defines the different \link[=is_bb2D]{bounding boxes} used in
#' ggvibble plotting workflows. 2D bounding boxes are used to crop data, to define the
#' visible plotting area, and to anchor annotations consistently across slices,
#' including offset layouts.
#'
#' @inheritParams vbl_doc
#' @param slice Integer slice value.
#'
#' @details
#' Each function returns \link[=is_bb2D]{2D bounding box} objects that correspond to specific extents:
#'
#' \itemize{
#'
#'   \item{`slice_bb()`: }{ Per-slice extent in native coordinates.
#'   Defined by `range(col)` and `range(row)` computed within each slice. Useful
#'   for slice-local annotations (e.g. label placement at slice centroid) and
#'   for diagnostics of slice coverage.}
#'
#'   \item{`data_bb()`: }{ Global extent of the voxel data across all selected slices in
#'   native coordinates. Defined by `range(col)` and `range(row)` computed over
#'   all slices in the 2D vibble. Useful as a stable, data-driven
#'   reference window when no explicit limits are provided.
#'
#'   In offset-layouts: `slice` can be used to get the data bounding box of a specific
#'   slice post-offset. If `NULL`, the first slice is used.
#'   }
#'
#'   \item{`screen_bb()`: }{ A single common bounding box applied to all slices in
#'   native coordinates. This is the user-imposed viewing window, typically derived
#'   from `crop` plus `expand` in \link{vibble2D}(). Useful to anchor annotations at consistent screen
#'   positions across slices (e.g. always place a legend label at the top-left of the same window).
#'
#'   In offset-layouts: `slice` can be used to get the screen bounding box of a specific
#'   slice post-offset. If `NULL`, the first slice is used.
#'   }
#'
#'   \item{`plot_bb()`: }{ The final bounding box in plotting (post-offset)
#'   coordinates. Defined as the union of all translated `screen_bb()` after
#'   applying per-slice offsets. Useful to compute global annotation positions
#'   for the complete composite layout and to ensure all offset slices remain
#'   within the visible plotting area.}
#' }
#'
#' In no-offset layouts, `plot_bb()` and `screen_bb()` are identical because
#' all slices share the same coordinate frame.
#'
NULL


#' @title Opacity options
#' @name vbl_doc_opacity
#' @description The `opacity` parameter supports constant, ranged, and voxel-wise inputs.
#' In all cases, the used values are guaranteed to lie within `[0,1]`,
#' where `0` corresponds to zero opacity (completely invisible) and `1`
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
NULL

#' @title Plotting layouts
#' @name vbl_doc_plot_layouts
#' @description
#' This documentation defines the two layout modes used to display multiple
#' slices in `ggplane()`: no-offset layouts and offset layouts.
#'
#' @details
#' \itemize{
#'   \item{No-offset layout: }{ Used when `offset_col = 0` and `offset_row = 0`.
#'   Each slice is displayed in its own panel using `facet_wrap()`. Slices do not
#'   share a coordinate system, so slices cannot overlap visually.}
#'
#'   \item{Offset layout: }{ Used when either `offset_col` or `offset_row` is
#'   non-zero. All slices are translated into a shared plotting space by applying
#'   slice-wise offsets, so multiple slices can be displayed together without
#'   faceting. Slices may overlap; in overlaps, the stacking order is controlled
#'   by `zstack` (ascending or descending slice order). Global plot bounds are
#'   derived from the union of translated `screen_bb` windows (`plot_bb`). This
#'   layout enables annotations anchored to the full composite layout (e.g.
#'   top-left of `plot_bb`) and consistent screen-relative placement across the
#'   entire offset arrangement.}
#' }
#'
#' See `vbl_doc_limits_2D` for definitions of `data_bb`, `screen_bb`, and
#' `plot_bb` as they relate to layout and annotation anchoring.
NULL


# params ------------------------------------------------------------------

#' @title Dummy documentation for recurring parameters & sections
#' @name vbl_doc
#'
#' @param .by A \link[dplyr:dplyr_tidy_select]{tidy-selection} of columns to
#' group by before applying the filtering logic of `.cond`.
#'
#' @param bb2D A named list that defines a \link[=is_bb2D]{2D bounding box} with \link[=is_limit]{limits} for col and row.
#' @param bb3D A named list that defines a \link[=is_bb3D]{3D bounding box} with \link[=is_limit]{limits} for x, y, z.
#' @param clrp Character scalar specifying the categorical palette.
#' Must be one of \code{\link{clrp_opts_vec}()}. Defaults to \code{vbl_opts("clrp")}.
#' @param clrp_adjust Optional named vector of named hex colors used to override
#' colors of specific labels (see \link{color_vector}()).
#' @param clrsp Character scalar specifying the numeric color palette used in \link{scale_fill_numeric}().
#' @param concavity Numeric. A relative measure of concavity. Passed to \link[concaveman:concaveman]{concaveman()}.
#' 1 results in a relatively detailed shape, Infinity results in a convex hull. You can use values lower than 1,
#' but they can produce pretty crazy shapes.
#' @param interpolate Logical scalar indicating whether to interpolate raster tiles.
#' @param lut Either a file path to a LUT, character vector of labels or a data.frame with integer indices
#' and character labels.
#' @param offset_col,offset_row Numeric scalars. Offsets applied per slice along
#' the `col` and `row` axes, which can be specified as either **absolute** or **relative** values:
#'
#' \itemize{
#'   \item{Absolute:}{ Integers, or numeric values wrapped in `as_abs()`, are
#'   interpreted as absolute offsets in data coordinates.}
#'   \item{Relative:}{ Numeric values, or values wrapped in `as_rel()`, are
#'   interpreted as offsets relative to the current plotting limits.}
#' }
#'
#' If both, `offset_col` and `offset_row` are zero, each slice is displayed in its
#' own panel using `facet_wrap()`, arranged according to `layout`.
#'
#' @param opacity Controls voxel transparency. Accepts constants, ranges, or
#' data-masked expressions. See \link[=vbl_doc_opacity]{Details}.
#' @param plane Character scalar. The anatomical orientation. Valid options are *c('sag', 'axi', 'cor')*.
#' @param rm0 Logical. If \code{TRUE}, remove voxels with value 0 from the resulting
#' vibble.
#' @param slice Integer value. The slice of interest.
#' @param slices Integer vector. The slices of interest.
#' @param vbl A \link[=vbl_doc_vbl]{vibble}.
#' @param vbl2D A \link[=vbl_doc_vbl2D]{2D vibble}.
#' @param verbose Logical. If `TRUE`, informative messages are printed in the
#' console.
#' @param x An object for which a method has been defined.
#' @param zstack Character scalar. Controls the stacking order of slices along
#' the slice axis when multiple slices are displayed with an offset-layout.
#'
#' \itemize{
#'   \item{`"asc"`: }{ Slices with lower slice indices are drawn first and may be
#'   occluded by higher-index slices in overlapping offset layouts.}
#'   \item{`"desc"`: }{ Slices with higher slice indices are drawn first and may be
#'   occluded by lower-index slices in overlapping offset layouts.}
#' }
#'
#' @keywords internal
NULL

#' @title Dummy documentation for categorical voxel variables
#' @name vbl_doc_var_categorical
#'
#' @param var Character. The name of a labels (factor) column.
#'
#' @keywords internal
NULL

#' @title Dummy documentation for logical mask variables
#' @name vbl_doc_var_mask
#'
#' @param var Character. The name of a mask (logical) column.
#'
#' @keywords internal
NULL

#' @title Dummy documentation for numeric voxel variables
#' @name vbl_doc_var_numeric
#'
#' @param var Character. The name of a numeric column.
#'
#' @keywords internal
NULL

#' @title Dummy documentation for vibble layers
#' @name vbl_doc_layer
#'
#' @param .cond A logical filter expression that determines the specific
#' voxels for which this layer is drawn. The expression is evaluated via
#' \link[rlang:args_data_masking]{data-masking} semantics with the 2D vibble passed
#' into this layer by \code{ggplane}(). See \link[=vbl_doc_cond]{Details}.
#' @param clip_overlap Logical. Applies only to offset layouts. If `TRUE`,
#' content from each slice is clipped so that it cannot extend into adjacent
#' slice regions.
#' @param linetype Defines the line pattern. Supported values:
#' \itemize{
#'   \item {Named types:}{ c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash") }
#'   \item {Numeric codes:}{ c("0", "1", "2", "3", "4", "5", "6") }
#'   \item {Custom:}{ c("12", "1F", "F0F0") as examples of hexadecimal dash patterns}
#' }
#' @param linewidth Numeric scalar. Controlls the thickness of drawn lines.
#' @param name Logical or character scalar. If character, the name with which
#' the color is associated with in the color legend. If logical, `FALSE` prevents
#' appearance in the legend and `TRUE` falls back to the naming default of the
#' function.
#' @param slices Optional numeric vector of slice indices to which the content
#' of this layer is restricted. If `NULL`, the content is drawn for all slices.
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



