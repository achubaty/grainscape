# register the S3 `igraph` class for use with S4 methods.
setOldClass("igraph")
selectMethod("show", "igraph")

#' The \code{hce} class
#'
#' Used internally.
#'
#' @slot voronoi     The Voronoi tessellation of the patches and resistance
#'                   surface (\code{RasterLayer}).
#'
#' @slot patchLinks  A \code{RasterLayer} whose values indicate patch ids
#'                   (positive integers) and link ids (negative integers).
#'
#' @slot linkData    A \code{data.frame} of link attributes.
#'
#' @author Alex Chubaty and Sam Doctolero
#' @importClassesFrom raster RasterLayer
#' @include grainscape-package.R
#' @keywords internal
setClass(
  "hce",
  slots = list(voronoi = "RasterLayer", patchLinks = "RasterLayer",
               linkData = "data.frame")
)

#' The \code{mpg} class
#'
#' @slot mpg         The minimum planar graph as class \code{igraph}.
#'
#' @slot patchId     The input \code{patch} raster with patch cells assigned to
#'                   their id (\code{RasterLayer}).
#'
#' @slot voronoi     The Voronoi tessellation of the patches and resistance
#'                   surface (\code{RasterLayer}).
#'
#' @slot lcpPerimWeight  The paths of the links between patches and their
#'                       accumulated costs (\code{RasterLayer}).
#'
#' @slot lcpLinkId   The paths of the links between patches and their id (\code{RasterLayer}).
#'
#' @slot mpgPlot     A \code{RasterLayer} version of the \code{mpg}, which can be
#'                   easily plotted to visualize the MPG.
#'
#' The \code{mpg} slot contains useful vertex and edge attributes.
#' Vertex attributes give attributes of patches including patch area,
#' the area of patch edges, the core area of each patch, and the coordinates
#' of the patch centroid.
#' All areal measurements are given as raster cell counts.
#' Edge attributes give attributes of the graph links including link
#' weights giving accumulated resistance/least-cost path distance,
#' Euclidean distance, and the start and end coordinates of each link.
#'
#' @author Alex Chubaty and Paul Galpern
#' @importClassesFrom raster RasterLayer
#' @include grainscape-package.R
#'
setClass(
  "mpg",
  slots = list(mpg = "igraph", patchId = "RasterLayer", voronoi = "RasterLayer",
               lcpPerimWeight = "RasterLayer", lcpLinkId = "RasterLayer",
               mpgPlot = "RasterLayer")
)



#' The \code{goc} class
#'
#'
#' @slot voronoi    A \code{RasterLayer} describing the regions of proximity in
#'                  resistance units around the focal patches or points.
#'
#'
#' @slot summary    A summary of the the grains of connectivity generated and
#'                  their properties.
#'
#' @slot th         A list giving the GOC graph at each threshold.
#'
#' Each element of \code{th} contains a \code{goc} object giving the GOC graph
#' as class \code{\link{igraph}}.
#' Vertex attributes describes qualities of each polygon including the coordinates
#' of each polygon centroid, the area of these polygons, and the original patch
#' IDs in the MPG that are included in each polygon.
#' All areal measurements are given as raster cell counts.
#' A variety of edge attributes are also given in the GOC graph.
#' See \code{\link{distance}} for more information.
#'
#' @author Alex Chubaty and Paul Galpern
#' @importClassesFrom raster RasterLayer
#' @importClassesFrom sp SpatialPolygons
setClass(
  "goc",
  slots = list(voronoi = "RasterLayer",
               summary = "data.frame", th = "list")
)

#' The \code{grain} class
#'
#'
#' @slot voronoi    A \code{RasterLayer} describing the regions of proximity in
#'                  resistance units around the focal patches or points.
#'
#' @slot summary    A summary of the the grains of connectivity generated and
#'                  their properties.
#'
#' @slot centroids  A \code{SpatialPoints} object indicating the grain's polygon
#'                  centroids.
#'
#' @slot th         A list of \code{igraph} objects giving the graphs describing the relationship
#'                  among the polygons in that grain
#'
#' See \code{\link{grain}} for more information.
#'
#' @author Alex Chubaty and Paul Galpern
#' @importClassesFrom raster RasterLayer
#' @importClassesFrom sp SpatialPoints SpatialPolygonsDataFrame
setClass(
  "grain",
  slots = list(voronoi = "RasterLayer",
               summary = "data.frame", centroids = "SpatialPoints",
               th = "igraph")
)

#' Show a \code{grainscape} object
#'
#' Custom \code{show} method to safely print the contents of a \code{goc} or
#' \code{grain} object.
#'
#'
#' @param object  A \code{\link[=goc-class]{goc}} or
#'                \code{\link[=grain-class]{grain}} object.
#'
#' @export
#' @rdname show
setMethod(
  "show",
  signature = "goc",
  definition = function(object) {
    cat("Slot voronoi:\n")
    cat(show(object@voronoi))

    cat("\nSlot summary:\n")
    cat(show(object@summary))

    cat("\nSlot th:\n")
    cat("List of ", length(object@th), " goc elements", "\n")
})

#' @export
#' @rdname show
setMethod(
  "show",
  signature = "grain",
  definition = function(object) {
    cat("Slot voronoi:\n")
    cat(show(object@voronoi))

    cat("\nSlot summary:\n")
    cat(show(object@summary))

    cat("\nSlot centroids:\n")
    cat("SpatialPoints objects with ", length(object@centroids), " features", "\n")

    cat("\nSlot th:\n")
    cat(show(object@th))
})
