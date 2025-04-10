# register the S3 `igraph` class for use with S4 methods.
setOldClass("igraph")
selectMethod("show", "igraph")

#' The `hce` class
#'
#' Used internally.
#'
#' @slot voronoi     The Voronoi tessellation of the patches and resistance
#'                   surface (`RasterLayer`).
#'
#' @slot patchLinks  A `RasterLayer` whose values indicate patch ids
#'                   (positive integers) and link ids (negative integers).
#'
#' @slot linkData    A `data.frame` of link attributes.
#'
#' @author Alex Chubaty and Sam Doctolero
#' @importClassesFrom raster RasterLayer
#' @include grainscape-package.R
#' @keywords internal
setClass(
  "hce",
  slots = list(
    voronoi = "RasterLayer", patchLinks = "RasterLayer",
    linkData = "data.frame"
  )
)

#' The `mpg` class
#'
#' @slot mpg         The minimum planar graph as class `igraph`.
#'
#' @slot patchId     The input `patch` raster with patch cells assigned to
#'                   their id (`RasterLayer`).
#'
#' @slot voronoi     The Voronoi tessellation of the patches and resistance
#'                   surface (`RasterLayer`).
#'
#' @slot lcpPerimWeight  The paths of the links between patches and their
#'                       accumulated costs (`RasterLayer`).
#'
#' @slot lcpLinkId   The paths of the links between patches and their id (`RasterLayer`).
#'
#' @slot mpgPlot     A `RasterLayer` version of the `mpg`, which can be
#'                   easily plotted to visualize the MPG.
#'
#' The `mpg` slot contains useful vertex and edge attributes.
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
  slots = list(
    mpg = "igraph", patchId = "RasterLayer", voronoi = "RasterLayer",
    lcpPerimWeight = "RasterLayer", lcpLinkId = "RasterLayer",
    mpgPlot = "RasterLayer"
  )
)

#' The `goc` class
#'
#' @slot voronoi    A `RasterLayer` describing the regions of proximity in
#'                  resistance units around the focal patches or points.
#'
#' @slot summary    A summary of the the grains of connectivity generated and
#'                  their properties.
#'
#' @slot th         A list giving the GOC graph at each threshold.
#'
#' Each element of `th` contains a `goc` object giving the GOC graph as
#' class [igraph::igraph()].
#' Vertex attributes describes qualities of each polygon including the coordinates
#' of each polygon centroid, the area of these polygons, and the original patch
#' IDs in the MPG that are included in each polygon.
#' All areal measurements are given as raster cell counts.
#' A variety of edge attributes are also given in the GOC graph.
#' See [distance()] for more information.
#'
#' @author Alex Chubaty and Paul Galpern
#' @importClassesFrom raster RasterLayer
#' @importClassesFrom sp SpatialPolygons
setClass(
  "goc",
  slots = list(
    voronoi = "RasterLayer",
    summary = "data.frame", th = "list"
  )
)

#' Show a `grainscape` object
#'
#' Custom `show` method to safely print the contents of a `goc` or `grain` object.
#'
#'
#' @param object  A [`goc()`][goc-class] or
#'                [`grain()`][grain-class] object.
#'
#' @export
#' @rdname show
setMethod("show",
  signature = "goc",
  definition = function(object) {
    cat("Slot voronoi:\n")
    cat(show(object@voronoi))

    cat("\nSlot summary:\n")
    cat(show(object@summary))

    cat("\nSlot th:\n")
    cat("List of ", length(object@th), " goc elements", "\n")
  }
)

#' The `grain` class
#'
#' @slot voronoi    A `RasterLayer` describing the regions of proximity in
#'                  resistance units around the focal patches or points.
#'
#' @slot summary    A summary of the the grains of connectivity generated and
#'                  their properties.
#'
#' @slot centroids  A `SpatialPoints` object indicating the grain's polygon
#'                  centroids.
#'
#' @slot th         A list of `igraph` objects giving the graphs describing the relationship
#'                  among the polygons in that grain
#'
#' See [grain()] for more information.
#'
#' @author Alex Chubaty and Paul Galpern
#' @importClassesFrom raster RasterLayer
#' @importClassesFrom sp SpatialPoints SpatialPolygonsDataFrame
setClass(
  "grain",
  slots = list(
    voronoi = "RasterLayer",
    summary = "data.frame", centroids = "SpatialPoints",
    th = "igraph"
  )
)

#' @export
#' @rdname show
setMethod("show",
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
  }
)

#' The `corridor` class
#'
#' @slot voronoi          A `RasterLayer` representation of the boundaries
#'                        of the voronoi polygons.
#'
#' @slot linksSP          A `SpatialLinesDataFrame` representation of links
#'                        in the grains of connectivity graph.
#'
#' @slot nodesSP          A `SpatialPoints` representation of the nodes in
#'                        the grains of connectivity graph
#'
#' @slot shortestLinksSP  A `SpatialLines` representation of the links in
#'                        the shortest path between coordinates
#'
#' @slot shortestNodesSP  A `SpatialPoints` representation of the nodes in
#'                        the shortest path between coordinates
#'
#' @slot corridorLength   A `numeric` of length 1 giving the length of the
#'                        shortest path between coordinates in accumulated
#'                        resistance units.
#'
#' See [corridor()] for more information.
#'
#' @author Alex Chubaty and Paul Galpern
#' @importClassesFrom raster RasterLayer
#' @importClassesFrom sp SpatialLines SpatialLinesDataFrame SpatialPoints
#'
setClass(
  "corridor",
  slots = list(
    voronoi = "RasterLayer", linksSP = "SpatialLinesDataFrame",
    nodesSP = "SpatialPoints", shortestLinksSP = "SpatialLines",
    shortestNodesSP = "SpatialPoints", corridorLength = "numeric"
  )
)

#' @export
#' @rdname show
setMethod("show",
  signature = "corridor",
  definition = function(object) {
    cat("Slot voronoi:\n")
    cat(show(object@voronoi))

    cat("\nSlot linksSP:\n")
    cat(show(object@linksSP))

    cat("\nSlot nodesSP:\n")
    cat(show(object@nodesSP))

    cat("\nSlot shortestLinksSP:\n")
    cat(show(object@shortestLinksSP))

    cat("\nSlot shortestNodesSP:\n")
    cat(show(object@shortestNodesSP))

    cat("\nSlot corridorLength:\n")
    cat(show(object@corridorLength))
  }
)
