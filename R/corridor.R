#' Visualize corridors between two points using a grains of connectivity (GOC)
#'
#' @description
#' Given a series of GOC models built at different scales, visualize the corridor
#' (or shortest path) between two points using one of the tessellations
#' (i.e., scales) in these models.
#'
#' @note This is an experimental function.
#'
#' @param x       A \code{goc} object created by \code{\link{GOC}}.
#'
#' @param whichThresh  Integer giving the index of the threshold to visualize.
#'
#' @param coords  A two column matrix or a \code{\link{SpatialPoints}} object
#'                giving coordinates at the end points of the corridor.
#'
#' @param weight  The GOC graph link weight to use in calculating the distance.
#'                Please see details in \code{\link{distance}}.
#'
#' @param ...     Additional arguments (not used).
#'
#' @return An object of class \code{\linkS4class{corridor}}.
#'
#' @references
#' Fall, A., M.-J. Fortin, M. Manseau, D. O'Brien. (2007) Spatial graphs:
#' Principles and applications for habitat connectivity. Ecosystems 10:448:461.
#'
#' Galpern, P., M. Manseau. (2013a) Finding the functional grain: comparing methods
#' for scaling resistance surfaces. Landscape Ecology 28:1269-1291.
#'
#' Galpern, P., M. Manseau. (2013b) Modelling the influence of landscape connectivity
#' on animal distribution: a functional grain approach. Ecography 36:1004-1016.
#'
#' Galpern, P., M. Manseau, A. Fall. (2011) Patch-based graphs of landscape connectivity:
#' a guide to construction, analysis, and application for conservation.
#' Biological Conservation 144:44-55.
#'
#' Galpern, P., M. Manseau, P.J. Wilson. (2012) Grains of connectivity: analysis
#' at multiple spatial scales in landscape genetics. Molecular Ecology 21:3996-4009.
#'
#' @author Paul Galpern and Alex Chubaty
#' @docType methods
#' @export
#' @importFrom graphics plot
#' @importFrom sp Line Lines SpatialLines SpatialLinesDataFrame SpatialPoints
#' @include classes.R grain.R
#' @rdname corridor
#' @seealso \code{\link{GOC}}, \code{\link{visualize}}
#'
#' @examples
#' \dontrun{
#' library(raster)
#'
#' ## Load raster landscape
#' tiny <- raster(system.file("extdata/tiny.asc", package = "grainscape"))
#'
#' ## Create a resistance surface from a raster using an is-becomes reclassification
#' tinyCost <- reclassify(tiny, rcl = cbind(c(1, 2, 3, 4), c(1, 5, 10, 12)))
#'
#' ## Produce a patch-based MPG where patches are resistance features=1
#' tinyPatchMPG <- MPG(cost = tinyCost, patch = (tinyCost == 1))
#'
#' ## Extract a representative subset of 5 grains of connectivity
#' tinyPatchGOC <- GOC(tinyPatchMPG, nThresh = 5)
#'
#' ## Quick visualization of a corridor
#' corridorStartEnd <- rbind(c(10,10), c(90,90))
#' tinyPatchCorridor <- corridor(tinyPatchGOC, whichThresh = 3, coords = corridorStartEnd)
#' plot(tinyPatchCorridor)
#'
#' ## More control over a corridor visualization
#' plot(tinyPatchCorridor@voronoi, col = "lightgrey", lwd = 2)
#' plot(tinyPatchCorridor@linksSP, col = "darkred", lty = "dashed", add = TRUE)
#' plot(tinyPatchCorridor@nodesSP, col = "darkred", pch = 21, bg="white", add = TRUE)
#' plot(tinyPatchCorridor@shortestLinksSP, col = "darkred", lty = "solid", lwd = 2, add = TRUE)
#' plot(tinyPatchCorridor@shortestNodesSP, col = "darkred", pch = 21, bg = "darkred", add = TRUE)
#' mtext(paste("Corridor shortest path length:",
#'             round(tinyPatchCorridor@corridorLength, 2),
#'             "resistance units"), side = 1)
#' }
#'
setGeneric("corridor", function(x, ...) {
    standardGeneric("corridor")
})

#' @export
#' @rdname corridor
setMethod(
  "corridor",
  signature = "goc",
  definition = function(x, whichThresh, coords, weight = "meanWeight", ...) {
    dots <- list(...)
    if (!is.null(dots$doPlot)) {
      warning("Argument 'doPlot' is deprecated and will be ignored.")
    }

    ## Check whichThresh
    if ((length(whichThresh) > 1) || (!(whichThresh %in% 1:length(x@th)))) { # nolint
      stop("whichThresh must index a single threshold existing in the GOC object")
    }

    ## Check coords
    if (class(coords) %in% c("SpatialPoints", "SpatialPointsDataFrame")) {
      coords <- coordinates(coords)
    }

    if (ncol(coords) != 2) {
      stop("coords must be a SpatialPoints object or a matrix of two columns",
           "giving X and Y coordinates")
    }

    if (nrow(coords) > 2) {
      warning("using only first two sets of coordinates for corridor start and end points")
      coords <- coords[1:2, ]
    }

    ## Check weight
    if (!(weight %in% names(edge_attr(x@th[[1]]$goc)))) {
      stop("link weight attribute with this name doesn't exist in GOC object")
    }

    ## GOC Graph
    edges <- as_edgelist(x@th[[whichThresh]]$goc)
    edges <- cbind(edgeNum = 1:nrow(edges),
                   v1 = sapply(edges[, 1], function(z) {
                     which(V(x@th[[whichThresh]]$goc)$name == z)
                    }),
                   v2 = sapply(edges[, 2], function(z) {
                     which(V(x@th[[whichThresh]]$goc)$name == z)
                    }))
    edgesGOC <- apply(edges, 1, function(i) {
      cbind(c(V(x@th[[whichThresh]]$goc)$centroidX[i["v1"]],
              V(x@th[[whichThresh]]$goc)$centroidX[i["v2"]]),
            c(V(x@th[[whichThresh]]$goc)$centroidY[i["v1"]],
              V(x@th[[whichThresh]]$goc)$centroidY[i["v2"]])) %>%
        Line() %>%
        Lines(ID = as.character(i["edgeNum"]))
    }) %>%
      SpatialLines() %>%
      SpatialLinesDataFrame(
        data = data.frame(
          edgeNum = 1:nrow(edges),
          weight = edge_attr(x@th[[whichThresh]]$goc, weight)
        )
      )

    verticesGOC <- SpatialPoints(cbind(V(x@th[[whichThresh]]$goc)$centroidX,
                                       V(x@th[[whichThresh]]$goc)$centroidY))

    ## Shortest path
    startEndPolygons <- point(x, coords)$pointPolygon[, whichThresh]
    startEndPath <- shortest_paths(
      x@th[[whichThresh]]$goc,
      which(V(x@th[[whichThresh]]$goc)$polygonId == startEndPolygons[1]),
      which(V(x@th[[whichThresh]]$goc)$polygonId == startEndPolygons[2]),
      weights = V(x@th[[whichThresh]]$goc)$meanWeight
    ) %>%
      `[[`(1) %>%
      `[[`(1) %>%
      as.numeric()

    shortestPathEdges <- cbind(V(x@th[[whichThresh]]$goc)$centroidX[startEndPath],
                               V(x@th[[whichThresh]]$goc)$centroidY[startEndPath]) %>%
      Line() %>%
      Lines(ID = "1") %>%
      list() %>%
      SpatialLines()

    shortestPathVertices <- SpatialPoints(cbind(
      V(x@th[[whichThresh]]$goc)$centroidX[startEndPath],
      V(x@th[[whichThresh]]$goc)$centroidY[startEndPath]))
    pathDist <- distances(
      x@th[[whichThresh]]$goc,
      v = V(x@th[[whichThresh]]$goc)[startEndPath[1]],
      weights = edge_attr(x@th[[whichThresh]]$goc, weight)
    )[startEndPath[length(startEndPath)]]

    voronoiBound <- boundaries(grain(x, whichThresh = whichThresh)@voronoi, classes = TRUE)

    result <- new("corridor",
                  voronoi = voronoiBound,
                  linksSP = edgesGOC,
                  nodesSP = verticesGOC,
                  shortestLinksSP = shortestPathEdges,
                  shortestNodesSP = shortestPathVertices,
                  corridorLength = pathDist)

    return(result)
})
