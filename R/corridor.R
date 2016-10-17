#' Visualize corridors between two points using a grains of connectivity (GOC) tessellation at a given scale in vector format
#'
#' @description
#' Given a series of GOC models built at different scales in a \code{GOC} object, visualize the corridor
#' (or shortest path) between two points using one of the tessellations (i.e., scales) in these models.
#' Visualization is exclusively in vector format. \code{\link{GOC}} must be run using the \code{sp=TRUE} option.
#'
#' @param GOC  A \code{GOC} object created by \code{\link{GOC}}
#'
#' @param whichThresh  Integer giving the index of the threshold to visualize.
#'
#' @param coords  A two column matrix or a \code{\link{SpatialPoints}} object
#'                giving coordinates at the end points of the corridor.
#'
#' @param doPlot  Logical.  If \code{TRUE} plots a vector visualization of the
#'                corridor at the given scale.
#'                For full control, manually produce plots using the outputs of this function.
#'
#' @param weight  The GOC graph link weight to use in calculating the distance.
#'                Please see details in \code{\link{distance}}.
#'
#' @return A list object:
#'
#' \describe{
#'   \item{\code{$voronoiSP}}{vector representation of polygons in the tessellation (\code{SpatialPolygonsDataFrame})}
#'
#'   \item{\code{$linksSP}}{vector representation of links in the grains of connectivity graph (\code{SpatialLinesDataFrame})}
#'
#'   \item{\code{$nodesSP}}{vector representation of the nodes in the grains of connectivity graph (\code{SpatialPoints})}
#'
#'   \item{\code{$shortestLinksSP}}{vector representation of the links in the shortest path between coordinates (\code{SpatialLines})}
#'
#'   \item{\code{$shortestNodesSP}}{vector representation of the nodes in the shortest path between coordinates (\code{SpatialPoints})}
#'
#'   \item{\code{$corridorLength}}{gives the length of the shortest path between coordinates in accumulated resistance units}
#' }
#'
#' @references
#' Fall, A., M.-J. Fortin, M. Manseau, D. O'Brien.  (2007) Spatial graphs: Principles and applications for habitat connectivity. Ecosystems. 10:448:461.\cr\cr
#' Galpern, P., M. Manseau, P.J. Wilson. (2012) Grains of connectivity: analysis at multiple spatial scales in landscape genetics. Molecular Ecology 21:3996-4009.\cr
#'
#' @author Paul Galpern
#' @docType methods
#' @export
#' @importFrom graphics plot
#' @importFrom igraph '%>%' as_edgelist edge_attr shortest_paths V
#' @importFrom sp Line Lines SpatialLines SpatialLinesDataFrame SpatialPoints
#' @include visualize.R
#' @rdname corridor
#' @seealso \code{\link{GOC}}, \code{\link{visualize}}
#'
#' @examples
#' \dontrun{
#' ## Load raster landscape
#' tiny <- raster(system.file("extdata/tiny.asc", package="grainscape2"))
#'
#' ## Create a resistance surface from a raster using an is-becomes reclassification
#' tinyCost <- reclassify(tiny, rcl = cbind(c(1, 2, 3, 4), c(1, 5, 10, 12)))
#'
#' ## Produce a patch-based MPG where patches are resistance features=1
#' tinyPatchMPG <- MPG(cost = tinyCost, patch = (tinyCost == 1))
#'
#' ## Extract a representative subset of 5 grains of connectivity using sp=TRUE
#' tinyPatchGOC <- GOC(tinyPatchMPG, nThresh = 5, sp = TRUE)
#'
#' ## Quick visualization of a corridor
#' corridorStartEnd <- rbind(c(10,10), c(90,90))
#' corridor(tinyPatchGOC, whichThresh = 3, coords = corridorStartEnd, doPlot = TRUE)
#'
#' ## More control over a corridor visualization
#' tinyPatchCorridor <- corridor(tinyPatchGOC, whichThresh = 3, coords = corridorStartEnd)
#' plot(tinyPatchCorridor$voronoiSP, col = "lightgrey", border = "white", lwd = 2)
#' plot(tinyPatchCorridor$linksSP, col = "darkred", lty = "dashed", add = TRUE)
#' plot(tinyPatchCorridor$nodesSP, col = "darkred", pch = 21, bg="white", add = TRUE)
#' plot(tinyPatchCorridor$shortestLinksSP, col = "darkred", lty = "solid", lwd = 2, add = TRUE)
#' plot(tinyPatchCorridor$shortestNodesSP, col = "darkred", pch = 21, bg = "darkred", add = TRUE)
#' mtext(paste("Corridor shortest path length:",
#'             round(tinyPatchCorridor$corridorLength, 2),
#'             "resistance units"), side = 1)
#' }
#'
corridor <- function(GOC, whichThresh, coords, doPlot = FALSE, weight = "meanWeight") {
  if (!inherits(GOC, "GOC")) {
    stop("grainscape2:  input object must be of class 'GOC'.  Run GOC() first using sp=TRUE.", call. = FALSE)
  }

  if (!requireNamespace("rgeos", quietly = TRUE)) {
    stop("grainscape2:  rgeos package must be installed to use sp=TRUE")
  }

  if (is.null(GOC$voronoiSP)) {
    stop("grainscape2:  GOC object must be produced using sp=TRUE", call. = FALSE)
  }

  ## Check whichThresh
  if ((length(whichThresh) > 1) || (!(whichThresh %in% 1:length(GOC$th)))) {
    stop("grainscape2:  whichThresh must index a single threshold existing in the GOC object", call. = FALSE)
  }

  if (!(weight %in% names(edge_attr(GOC$th[[1]]$goc)))) {
    stop("grainscape2:  link weight attribute with this name doesn't exist in GOC object", call. = FALSE)
  }

  if ((is.null(dim(coords))) & (!inherits(coords, "SpatialPoints"))) {
    coords <- t(as.matrix(coords))
  }

  if (!inherits(coords, "SpatialPoints") && (dim(coords)[2] != 2)) {
    stop("grainscape2:  coords must be a SpatialPoints object or a matrix of two columns giving X and Y coordinates", call. = FALSE)
  }

  if ((inherits(coords, "SpatialPoints") && (length(coords) > 2)) || (nrow(coords) > 2)) {
    warning("grainscape2:  using only first two sets of coordinates for corridor start and end points", call. = FALSE)
    coords <- coords[1:2, ]
  }

  ## GOC Graph
  edges <- as_edgelist(GOC$th[[whichThresh]]$goc)
  edges <- cbind(edgeNum = 1:nrow(edges),
                 v1 = sapply(edges[, 1], function(x) {
                   which(V(GOC$th[[whichThresh]]$goc)$name == x)
                  }),
                 v2 = sapply(edges[, 2], function(x) {
                   which(V(GOC$th[[whichThresh]]$goc)$name == x)
                  }))
  edgesGOC <- apply(edges, 1, function(i) {
    cbind(c(V(GOC$th[[whichThresh]]$goc)$centroidX[i["v1"]],
            V(GOC$th[[whichThresh]]$goc)$centroidX[i["v2"]]),
          c(V(GOC$th[[whichThresh]]$goc)$centroidY[i["v1"]],
            V(GOC$th[[whichThresh]]$goc)$centroidY[i["v2"]])) %>%
      Line() %>%
      Lines(ID = as.character(i["edgeNum"]))
  }) %>%
    SpatialLines() %>%
    SpatialLinesDataFrame(data = data.frame(
      edgeNum = 1:nrow(edges),
      weight = edge_attr(GOC$th[[whichThresh]]$goc, weight)
    ))

  verticesGOC <- SpatialPoints(cbind(V(GOC$th[[whichThresh]]$goc)$centroidX,
                                     V(GOC$th[[whichThresh]]$goc)$centroidY))

  ## Shortest path
  startEndPolygons <- point(GOC, coords)$pointPolygon[, whichThresh]
  startEndPath <- shortest_paths(GOC$th[[whichThresh]]$goc,
                                 which(V(GOC$th[[whichThresh]]$goc)$polygonId == startEndPolygons[1]),
                                 which(V(GOC$th[[whichThresh]]$goc)$polygonId == startEndPolygons[2]),
                                 weights = V(GOC$th[[whichThresh]]$goc)$meanWeight) %>%
    `[[`(1) %>%
    `[[`(1) %>%
    as.numeric()

  shortestPathEdges <- cbind(V(GOC$th[[whichThresh]]$goc)$centroidX[startEndPath],
                             V(GOC$th[[whichThresh]]$goc)$centroidY[startEndPath]) %>%
    Line() %>%
    Lines(ID = "1") %>%
    list() %>%
    SpatialLines()

  shortestPathVertices <- SpatialPoints(cbind(
    V(GOC$th[[whichThresh]]$goc)$centroidX[startEndPath],
    V(GOC$th[[whichThresh]]$goc)$centroidY[startEndPath]))
  pathDist <- distances(
    GOC$th[[whichThresh]]$goc,
    v = V(GOC$th[[whichThresh]]$goc)[startEndPath[1]],
    weights = edge_attr(GOC$th[[whichThresh]]$goc, weight)
  )[startEndPath[length(startEndPath)]]

  voronoiSP <- visualize(GOC, whichThresh, sp = TRUE)$voronoiSP

  ## Do plot
  if (doPlot == 1) {
    plot(voronoiSP, border = "white", col = "grey88", lwd = 2)
    plot(edgesGOC, add = TRUE, col = "grey60", lwd = 1.5)
    plot(verticesGOC, add = TRUE, pch = 21, col = "grey60", bg = "white", cex = 0.75)
    plot(shortestPathEdges, add = TRUE, col = "black", lwd = 2)
    plot(shortestPathVertices, add = TRUE,  pch = 21, col = "black", bg = "white", cex = 0.75)
  } else if (doPlot == 2) {
    plot(voronoiSP, border = "black", lwd = 0.75)
    plot(edgesGOC, add = TRUE, col = "darkgray", lwd = 1.5)
    plot(verticesGOC, add = TRUE, pch = 21, col = "darkgrey", bg = "white", cex = 0.75)
    plot(shortestPathEdges, add = TRUE, col = "black", lwd = 2)
    plot(shortestPathVertices, add = TRUE,  pch = 21, col = "black", bg = "white", cex = 0.75)
  }

  result <- list(
    voronoiSP = voronoiSP,
    linksSP = edgesGOC,
    nodesSP = verticesGOC,
    shortestLinksSP = shortestPathEdges,
    shortestNodesSP = shortestPathVertices,
    corridorLength = pathDist
  )
  return(result)
}
