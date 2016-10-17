#' Visualize grains of connectivity (GOC) tessellations at a given scale
#'
#' @description
#' Given a series of GOC models built at different scales in a \code{GOC} object,
#' visualize one the tessellations (i.e., scales) in these models.
#' Visualization is by default in raster format.
#' Vector based visualization is also possible.
#'
#' @param GOC  A \code{GOC} object created by \code{\link{GOC}}.
#'
#' @param whichThresh  Integer giving the index of the threshold to visualize.
#'
#' @param sp  Logical.  If \code{TRUE} then produce a \code{\link{SpatialPolygonsDataFrame}}
#'            representation of the selected threshold.
#'            Requires also running \code{\link{GOC}} with \code{sp=TRUE},
#'            and that the \code{rgeos} package is installed.
#'
#' @param doPlot  Logical.  If \code{TRUE} plots a raster (or vector if \code{sp=TRUE})
#'                of the Voronoi tessellation at \code{whichThresh} for quick visualizations.
#'                For full control, manually produce plots using the \code{$voronoi}
#'                or \code{$voronoiSP} objects created by this function.
#'
#' @return  A list object:\cr\cr
#' \code{$summary} giving the properties of the visualized scale of the GOC model\cr
#' \code{$voronoi} giving the tessellation (\code{RasterLayer})\cr
#' \code{$centroids} the centroids of the polygons in the tessellation (\code{SpatialPoints})\cr
#' \code{$voronoiSP} vector representation of polygons in the tessellation (\code{SpatialPolygonsDataFrame}; if \code{sp=TRUE})
#'
#' @references
#' Fall, A., M.-J. Fortin, M. Manseau, D. O'Brien.  (2007) Spatial graphs:  Principles and applications for habitat connectivity.  Ecosystems.  10:448:461\cr
#' Galpern, P., M. Manseau, P.J. Wilson. (2012) Grains of connectivity: analysis at multiple spatial scales in landscape genetics.  Molecular Ecology 21:3996-4009.\cr
#'
#' @author Paul Galpern
#' @docType methods
#' @export
#' @importFrom graphics plot
#' @importFrom igraph 'E<-' is_igraph V
#' @importFrom methods as
#' @importFrom raster plot reclassify
#' @importFrom sp geometry plot SpatialPoints SpatialPolygonsDataFrame spChFIDs
#' @rdname visualize
#' @seealso \code{\link{GOC}}
#'
#' @examples
#' \dontrun{
#' ## Load raster landscape
#' tiny <- raster(system.file("extdata/tiny.asc", package = "grainscape2"))
#'
#' ## Create a resistance surface from a raster using an is-becomes reclassification
#' tinyCost <- reclassify(tiny, rcl = cbind(c(1, 2, 3, 4), c(1, 5, 10, 12)))
#'
#' ## Produce a patch-based MPG where patches are resistance features=1
#' tinyPatchMPG <- MPG(cost = tinyCost, patch = tinyCost == 1)
#'
#' ## Extract a representative subset of 5 grains of connectivity
#' tinyPatchGOC <- GOC(tinyPatchMPG, nThresh = 5)
#'
#' ## Very quick visualization at the finest scale/grain/threshold
#' ## Producing plot on the default graphics device
#' visualize(tinyPatchGOC, whichThresh = 1, doPlot = TRUE)
#'
#' ## Visualize the model at the finest scale/grain/threshold
#' ## Manual control of plotting
#' plot(visualize(tinyPatchGOC, whichThresh = 1)$voronoi,
#'      col = sample(rainbow(100)), legend = FALSE, main = "Threshold 1")
#'
#' ## Extract a representative subset of 5 grains of connectivity for vector visualization
#' tinyPatchGOC <- GOC(tinyPatchMPG, nThresh= 5 , sp = TRUE)
#'
#' ## Visualize the model at a selected scale/grain/threshold using vector polygons
#' plot(tinyPatchMPG$patchId, col = "grey", legend = FALSE)
#' plot(visualize(tinyPatchGOC, whichThresh = 3, sp = TRUE)$voronoiSP, add = TRUE, lwd = 2)
#' }
#'
visualize <- function(GOC, whichThresh, sp = FALSE, doPlot = FALSE) {
  if (!inherits(GOC, "GOC")) {
    stop("grainscape2:  input object must be of class 'GOC'.  Run GOC() first.", call. = FALSE)
  }

  if (isTRUE(sp) && !requireNamespace("rgeos", quietly = TRUE)) {
    stop("grainscape2:  rgeos package must be installed to use sp = TRUE")
  }

  ## Check whichThresh
  if ((length(whichThresh) > 1) || (!(whichThresh %in% 1:length(GOC$th)))) {
    stop("grainscape2:  whichThresh must index a single threshold existing in the GOC object", call. = FALSE)
  }

  if (sp && is.null(GOC$voronoiSP)) {
    stop("grainscape2:  GOC object must also be produced using sp=TRUE", call. = FALSE)
  }

  results <- list()

  results$summary <- GOC$summary[whichThresh, ]

  if (is_igraph(GOC$th[[whichThresh]]$goc)) {
    threshGraph <- GOC$th[[whichThresh]]$goc

    ## Produce is-becomes reclassifyification table for voronoi raster
    rclTable <-  matrix(0, 1, 2)
    for (i in 1:length(V(threshGraph)$polygonId)) {
      rclTable <- rbind(rclTable,
                        cbind(as.integer(unlist(strsplit(V(threshGraph)$patchId[i], ", "))),
                              as.integer(V(threshGraph)$polygonId[i])))
    }
    rclTable <- rclTable[2:nrow(rclTable), ]
    results$voronoi <- reclassify(GOC$voronoi, rcl = rclTable)

    results$centroids <- SpatialPoints(cbind(V(threshGraph)$centroidX,
                                             V(threshGraph)$centroidY))

    ## Take the SpatialPolygons object and combine polygons as necessary
    if (sp) {
      message("Creating SpatialPolygons.")
      voronoiSP <- geometry(GOC$voronoiSP)
      indexSP <- as(GOC$voronoiSP, "data.frame")[, 1]
      newVoronoi <- NULL
      for (i in 1:length(V(threshGraph)$polygonId)) {
        fromId <- as.integer(unlist(strsplit(V(threshGraph)$patchId[i], ", ")))
        toId <- as.character(as.integer(V(threshGraph)$polygonId[i]))
        if (length(fromId) > 1) {
          thisPolygon <- NULL
          for (iFrom in 2:length(fromId)) {
            if (is.null(thisPolygon)) {
              thisPolygon <- rgeos::gUnion(voronoiSP[which(indexSP == fromId[iFrom - 1])],
                                           voronoiSP[which(indexSP == fromId[iFrom])], id = toId)
            } else {
              thisPolygon <- rgeos::gUnion(thisPolygon, voronoiSP[which(indexSP == fromId[iFrom])], id = toId)
            }
          }
        } else {
          thisPolygon <- spChFIDs(voronoiSP[which(indexSP == fromId)], toId)
        }
        if (is.null(newVoronoi)) {
          newVoronoi <- thisPolygon
        } else {
          newVoronoi <- rbind(newVoronoi, thisPolygon)
        }
      }
      results$voronoiSP <- SpatialPolygonsDataFrame(
        newVoronoi,
        data.frame(polygonId = V(threshGraph)$polygonId,
                   row.names = V(threshGraph)$polygonId)
      )
    }

    if (doPlot) {
      if (sp) {
        sp::plot(results$voronoiSP)
      } else {
        raster::plot(results$voronoi, main = paste(c("whichThresh=", whichThresh), collapse = ""))
      }
    }
  }

  return(results)
}
