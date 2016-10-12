#' Visualize grains of connectivity (GOC) tessellations at a given scale
#'
#' @description
#' Given a series of GOC models built at different scales in a \code{gsGOC} object,
#' visualize one the tessellations (i.e., scales) in these models.
#' Visualization is by default in raster format.
#' Vector based visualization is also possible.
#'
#' @param gsGOC  A \code{gsGOC} object created by \code{\link{gsGOC}}.
#'
#' @param whichThresh  Integer giving the index of the threshold to visualize.
#'
#' @param sp  Logical.  If \code{TRUE} then produce a \code{\link{SpatialPolygonsDataFrame}}
#'            representation of the selected threshold.
#'            Requires also running \code{\link{gsGOC}} with \code{sp=TRUE},
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
#' @rdname gsGOCVisualize
#' @seealso \code{\link{gsGOC}}
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
#' tinyPatchMPG <- gsMPG(cost = tinyCost, patch = tinyCost == 1)
#'
#' ## Extract a representative subset of 5 grains of connectivity
#' tinyPatchGOC <- gsGOC(tinyPatchMPG, nThresh = 5)
#'
#' ## Very quick visualization at the finest scale/grain/threshold
#' ## Producing plot on the default graphics device
#' gsGOCVisualize(tinyPatchGOC, whichThresh = 1, doPlot = TRUE)
#'
#' ## Visualize the model at the finest scale/grain/threshold
#' ## Manual control of plotting
#' plot(gsGOCVisualize(tinyPatchGOC, whichThresh = 1)$voronoi,
#'      col = sample(rainbow(100)), legend = FALSE, main = "Threshold 1")
#'
#' ## Extract a representative subset of 5 grains of connectivity for vector visualization
#' tinyPatchGOC <- gsGOC(tinyPatchMPG, nThresh= 5 , sp = TRUE)
#'
#' ## Visualize the model at a selected scale/grain/threshold using vector polygons
#' plot(tinyPatchMPG$patchId, col = "grey", legend = FALSE)
#' plot(gsGOCVisualize(tinyPatchGOC, whichThresh = 3, sp = TRUE)$voronoiSP, add = TRUE, lwd = 2)
#' }
#'
gsGOCVisualize <- function(gsGOC, whichThresh, sp = FALSE, doPlot = FALSE) {
  if (class(gsGOC) != "gsGOC") {
    stop("grainscape2:  input object must be of class 'gsGOC'.  Run gsGOC() first.", call. = FALSE)
  }

  if (sp && !requireNamespace("rgeos", quietly = TRUE)) {
    stop("grainscape2:  rgeos package must be installed to use sp = TRUE")
  }

  ## Check whichThresh
  if ((length(whichThresh) > 1) || (!(whichThresh %in% 1:length(gsGOC$th)))) {
    stop("grainscape2:  whichThresh must index a single threshold existing in the gsGOC object", call. = FALSE)
  }

  if (sp && is.null(gsGOC$voronoiSP)) {
    stop("grainscape2:  gsGOC object must also be produced using sp=TRUE", call. = FALSE)
  }

  results <- list()

  results$summary <- gsGOC$summary[whichThresh, ]

  if (is_igraph(gsGOC$th[[whichThresh]]$goc)) {
    threshGraph <- gsGOC$th[[whichThresh]]$goc

    ## Produce is-becomes reclassifyification table for voronoi raster
    rclTable <-  matrix(0, 1, 2)
    for (i in 1:length(V(threshGraph)$polygonId)) {
      rclTable <- rbind(rclTable,
                        cbind(as.integer(unlist(strsplit(V(threshGraph)$patchId[i], ", "))),
                              as.integer(V(threshGraph)$polygonId[i])))
    }
    rclTable <- rclTable[2:nrow(rclTable), ]
    results$voronoi <- reclassify(gsGOC$voronoi, rcl = rclTable)

    results$centroids <- SpatialPoints(cbind(V(threshGraph)$centroidX,
                                             V(threshGraph)$centroidY))

    ## Take the SpatialPolygons object and combine polygons as necessary
    if (sp) {
      message("Creating SpatialPolygons.")
      voronoiSP <- geometry(gsGOC$voronoiSP)
      indexSP <- as(gsGOC$voronoiSP, "data.frame")[, 1]
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
