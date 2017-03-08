#' Extract a grain of connectivity (GOC) tessellation at a given scale
#'
#' @description
#' Extract a tessellation (i.e., scales) from a GOC model.
#'
#' @param x   A \code{goc} object created by \code{\link{GOC}}.
#'
#' @param whichThresh  Integer giving the grain threshold to extract.
#'
#' @param sp  Logical.  If \code{TRUE} then produce a \code{\link{SpatialPolygonsDataFrame}}
#'            representation of the selected threshold.
#'            Requires also running \code{\link{GOC}} with \code{sp = TRUE},
#'            and that the \code{rgeos} package is installed.
#'
#' @param ...  Additional arguments to pass to \code{plot} if \code{doPlot = TRUE}).
#'
#' @return  A list object containing the following elements:
#'
#' \describe{
#'   \item{\code{summary}}{gives the properties of the visualized scale of the GOC model;}
#'
#'   \item{\code{voronoi}}{a \code{RasterLayer} giving the Voronoi tessellation;}
#'
#'   \item{\code{centroids}}{a \code{SpatialPoints} objects giving the centroids
#'   of the polygons in the tessellation;}
#'
#'   \item{\code{voronoiSP}}{vector representation of polygons in the tessellation
#'   (\code{SpatialPolygonsDataFrame}; if \code{sp = TRUE})}
#' }
#'
#' @references
#' Fall, A., M.-J. Fortin, M. Manseau, D. O'Brien. (2007) Spatial graphs: Principles and applications for habitat connectivity. Ecosystems 10:448:461.
#'
#' Galpern, P., M. Manseau, P.J. Wilson. (2012) Grains of connectivity: analysis at multiple spatial scales in landscape genetics.  Molecular Ecology 21:3996-4009.
#'
#' @author Paul Galpern and Alex Chubaty
#' @docType methods
#' @export
#' @importFrom graphics plot
#' @importFrom raster as.data.frame plot reclassify
#' @importFrom sp geometry plot SpatialPoints SpatialPolygonsDataFrame spChFIDs
#' @include classes.R
#' @rdname grain
#' @seealso \code{\link{GOC}}
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
#' ## Very quick visualization at the finest scale/grain/threshold,
#' ## producing plot on the default graphics device
#' tinyPatchGOCgrain <- grain(tinyPatchGOC, whichThresh = 1)
#' plot(tinyPatchGOCgrain, col = topo.colors(10))
#'
#' ## Visualize the model at the finest scale/grain/threshold
#' ## Manual control of plotting
#' plot(grain(tinyPatchGOC, whichThresh = 1)@voronoi,
#'      col = sample(rainbow(100)), legend = FALSE, main = "Threshold 1")
#'
#' ## Extract a representative subset of 5 grains of connectivity for vector visualization
#' tinyPatchGOC <- GOC(tinyPatchMPG, nThresh = 5, sp = TRUE)
#'
#' ## grain the model at a selected scale/grain/threshold using vector polygons
#' plot(tinyPatchMPG@patchId, col = "grey", legend = FALSE)
#' plot(grain(tinyPatchGOC, whichThresh = 3, sp = TRUE)@voronoiSP, add = TRUE, lwd = 2)
#' }
#'
setGeneric("grain", function(x, ...) {
  standardGeneric("grain")
})

#' @export
#' @rdname grain
setMethod(
  "grain",
  signature = "goc",
  definition = function(x, whichThresh, sp = FALSE, ...) {
    if (isTRUE(sp) && !requireNamespace("rgeos", quietly = TRUE)) {
      stop("grainscape:  rgeos package must be installed to use sp = TRUE")
    }

    dots <- list(...)

    ## Check whichThresh
    thresholds <- x@summary$id
    if ((length(whichThresh) > 1) || (!(whichThresh %in% thresholds))) {
      stop("grainscape:  whichThresh must index a single threshold existing in the GOC object", call. = FALSE)
    }

    ## Check sp
    if (isTRUE(sp) && identical(x@voronoiSP, .emptySP())) {
      stop("grainscape:  GOC object must also be produced using sp=TRUE", call. = FALSE)
    }

    results <- list()

    results$summary <- x@summary[whichThresh, ]

    if (is_igraph(x@th[[whichThresh]]$goc)) {
      threshGraph <- x@th[[whichThresh]]$goc

      ## Produce is-becomes reclassifyification table for voronoi raster
      rclTable <-  matrix(0, 1, 2)
      for (i in 1:length(V(threshGraph)$polygonId)) {
        rclTable <- rbind(rclTable,
                          cbind(as.integer(unlist(strsplit(V(threshGraph)$patchId[i], ", "))),
                                as.integer(V(threshGraph)$polygonId[i])))
      }
      rclTable <- rclTable[2:nrow(rclTable), ]

      results$voronoi <- reclassify(x@voronoi, rcl = rclTable)

      results$centroids <- SpatialPoints(cbind(V(threshGraph)$centroidX,
                                               V(threshGraph)$centroidY))

      ## Take the SpatialPolygons object and combine polygons as necessary
      if (isTRUE(sp)) {
        message("Creating SpatialPolygons.") ## allow to be silenced
        voronoiSP <- geometry(x@voronoiSP)
        indexSP <- as.data.frame(x@voronoiSP)[, 1]
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
      } else {
        results$voronoiSP <- .emptySPDF()
      }

      if (any(grepl(pattern = "doPlot", names(dots)))) {
        warning("Use of doPlot is deprecated. Use plot(grain(goc, ...)) instead.")
      }
    }

    out <- new("grain", voronoi = results$voronoi, voronoiSP = results$voronoiSP,
               summary = results$summary, centroids = results$centroids)

    return(out)
})
