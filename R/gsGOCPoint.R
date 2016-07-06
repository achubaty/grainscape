## gsGOCPoint
gsGOCPoint <- function(gsGOC, coords) {

  if (class(gsGOC) != "gsGOC") {
    stop("grainscape:  input object must be of class 'gsGOC'.  Run gsGOC() first.", call.=FALSE)
  }

  if ((is.null(dim(coords))) & (class(coords) != "SpatialPoints")) {
    coords <- t(as.matrix(coords))
  }

  if ((class(coords) != "SpatialPoints") && (dim(coords)[2] != 2)) {
    stop("grainscape:  coords must be a SpatialPoints object or a matrix of two columns giving X and Y coordinates", call.=FALSE)
  }




  if (class(coords) != "SpatialPoints") {
    coords <- SpatialPoints(coords)
  }

  ## Remove points that fall in NA locations
  cellPoints <- cellFromXY(gsGOC$voronoi, coords)
  if (suppressWarnings(sum(is.na(gsGOC$voronoi[cellPoints]))) > 0) {
    cellPoints <- suppressWarnings(cellPoints[!is.na(gsGOC$voronoi[cellPoints])])
    stop("grainscape:  there are coords that are not defined on the raster.\n", call.=FALSE)
  }

  grainPoints <- matrix(NA, nrow=length(cellPoints), ncol=length(gsGOC$th))
  totalPatchAreaPoints <- grainPoints
  totalCoreAreaPoints <- grainPoints


  for (iThresh in 1:length(gsGOC$th)) {
    if (is.igraph(gsGOC$th[[iThresh]]$goc)) {
      threshGraph <- gsGOC$th[[iThresh]]$goc

      ## Produce patchId and patchArea lookup tables with polygonId as the index
      patchIdLookup <-  matrix(0, 1, 2)
      for (i in 1:length(V(threshGraph)$polygonId)) {
        patchIdLookup <- rbind(patchIdLookup, cbind(as.integer(V(threshGraph)$polygonId[i]), as.integer(unlist(strsplit(V(threshGraph)$patchId[i], ", ")))))
      }
      patchIdLookup <- patchIdLookup[2:nrow(patchIdLookup), ]
      patchAreaLookup <- cbind(V(threshGraph)$polygonId, V(threshGraph)$totalPatchArea, V(threshGraph)$totalPatchEdgeArea, V(threshGraph)$totalCoreArea)

      ## Faster method which references the cells from the stored voronoi raster
      ## and uses the graph vertex record to determine the polygonId

      grainPoints[, iThresh] <- as.numeric(sapply(gsGOC$voronoi[cellPoints], function(x) patchIdLookup[patchIdLookup[,2]==x,1]))

      totalPatchAreaPoints[, iThresh] <- as.numeric(sapply(grainPoints[, iThresh], function(x) patchAreaLookup[patchAreaLookup[,1]==x, 2] ))
      totalCoreAreaPoints[, iThresh] <- as.numeric(sapply(grainPoints[, iThresh], function(x) patchAreaLookup[patchAreaLookup[,1]==x, 4] ))

    }

  }

  results <- list()
  results$pointPolygon <- grainPoints
  results$pointTotalPatchArea <- totalPatchAreaPoints
  results$pointTotalCoreArea <- totalCoreAreaPoints
  results$pointECS <- apply(totalPatchAreaPoints, 2, mean)
  results$pointECSCore <- apply(totalCoreAreaPoints, 2, mean)
  return(results)
}
