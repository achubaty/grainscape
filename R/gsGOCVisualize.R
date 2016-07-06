## gsGOCVisualize
gsGOCVisualize <- function(gsGOC, whichThresh, sp=FALSE, doPlot=FALSE) {

  if (class(gsGOC) != "gsGOC") {
    stop("grainscape:  input object must be of class 'gsGOC'.  Run gsGOC() first.", call.=FALSE)
  }

  if (sp) {
    if (!require(rgeos)) stop("grainscape:  rgeos package must be installed to use sp=TRUE")
  }

  ## Check whichThresh
  if ((length(whichThresh) > 1) || (!(whichThresh %in% 1:length(gsGOC$th)))) {
    stop("grainscape:  whichThresh must index a single threshold existing in the gsGOC object", call.=FALSE)
  }

  if (sp && is.null(gsGOC$voronoiSP)) {
    stop("grainscape:  gsGOC object must also be produced using sp=TRUE", call.=FALSE)
  }

  results <- list()

  results$summary <- gsGOC$summary[whichThresh, ]

  if (is.igraph(gsGOC$th[[whichThresh]]$goc)) {
    threshGraph <- gsGOC$th[[whichThresh]]$goc

    ## Produce is-becomes reclassifyification table for voronoi raster
    rclTable <-  matrix(0, 1, 2)
    for (i in 1:length(V(threshGraph)$polygonId)) {
      rclTable <- rbind(rclTable, cbind(as.integer(unlist(strsplit(V(threshGraph)$patchId[i], ", "))), as.integer(V(threshGraph)$polygonId[i])))
    }
    rclTable <- rclTable[2:nrow(rclTable), ]
    results$voronoi <- reclassify(gsGOC$voronoi, rcl=rclTable)

    results$centroids <- SpatialPoints(cbind(V(threshGraph)$centroidX, V(threshGraph)$centroidY))

    ## Take the SpatialPolygons object and combine polygons as necessary
    if (sp) {
      cat("Creating SpatialPolygons\n")
      voronoiSP <- geometry(gsGOC$voronoiSP)
      indexSP <- as(gsGOC$voronoiSP, "data.frame")[,1]
      newVoronoi <- NULL
      for (i in 1:length(V(threshGraph)$polygonId)) {
        fromId <- as.integer(unlist(strsplit(V(threshGraph)$patchId[i], ", ")))
        toId <- as.character(as.integer(V(threshGraph)$polygonId[i]))
        if (length(fromId) > 1) {
          thisPolygon <- NULL
          for (iFrom in 2:length(fromId)) {
            if (is.null(thisPolygon)) {
              thisPolygon <- gUnion(voronoiSP[which(indexSP==fromId[iFrom-1])], voronoiSP[which(indexSP==fromId[iFrom])], id=toId)
            }
            else {
              thisPolygon <- gUnion(thisPolygon, voronoiSP[which(indexSP==fromId[iFrom])], id=toId)
            }
          }
        }
        else {
          thisPolygon <- spChFIDs(voronoiSP[which(indexSP==fromId)], toId)
        }
        if (is.null(newVoronoi)) {
          newVoronoi <- thisPolygon
        }
        else {
          newVoronoi <- rbind(newVoronoi, thisPolygon)
        }
      }
      newVoronoi <- SpatialPolygonsDataFrame(newVoronoi, data.frame(polygonId=V(threshGraph)$polygonId, row.names=V(threshGraph)$polygonId))
      results$voronoiSP <- newVoronoi
    }

    if (doPlot) {
      if (sp) {
        plot(results$voronoiSP)
      }
      else {
        plot(results$voronoi, main=paste(c("whichThresh=", whichThresh), collapse=""))
      }
    }
  }


  return(results)
}
