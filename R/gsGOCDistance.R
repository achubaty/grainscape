## gsGOCDistance
gsGOCDistance <- function(gsGOC, coords, weight="meanWeight") {

  if (class(gsGOC) != "gsGOC") {
    stop("grainscape:  input object must be of class 'gsGOC'.  Run gsGOC() first.", call.=FALSE)
  }

  if ((is.null(dim(coords))) & (class(coords) != "SpatialPoints")) {
    coords <- t(as.matrix(coords))
  }

  if ((class(coords) != "SpatialPoints") && (dim(coords)[2] != 2)) {
    stop("grainscape:  coords must be a SpatialPoints object or a matrix of two columns giving X and Y coordinates", call.=FALSE)
  }

  if (!(weight %in% list.edge.attributes(gsGOC$th[[1]]$goc))) {
    stop("grainscape:  link weight attribute with this name doesn't exist in gsGOC object", call.=FALSE)
  }

  whichGrain <- gsGOCPoint(gsGOC, coords)$pointPolygon

  results <- list()
  results$metaData <- gsGOC$metaData
  results$th <- vector("list", ncol(whichGrain))

  for (iThresh in 1:ncol(whichGrain)) {
    threshGraph <- gsGOC$th[[iThresh]]$goc

    if (is.igraph(threshGraph)) {
      E(threshGraph)$weight <- get.edge.attribute(threshGraph, weight)
      vertices <- sapply(whichGrain[,iThresh], function(x) which(V(threshGraph)$polygonId==x))
      results$th[[iThresh]]$grainD <- shortest.paths(threshGraph, v=vertices)[, vertices]
    }
    else {
      results$th[[iThresh]] <- NA
    }
  }
  return(results)
}
