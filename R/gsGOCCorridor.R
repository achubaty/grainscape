## gsGOCCorridor
gsGOCCorridor <- function(gsGOC, whichThresh, coords, doPlot=FALSE, weight="meanWeight") {

  if (class(gsGOC) != "gsGOC") {
    stop("grainscape:  input object must be of class 'gsGOC'.  Run gsGOC() first using sp=TRUE.", call.=FALSE)
  }

  if (!require(rgeos)) stop("grainscape:  rgeos package must be installed to use gsGOCCorridor()")

  if (is.null(gsGOC$voronoiSP)) {
    stop("grainscape:  gsGOC object must be produced using sp=TRUE", call.=FALSE)
  }

  ## Check whichThresh
  if ((length(whichThresh) > 1) || (!(whichThresh %in% 1:length(gsGOC$th)))) {
    stop("grainscape:  whichThresh must index a single threshold existing in the gsGOC object", call.=FALSE)
  }

  if (!(weight %in% list.edge.attributes(gsGOC$th[[1]]$goc))) {
    stop("grainscape:  link weight attribute with this name doesn't exist in gsGOC object", call.=FALSE)
  }

  if ((is.null(dim(coords))) & (class(coords) != "SpatialPoints")) {
    coords <- t(as.matrix(coords))
  }

  if ((class(coords) != "SpatialPoints") && (dim(coords)[2] != 2)) {
    stop("grainscape:  coords must be a SpatialPoints object or a matrix of two columns giving X and Y coordinates", call.=FALSE)
  }

  if (((class(coords) == "SpatialPoints") && (length(coords) > 2)) || (nrow(coords) > 2)) {
    warning("grainscape:  using only first two sets of coordinates for corridor start and end points", call.=FALSE)
    coords <- coords[1:2, ]
  }



  ## GOC Graph
  edges <- get.edgelist(gsGOC$th[[whichThresh]]$goc)
  edges <- cbind(edgeNum=1:nrow(edges), v1=sapply(edges[,1], function(x) which(V(gsGOC$th[[whichThresh]]$goc)$name == x)),
                 v2=sapply(edges[,2], function(x) which(V(gsGOC$th[[whichThresh]]$goc)$name == x)))
  edgesGOC <- apply(edges, 1, function(i)
    Lines(Line(cbind(c(V(gsGOC$th[[whichThresh]]$goc)$centroidX[i["v1"]], V(gsGOC$th[[whichThresh]]$goc)$centroidX[i["v2"]]),
                     c(V(gsGOC$th[[whichThresh]]$goc)$centroidY[i["v1"]], V(gsGOC$th[[whichThresh]]$goc)$centroidY[i["v2"]]))),
          ID=as.character(i["edgeNum"])))
  edgesGOC <- SpatialLinesDataFrame(SpatialLines(edgesGOC), data=data.frame(edgeNum=1:nrow(edges), weight=get.edge.attribute(gsGOC$th[[whichThresh]]$goc, weight)))
  verticesGOC <- SpatialPoints(cbind(V(gsGOC$th[[whichThresh]]$goc)$centroidX, V(gsGOC$th[[whichThresh]]$goc)$centroidY))


  ## Shortest path
  startEndPolygons <- gsGOCPoint(gsGOC, coords)$pointPolygon[, whichThresh]
  startEndPath <- get.shortest.paths(gsGOC$th[[whichThresh]]$goc,
                                     which(V(gsGOC$th[[whichThresh]]$goc)$polygonId==startEndPolygons[1]),
                                     which(V(gsGOC$th[[whichThresh]]$goc)$polygonId==startEndPolygons[2]),
                                     weights=V(gsGOC$th[[whichThresh]]$goc)$meanWeight)[[1]]
  shortestPathEdges <- SpatialLines(list(Lines(Line(cbind(V(gsGOC$th[[whichThresh]]$goc)$centroidX[startEndPath], V(gsGOC$th[[whichThresh]]$goc)$centroidY[startEndPath])), ID="1")))
  shortestPathVertices <- SpatialPoints(cbind(V(gsGOC$th[[whichThresh]]$goc)$centroidX[startEndPath], V(gsGOC$th[[whichThresh]]$goc)$centroidY[startEndPath]))
  pathDist <- shortest.paths(gsGOC$th[[whichThresh]]$goc, v=V(gsGOC$th[[whichThresh]]$goc)[startEndPath[1]], weights=get.edge.attribute(gsGOC$th[[whichThresh]]$goc, weight))[startEndPath[length(startEndPath)]]

  voronoiSP <- gsGOCVisualize(gsGOC, whichThresh, sp=TRUE)$voronoiSP

  ## Do plot
  if (doPlot==1) {
    plot(voronoiSP, border="white", col="grey88", lwd=2)
    plot(edgesGOC, add=TRUE, col="grey60", lwd=1.5)
    plot(verticesGOC, add=TRUE, pch=21, col="grey60", bg="white", cex=0.75)
    plot(shortestPathEdges, add=TRUE, col="black", lwd=2)
    plot(shortestPathVertices, add=TRUE,  pch=21, col="black", bg="white", cex=0.75)
  }
  if (doPlot==2) {
    plot(voronoiSP, border="black", lwd=0.75)
    plot(edgesGOC, add=TRUE, col="darkgray", lwd=1.5)
    plot(verticesGOC, add=TRUE, pch=21, col="darkgrey", bg="white", cex=0.75)
    plot(shortestPathEdges, add=TRUE, col="black", lwd=2)
    plot(shortestPathVertices, add=TRUE,  pch=21, col="black", bg="white", cex=0.75)
  }

  result <- list()

  result$voronoiSP <- voronoiSP
  result$linksSP <- edgesGOC
  result$nodesSP <- verticesGOC
  result$shortestLinksSP <- shortestPathEdges
  result$shortestNodesSP <- shortestPathVertices
  result$corridorLength <- pathDist
  return(result)
}
