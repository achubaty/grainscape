## gsThreshold
gsThreshold <- function(gsMPG, weight="lcpPerimWeight", nThresh=NULL, doThresh=NULL)   {

  if ((class(gsMPG) != "gsMPG")) {
    stop("grainscape: gsMPG must be a 'gsMPG' object")
  }

  baseGraph <- gsMPG$mpg

  threshGraph <- vector("list")

  linkWeight <- try(get.edge.attribute(baseGraph, weight), silent=TRUE)
  if (class(linkWeight) == "try-error") {
    stop("grainscape: weight must be the name of an existing link attribute to threshold (e.g. 'lcpPerimWeight')", call.=FALSE)
  }

  if (is.null(nThresh) && is.null(doThresh)) {
    stop("grainscape: either nThresh or doThresh must be specified", call.=FALSE)
  }
  else if (!is.null(nThresh) && !is.null(doThresh)) {
    stop("grainscape: only one of nThresh or doThresh must be specified", call.=FALSE)
  }
  else if (is.null(doThresh)) {
    doThresh <- seq(0, max(linkWeight), length=nThresh)
  }

  threshGraph$summary <- data.frame(maxLink=doThresh)

  threshGraph$th <- lapply(1:length(doThresh), function(i) {
    delete.edges(baseGraph, which(linkWeight > doThresh[i]))
  })

  threshGraph$summary$nComponents <- unlist(lapply(threshGraph$th, function(x) clusters(x)$no))

  return(threshGraph)
}
## gsGOC
gsGOC <- function(gsMPG, nThresh=NULL, doThresh=NULL, weight="lcpPerimWeight", sp=FALSE, verbose=3) {

  if (class(gsMPG) != "gsMPG") {
    stop("grainscape: graph must be a gsMPG object", call.=FALSE)
  }

  if (sp) {
    if (!require(rgeos)) stop("grainscape:  rgeos package must be installed to use sp=TRUE")
  }

  threshGraph <- vector("list")
  baseGraph <- gsMPG$mpg

  linkWeight <- try(get.edge.attribute(baseGraph, weight), silent=TRUE)
  if (class(linkWeight) == "try-error") {
    stop("grainscape: weight must be the name of an existing link attribute to threshold (e.g. 'lcpPerimWeight')", call.=FALSE)
  }

  if (is.null(nThresh) && is.null(doThresh)) {
    stop("grainscape: either nThresh or doThresh must be specified", call.=FALSE)
  }
  else if (!is.null(nThresh) && !is.null(doThresh)) {
    stop("grainscape: only one of nThresh or doThresh must be specified", call.=FALSE)
  }
  else if (is.null(doThresh)) {
    ## Determine nThresh unique thresholds covering the full range of possibilities in terms of the number
    ## of polygons

    allUniqueThresh <- t(sapply(sort(c(0,unique(linkWeight))), function(x) cbind(x, clusters(delete.edges(gsMPG$mpg, which(linkWeight > x)))$no)))
    doThresh <- allUniqueThresh[!duplicated(allUniqueThresh[,2]), 1]
    doThresh <- doThresh[round(seq(1, length(doThresh), length=nThresh))]
  }


  threshGraph$metaData <- gsMPG$metaData
  threshGraph$voronoi <- gsMPG$voronoi
  threshGraph$voronoi[threshGraph$voronoi==-1] <- NA
  threshGraph$summary <- data.frame(maxLink=doThresh)


  ## Optionally retain a vectorized version of the smallest grain of connectivity
  ## That can later be used to create larger grains by aggregation
  if (sp) {
    if (verbose>=2) cat("Creating SpatialPolygons for smallest grain\n")
    if (verbose>=3) {
      cat("  Time for completion is dependent on the number of patches and the dimensions of the raster\n")
      cat("  Occasional failures caused by memory errors are due to an as-yet uncorrected bug in the GEOS library (rgeos).  See manual.\n")
    }
    threshGraph$voronoiSP <- rasterToPolygons(threshGraph$voronoi, dissolve=TRUE)
  }


  allLinks <- get.edges(baseGraph, E(baseGraph))

  ## Check MPG for orphaned patches
  ## A workaround has not yet been implemented
  unlinkedPatches <- as.integer(V(baseGraph)$name[which(sapply(V(baseGraph)$name, function(x) sum(allLinks==as.integer(x)))==0)])
  if (length(unlinkedPatches) > 0) {

    for (iPatch in unlinkedPatches) {
      ## Identify the largest adjacent Voronoi region
      #adjacentVor <- unique(threshGraph$voronoi[adjacent(threshGraph$voronoi, cells=which(threshGraph$voronoi[]==iPatch), target=which(threshGraph$voronoi[]!=iPatch), directions=8, pairs=TRUE)[,2]])
      #largestAdjacentVor <- adjacentVor[which.max(sapply(adjacentVor, function(x) sum(threshGraph$voronoi[]==x, na.rm=TRUE)))]

      #if (!is.null(largestAdjacentVor)) {
      #    threshGraph$voronoi[threshGraph$voronoi==iPatch] <- largestAdjacentVor

      #    warning(paste("patchId=", iPatch, " has no connecting links in the MPG.  Its Voronoi region has been assigned to the largest adjacent region (patchId="
      #        , largestAdjacentVor, ").  It will be ignored for GOC-related analyses.\n", sep=""), call.=FALSE)
      #}
      #else {
      warning(paste("patchId=", iPatch, " has no connecting links in the MPG.  This is likely caused by a patch surrounded in missing values (NA cells).\n",
                    "  At present, all patches must be linked to at least on other patch in the MPG for GOC analyses.\n  Replacing NA cells in the cost or sa rasters may be required\n", sep=""), call.=FALSE)
    }
    stop("grainscape:  cost, patch and/or sa rasters used to create the MPG present a limit case for GOC analyses.  Generated warnings may indicated cause.\nWorkaround for these cases has not yet been implemented.  Please contact the package author for more information.", call.=FALSE)
    ## Remove the vertices representing these patches from the mpg
    #baseGraph <- delete.vertices(baseGraph, which(V(baseGraph)$name %in% as.character(unlinkedPatches))-1)
    #allLinks <- get.edges(baseGraph, E(baseGraph)) + 1
    #linkWeight <- get.edge.attribute(baseGraph, weight)
  }


  linkId <- get.edge.attribute(baseGraph, "linkId")

  cellXY <- coordinates(threshGraph$voronoi)

  threshGraph$th <- vector("list", length(doThresh))

  for (iThresh in 1:length(doThresh)) {

    if (verbose>=1) cat("Threshold", iThresh, "of", length(doThresh), "\n")
    tGraph <- delete.edges(baseGraph, which(linkWeight > doThresh[iThresh]))

    ## Determine the component structure of the threshold graph
    componentList <- clusters(tGraph)

    ## Determine if there is more than one component in the graph (if not, return NA)
    if (componentList$no > 1) {
      components <- componentList$membership


      ## Determine which edges have endpoints in different components, and create a lookup data frame
      linkComponentLookup <- cbind(linkId, get.edge.attribute(baseGraph, weight), allLinks,
                                   t(apply(allLinks, 1, function(x) c(components[x[1]], components[x[2]]))))

      linkComponentLookup <- data.frame(linkComponentLookup[linkComponentLookup[,5] != linkComponentLookup[,6],])

      ## Deal with the case when there are exactly 2 components
      if (ncol(linkComponentLookup) == 1) {
        linkComponentLookup <- data.frame(t(linkComponentLookup))
      }

      ## Exclude cases when there are patches that have no edges
      if (nrow(linkComponentLookup)>0) {

        ## Standardize component link names (i.e. give a link from component 2 to component 1
        ## the same name as a link from component 1 to component 2)
        linkComponentLookup <- cbind(linkComponentLookup, matrix(NA, nrow(linkComponentLookup), 1))
        names(linkComponentLookup) <- c("linkId", "linkWeight", "node1", "node2", "compNode1", "compNode2", "compLinkId")
        done <- rep(FALSE, nrow(linkComponentLookup))

        for (i in 1:nrow(linkComponentLookup)) {
          if (!done[i]) {
            c1 <- linkComponentLookup[i,"compNode1"]
            c2 <- linkComponentLookup[i,"compNode2"]
            sameLink <- (linkComponentLookup[,"compNode1"] == c1) & (linkComponentLookup[,"compNode2"] == c2) | ((linkComponentLookup[,"compNode1"] == c2) & (linkComponentLookup[,"compNode2"] == c1))
            linkComponentLookup[sameLink,"compLinkId"] <- paste(c1,c2,sep="_")
            done[sameLink] <- TRUE
          }
        }

        ## Build data.frame for component graph
        ## Find maximum, minimum, mean, and median edge weights between components
        maxWeight <- as.vector(sapply(unique(linkComponentLookup[,"compLinkId"]), function(x)
          max(linkComponentLookup[linkComponentLookup$compLinkId==x,"linkWeight"])))
        linkIdMaxWeight <- as.vector(sapply(unique(linkComponentLookup[,"compLinkId"]), function(x)
          linkComponentLookup[linkComponentLookup$compLinkId==x,"linkId"][which.max(linkComponentLookup[linkComponentLookup$compLinkId==x,"linkWeight"])]))
        minWeight <- as.vector(sapply(unique(linkComponentLookup[,"compLinkId"]), function(x)
          min(linkComponentLookup[linkComponentLookup$compLinkId==x,"linkWeight"])))
        linkIdMinWeight <- as.vector(sapply(unique(linkComponentLookup[,"compLinkId"]), function(x)
          linkComponentLookup[linkComponentLookup$compLinkId==x,"linkId"][which.min(linkComponentLookup[linkComponentLookup$compLinkId==x,"linkWeight"])]))
        medianWeight <- as.vector(sapply(unique(linkComponentLookup[,"compLinkId"]), function(x)
          median(linkComponentLookup[linkComponentLookup$compLinkId==x,"linkWeight"])))
        meanWeight <- as.vector(sapply(unique(linkComponentLookup[,"compLinkId"]), function(x)
          mean(linkComponentLookup[linkComponentLookup$compLinkId==x,"linkWeight"])))
        numEdgesWeight <- as.vector(sapply(unique(linkComponentLookup[,"compLinkId"]), function(x)
          sum(linkComponentLookup$compLinkId==x)))

        ## Get all linkIds between components and add them as a comma-delimited list
        linkIdAll <- as.character(sapply(unique(linkComponentLookup[,"compLinkId"]), function(x) paste(linkComponentLookup[linkComponentLookup$compLinkId==x, "linkId"], collapse=", ")))

        ## Convert back from string representation of component linkIds to numeric
        componentGraphNodes <- do.call(rbind,strsplit(unique(linkComponentLookup$compLinkId), "_"))

        ## Produce component graph with all edge attributes, and vertex attributes containing a comma-delimited string of vertex names
        componentGraph <- graph.data.frame(data.frame(componentGraphNodes,
                                                      maxWeight,
                                                      linkIdMaxWeight,
                                                      minWeight,
                                                      linkIdMinWeight,
                                                      medianWeight,
                                                      meanWeight,
                                                      numEdgesWeight,
                                                      linkIdAll),
                                           directed=F)

        V(componentGraph)$polygonId <- V(componentGraph)$name
        sourcePatchId <- sapply(as.numeric(as.character(V(componentGraph)$polygonId)), function(x)
          paste(as.character(V(baseGraph)$patchId[components==x]), collapse=", "))

        ## Produce a raster representing this grain of connectivity
        gocRaster <- threshGraph$voronoi


        rawreclassifyVor <- cbind(sourcePatchId, V(componentGraph)$polygonId)
        reclassifyVor <- matrix(0, 1, 2)
        for (j in 1:nrow(rawreclassifyVor)) {
          reclassifyVor <- rbind(reclassifyVor, cbind(as.integer(strsplit(rawreclassifyVor[j,1], ", ")[[1]]), as.integer(rawreclassifyVor[j,2])))
        }
        reclassifyVor <- reclassifyVor[2:nrow(reclassifyVor), ]

        gocRaster <- reclassify(gocRaster, rcl=reclassifyVor)

        ## Find centroids of each polygon and add as vertex attributes
        uniquePolygons <- V(componentGraph)$polygonId

        rasX <- gocRaster
        rasY <- rasX
        rasX[] <- cellXY[,1]
        rasY[] <- cellXY[,2]

        centroids <- cbind(zonal(rasX, gocRaster, fun='mean'), zonal(rasY, gocRaster, fun='mean')[,2])
        centroids <- centroids[centroids[,1]>=0, 2:3]
        centroids <- centroids[as.integer(uniquePolygons), ]

        V(componentGraph)$centroidX <- centroids[,1]
        V(componentGraph)$centroidY <- centroids[,2]


        ## Find areas of each polygon and add as a vertex attribute
        polygonArea <- freq(gocRaster)
        polygonArea <- polygonArea[polygonArea[,1]>=0, 2]
        polygonArea <- polygonArea[as.integer(uniquePolygons)]

        V(componentGraph)$polygonArea <- polygonArea

        ## Find the total patch area, total patch edge area, and total core area in each polygon and add as vertex attributes
        patchAreaLookup <- cbind(V(baseGraph)$patchId, V(baseGraph)$patchArea, V(baseGraph)$patchEdgeArea, V(baseGraph)$coreArea)
        V(componentGraph)$totalPatchArea <- as.numeric(unlist(sapply(sourcePatchId, function(x) sum(patchAreaLookup[patchAreaLookup[,1] %in% as.numeric(strsplit(x, ", ")[[1]]), 2]))))
        V(componentGraph)$totalPatchEdgeArea <- as.numeric(unlist(sapply(sourcePatchId, function(x) sum(patchAreaLookup[patchAreaLookup[,1] %in% as.numeric(strsplit(x, ", ")[[1]]), 3]))))
        V(componentGraph)$totalCoreArea <- as.numeric(unlist(sapply(sourcePatchId, function(x) sum(patchAreaLookup[patchAreaLookup[,1] %in% as.numeric(strsplit(x, ", ")[[1]]), 4]))))

        V(componentGraph)$patchId <- sourcePatchId



        ## Find distances between each polygon centroid
        eucCentroidWeight <- apply(get.edgelist(componentGraph), 1, function(x) {
          x1 <- which(uniquePolygons==x[1])
          x2 <- which(uniquePolygons==x[2])

          return(sqrt((centroids[x2, 1]-centroids[x1, 1])^2 + (centroids[x2, 2] - centroids[x1, 2])^2))
        })
        E(componentGraph)$eucCentroidWeight <- eucCentroidWeight

        threshGraph$th[[iThresh]]$goc <- componentGraph


      }
      else {
        threshGraph$th[[iThresh]]$goc <- NA
      }
    }
    else {
      threshGraph$th[[iThresh]]$goc <- NA
    }
  }


  ## Add data to the summary table
  threshGraph$summary$nPolygon <- unlist(lapply(threshGraph$th, function(x) if (is.igraph(x$goc)) vcount(x$goc) else NA))
  threshGraph$summary$maxPolygonArea <- unlist(lapply(threshGraph$th, function(x) if (is.igraph(x$goc)) max(V(x$goc)$polygonArea) else NA))
  threshGraph$summary$minPolygonArea <- unlist(lapply(threshGraph$th, function(x) if (is.igraph(x$goc)) min(V(x$goc)$polygonArea) else NA))
  threshGraph$summary$meanPolygonArea <- unlist(lapply(threshGraph$th, function(x) if (is.igraph(x$goc))mean(V(x$goc)$polygonArea) else NA))
  threshGraph$summary$medianPolygonArea <- unlist(lapply(threshGraph$th, function(x) if (is.igraph(x$goc)) median(V(x$goc)$polygonArea) else NA))

  ## Find ECS (Expected cluster size; O'Brien et al, 2006) using totalPatchArea
  threshGraph$summary$ECS <- unlist(lapply(threshGraph$th, function(x) if (is.igraph(x$goc)) sum(V(x$goc)$totalPatchArea^2)/sum(V(x$goc)$totalPatchArea) else NA))
  ## Find ECSCore (Expected cluster size; O'Brien et al, 2006) using totalCoreArea
  threshGraph$summary$ECSCore <- unlist(lapply(threshGraph$th, function(x) if (is.igraph(x$goc)) sum(V(x$goc)$totalCoreArea^2)/sum(V(x$goc)$totalCoreArea) else NA))
  class(threshGraph) <- "gsGOC"

  return(threshGraph)
}
