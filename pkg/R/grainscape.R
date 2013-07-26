## grainscape
## R package


## gsMPG
gsMPG <- function(cost, patch, sa=NULL, outputFolder=NULL, filterPatch=NULL, spreadFactor=0, selesPath=system.file("SELES", package="grainscape")) {
    ## Check OS
    if (.Platform$OS.type != "windows") {
        stop("grainscape:  this function calls an executable (SELES) compiled for Windows.\nIt will not work on your OS.", call.=FALSE)
    }

    ## Check that cost raster is of class RasterLayer
    if ((class(cost) != "RasterLayer")) {
        stop("grainscape: cost raster must be of class RasterLayer", call.=FALSE)
    }
    
    ## Prepare a lattice patch if patch is numeric
    if (class(patch) == "numeric") {
        ## Produce the lattice patch rasters
        focalPointDistFreq <- patch
        patch <- cost
        patch[] <- 0
        patch[cellFromRowColCombine(patch, seq(1,nrow(patch), by=focalPointDistFreq)+focalPointDistFreq/2, seq(1, ncol(patch), by=focalPointDistFreq)+focalPointDistFreq/2)] <- 1
        ## Remove lattice points that fall on NA cost cells
        patch[is.na(cost)] <- 0
    }
    else if ((class(patch) != "RasterLayer")) {
        stop("grainscape: patch must be a raster (patch-based model) OR an integer (lattice model)", call.=FALSE)
    }

    ## Check that input rasters are of class RasterLayer
    if ((class(cost) != "RasterLayer")) {
        stop("grainscape: cost raster must be of class RasterLayer", call.=FALSE)
    }
    
    
    ## Check patch and cost are comparable
    if (!compareRaster(patch, cost, res=TRUE, orig=TRUE, stopiffalse=FALSE)) {
        stop("grainscape: patch and cost rasters must be identical in extent, projection, origin and resolution", call.=FALSE)
    }
    
    ## Check additional geographic features of input rasters
    if (res(cost)[1] != res(cost)[2]) {
        warning(paste("grainscape:  raster cells are not square;  assuming a square cell of ", res(cost)[1], " units", sep=""), call.=FALSE)  
    }
    
    ## Check projection
    if ((projection(cost) != "NA") && (!grepl("UTM|utm", toupper(projection(cost))))) {
        warning("grainscape:  projection suggests that all cells may not be of equal area; Note that grainscape assumes equal area in all calculations", call.=FALSE)
    }
    

    
    st <- proc.time() 
    rasCost <- cost
    rasPatch <- cost
    rasSa <- cost
    
    rasCost[] <- getValues(cost)
    rasPatch[] <- getValues(patch)
    
    ## Check filterPatch
    if (is.null(filterPatch)) {
        ## Set filterPatch smaller than area of cell
        filterPatch <- (prod(res(rasCost))/10000)*0.01
    }
    else {
        ## Set filterPatch as area in hectares
        filterPatch <- (prod(res(rasCost))/10000)*abs(filterPatch)
    }
    
    ## Check sa is comparable with other rasters
    if (!is.null(sa)) {
        if (class(sa) != "RasterLayer") {
            stop("grainscape:  sa raster must be of class RasterLayer", call.=FALSE)
        }
        if (!compareRaster(cost, sa, res=TRUE, orig=TRUE, stopiffalse=FALSE)) {
            stop("grainscape: patch, cost and sa rasters must be identical in extent, projection, origin and resolution", call.=FALSE) 
        }
        rasSa[] <- getValues(sa)
        rasCost[is.na(rasSa)] <- NA
        rasPatch[is.na(rasSa)] <- NA
    }
    else {
        rasSa[] <- 1
    }

    ## Check that patch raster is binary
    if (!all(unique(rasPatch[]) %in% c(TRUE,FALSE))) {
        stop("grainscape:  patch must be a binary raster (=1 for patches; =0 for non-patches).  Missing values (NA) should be set to 0.", call.=FALSE)
    }
    
    ## Check that cost raster is not equal to NA at patches
    if (sum(is.na(rasCost[rasPatch==1]) > 0)) {
        stop("grainscape:  cost raster must not contain missing values at patch cells", call.=FALSE)
    }
    
    
    ## Create outputFolder
    if (!is.null(outputFolder)) {
        if (!file.exists(outputFolder)) {
           dir.create(outputFolder) 
        }
        outputFolder <- normalizePath(outputFolder)
        keepOutput <- TRUE
    }
    else {
        outputFolder <- paste(c("gs", sample(LETTERS)[1:6]), collapse="")
        dir.create(outputFolder)
        outputFolder <- normalizePath(outputFolder)
        keepOutput <- FALSE
    }
    
    extractGraphSCN <- readLines(paste(selesPath, "/extractgraph.scn", sep=""), n=-1)
    extractGraphSEL <- readLines(paste(selesPath, "/extractgraph.sel", sep=""), n=-1)
    
    subTable <- c("XXfilterPatchXX", format(filterPatch, scientific=FALSE),
                  "XXspreadFactorXX", format(spreadFactor, scientific=FALSE),
                  "XXdoCGXX", "FALSE",
                  "XXselesFolderXX", paste("\"", gsub("/", "\\\\", selesPath), "\"", sep=""),
                  "XXcostResXX", format(res(rasCost)[1], scientific=FALSE),
                  "XXmaxCostXX", format(max(unique(rasCost[]), na.rm=TRUE),  scientific=FALSE),
                  "XXhaPerCellXX", format(prod(res(rasCost))/10000, scientific=FALSE))
                    
    subTable <- matrix(subTable, 7, 2, byrow=TRUE)
    
    for (i in 1:nrow(subTable)) {
        extractGraphSCN <- sub(subTable[i, 1], subTable[i, 2], extractGraphSCN, fixed=TRUE)
        extractGraphSEL <- sub(subTable[i, 1], subTable[i, 2], extractGraphSEL, fixed=TRUE)
    }
    writeLines(extractGraphSCN, paste(outputFolder, "/eg.scn", sep=""))
    writeLines(extractGraphSEL, paste(outputFolder, "/eg.sel", sep=""))
    
    writeRaster(rasPatch, paste(outputFolder, "/patch.asc", sep=""), format="ascii")
    writeRaster(rasCost, paste(outputFolder, "/cost.asc", sep=""), format="ascii")
    writeRaster(rasSa, paste(outputFolder, "/sa.asc", sep=""), format="ascii")
    

    ## Call SELES
    system(paste(shQuote(paste(normalizePath(selesPath), "\\seles3_4", sep="")), " -p ", shQuote(paste(outputFolder, "\\eg.scn", sep="")), sep=""), wait=TRUE)


    ## Import SELES output
    selesGraph <- suppressWarnings(try(read.table(paste(outputFolder, "\\linkstatsmpg.txt", sep=""), header=TRUE), silent=TRUE))
    
    if ((class(selesGraph) == "try-error") ||(nrow(selesGraph)==0)) {
        
        if (!keepOutput) unlink(outputFolder, recursive=TRUE)

        stop("grainscape:  SELES failed to extract MPG.  Please check that input cost, patch and sa rasters are suitable, and that filterPatch is not set too high.", call.=FALSE)
    }
    
    else {
        
        ## Establish mpg object
        mpg <- list()
        mpg$mpg <- NA
        mpg$landscapeType <- "cost"
        mpg$landscape <- rasCost
        mpg$patchId <- rasCost
        mpg$voronoi <- rasCost
        mpg$lcpLinkId <- rasCost
        mpg$lcpPerimWeight <-rasCost
        mpg$lcpPerimType <- rasCost
        #mpg$eucLinkId <- rasCost
        #mpg$eucPerimWeight <- rasCost
        mpg$mpgPlot <- rasCost
        mpg$runTime <- NA
    
        
        ## Load and force rasters into memory, storing them in mpg object
        rasTmp <- raster(paste(outputFolder, "\\patchid.asc", sep=""))
        mpg$patchId[] <- getValues(rasTmp)
        
        rasTmp <- raster(paste(outputFolder, "\\voronoi.asc", sep=""))
        mpg$voronoi[] <- getValues(rasTmp)
        
        rasTmp <- raster(paste(outputFolder, "\\linkidmpg.asc", sep=""))
        mpg$lcpLinkId[] <- getValues(rasTmp)
        
        rasTmp <- raster(paste(outputFolder, "\\linkweightmpg.asc", sep=""))
        mpg$lcpPerimWeight[] <- getValues(rasTmp)
        
        rasTmp <- raster(paste(outputFolder, "\\linktype.asc", sep=""))
        mpg$lcpPerimType[] <- getValues(rasTmp)
        
        rasTmp <- raster(paste(outputFolder, "\\linktype.asc", sep=""))
        mpg$lcpPerimType[] <- getValues(rasTmp)
        
        #rasTmp <- raster(paste(outputFolder, "\\euclinkid.asc", sep=""))
        #mpg$eucLinkId[] <- getValues(rasTmp)
       
        #rasTmp <- raster(paste(outputFolder, "\\eucperimweight.asc", sep=""))
        #mpg$eucPerimWeight[] <- getValues(rasTmp)
       
        mpg$mpgPlot <- !is.na(mpg$lcpPerimWeight)
        mpg$mpgPlot[mpg$mpgPlot==0] <- NA
        mpg$mpgPlot[rasPatch==1] <- 2
       
        ## Get additional patch information not done by SELES (code reproduced from gsPatch())
        uniquePatches <- sort(unique(mpg$voronoi[]))
    
        ## Patch edge
        patchEdge <- !is.na(mpg$patchId)
        patchEdge[patchEdge==0] <- NA
        patchEdge <- raster::edge(patchEdge, type="inner")
        patchEdge[patchEdge==0] <- NA
        patchEdge <- mask(mpg$patchId, patchEdge)
    
        ## Patch area and core area
        patchArea <- freq(mpg$patchId)
        patchArea <- patchArea[!is.na(patchArea[,1]), 2]
        patchEdgeArea <- freq(patchEdge)
        patchEdgeArea <- patchEdgeArea[!is.na(patchEdgeArea[,1]), 2]
        patch <- data.frame(name=uniquePatches, patchId=uniquePatches,
                                  patchArea=patchArea,
                                  patchEdgeArea=patchEdgeArea,
                                  coreArea=patchArea-patchEdgeArea)
    
        ## Find centroids of each patch
        cellXY <- coordinates(mpg$patchId)
        rasX <- mpg$patchId
        rasY <- rasX
        rasX[] <- cellXY[,1]
        rasY[] <- cellXY[,2]
        centroids <- cbind(zonal(rasX, mpg$patchId, fun='mean'), zonal(rasY, mpg$patchId, fun='mean')[,2])
        
        toGraphV <- cbind(patch, centroidX=centroids[,2], centroidY=centroids[,3])
        
        
        ## How to convert SELES cells to raster cell numbers
        .selesCellToRasterXY <- function(ras, loc) {
            x <- ncol(ras)
            xyFromCell(ras, cellFromRowCol(ras, x-(trunc(loc/x)), x*((loc/x) - trunc(loc/x)))+1)
            }
            
            
        startPerim <- .selesCellToRasterXY(rasCost, selesGraph[, "StartLoc"])
        endPerim <- .selesCellToRasterXY(rasCost, selesGraph[, "EndLoc"])

      
        toGraphE <- data.frame(v1=selesGraph[, "nodeId1"], v2=selesGraph[, "nodeId2"],
                               linkId=selesGraph[, "linkId"], lcpPerimWeight=selesGraph[, "Cost"],
                               eucPerimWeight=selesGraph[, "SLDist"], 
                               startPerimX=startPerim[,1], startPerimY=startPerim[,2],
                               endPerimX=endPerim[,1], endPerimY=endPerim[,2],
                               linkType=selesGraph[, "linkType"])
        mpg$mpg <- graph.data.frame(toGraphE, directed=FALSE, vertices=toGraphV)
        
        class(mpg) <- "gsMPG"
        
        mpg$runTime <- paste(signif((proc.time()-st)[3], 2), " seconds", sep="")
    
        cat("Elapsed:", mpg$runTime, "\n")
    
        if (!keepOutput) unlink(outputFolder, recursive=TRUE)

        return(mpg)
    }
    
}

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
## gsGraphDataFrame
gsGraphDataFrame <- function(gsObj) {
    if (!(class(gsObj) %in% c("gsMPG", "gsGOC", "igraph"))) {
        stop("grainscape: gsObj must be a gsMPG, gsGOC or igraph object", call.=FALSE)
    }
    
    if (class(gsObj) == "gsMPG") {
        theseGraphs <- vector("list", 1)
        theseGraphs[[1]] <- gsObj$mpg
    }
    else if (class(gsObj) == "igraph") {
        theseGraphs <- vector("list", 1)
        theseGraphs[[1]] <- gsObj
    }
    else {
        theseGraphs <- lapply(gsObj$th, function(x) x$goc)
    }
    
    results <- vector("list", length(theseGraphs))
    
    for (i in 1:length(theseGraphs)) {
        thisGraph <- theseGraphs[[i]]
        
        if (is.igraph(thisGraph))  {
            results[[i]] <- list()
            results[[i]]$v <- data.frame(sapply(list.vertex.attributes(thisGraph), function(x) get.vertex.attribute(thisGraph, x)), stringsAsFactors=FALSE)
            results[[i]]$e <- data.frame(get.edgelist(thisGraph), sapply(list.edge.attributes(thisGraph), function(x) get.edge.attribute(thisGraph, x)), stringsAsFactors=FALSE)
            edgeDfNames <- names(results[[i]]$e)
            names(results[[i]]$e) <- c("e1", "e2", edgeDfNames[3:length(edgeDfNames)])
            
            ## Clean-up storage mode structure of data.frames
            results[[i]]$e <- as.data.frame(sapply(results[[i]]$e, as.character), stringsAsFactors=FALSE)
            results[[i]]$v <- as.data.frame(sapply(results[[i]]$v, as.character), stringsAsFactors=FALSE)
            results[[i]]$e <- as.data.frame(lapply(results[[i]]$e, function(x) type.convert(x, as.is=TRUE)), stringsAsFactors=FALSE)
            results[[i]]$v <- as.data.frame(lapply(results[[i]]$v, function(x) type.convert(x, as.is=TRUE)), stringsAsFactors=FALSE)

        }
        else {
            results[[i]]$v <- NA
            results[[i]]$e <- NA
        }
    }
    
    return(results)

}
## gsMPGstitch DOES NOT WORK AT PRESENT
gsMPGstitch <- function(cost, patchid, numStrips, percentOverlap, disttype="Cost", cpu=1, outputFolder=NULL, filterPatch=NULL, spreadFactor=0, selesPath = system.file("SELES", package = "grainscape")){
  stop("gsMPGstitch is currently in revision.  It does not work at this time.  Please contact the authors for more information. ", call.=FALSE)
  rasCost <- cost
  rasPatchid <- patchid
  rasCost[] <- getValues(cost)
  rasPatchid[] <- getValues(patchid)
  
  ##########################
  # Initial error messages #
  ##########################
  if (.Platform$OS.type != "windows") {
    stop("grainscape:  this function calls an executable (SELES) compiled for Windows.\nIt will not work on your OS.", 
         call. = FALSE)
  }
  if ((class(cost) != "RasterLayer")) {
    stop("grainscape: cost raster must be of class RasterLayer", 
         call. = FALSE)
  }
  if ((class(patchid) != "RasterLayer")) {
    stop("grainscape: patchid must be a raster (patch-based model)", 
         call. = FALSE)
  }
  if (res(cost)[1] != res(cost)[2]) {
    warning(paste("grainscape:  raster cells are not square;  assuming a square cell of ", 
                  res(cost)[1], " units", sep = ""), call. = FALSE)
  }
  if ((projection(cost) != "NA") && (!grepl("UTM|utm", toupper(projection(cost))))) {
    warning("grainscape:  projection suggests that all cells may not be of equal area; Note that grainscape assumes equal area in all calculations", 
            call. = FALSE)
  }
  if (is.null(filterPatch)) {
    filterPatch <- (prod(res(rasCost))/10000) * 0.01
  }
  else {
    filterPatch <- (prod(res(rasCost))/10000) * abs(filterPatch)
  }
  
  if (numStrips >= ncol(patchid)) {
    stop("grainscape: number of strips must be less than the number of columns in the patchid raster", call. = FALSE)
  }
  
  if (percentOverlap>100 | percentOverlap<1) {
    stop("grainscape: percentOverlap must be greater than 1 and less than 100", call. = FALSE)
  }
  
  ## Create outputFolder
    if (!is.null(outputFolder)) {
        if (!file.exists(outputFolder)) {
           dir.create(outputFolder) 
        }
        outputFolder <- normalizePath(outputFolder)
        keepOutput <- TRUE
    }
    else {
        outputFolder <- paste(c("gs", sample(LETTERS)[1:6]), collapse="")
        dir.create(outputFolder)
        outputFolder <- normalizePath(outputFolder)
        keepOutput <- FALSE
    }
  
  
  #########################################################
  # Step 1. Cut landscape resistance surface into strips  #
  #########################################################
  st <- proc.time()
  
  #calculate number of columns in each strip
  strip_ncol <- round(ncol(rasPatchid)/(numStrips-(numStrips-1)*percentOverlap/100))
  
  #calculate the number of columns in the overlap
  overlap_ncol <- floor((strip_ncol*numStrips-ncol(rasPatchid))/(numStrips-1))
  
  #Translate strip numcols into distance units
  strip_width <- strip_ncol*res(rasPatchid)[1] 
  
  #set output working directory
  #setwd(outputFolder)
  #set initial x position
  x_starting <- xmin(rasPatchid)
  #crop and save each strip
  for (i in 1:numStrips) {
    #all but the last strip have a fixed width equal to strip_width
    if (i<numStrips) {
      #Set extent of cropping area
      cropped_extent <- extent(x_starting,(x_starting + strip_width),ymin(rasPatchid),ymax(rasPatchid))
      #create names for cropped maps
      strip_originalids <- paste(outputFolder, "/", "PatchID",i,".asc",sep="")
      cost_name <- paste(outputFolder, "/", "Cost",i,".asc",sep="")
      #crop maps and save output
      thisPatchStrip <- crop(rasPatchid, cropped_extent)
      writeRaster(thisPatchStrip, filename=strip_originalids, type="ascii")
      thisCostStrip <- crop(rasCost, cropped_extent)
      writeRaster(thisCostStrip, filename=cost_name, type="ascii")  
      #update starting x position
      x_starting <- x_starting + strip_width/2
    }
    #allow the last strip to have variable width (slightly smaller or larger than the strip_width)
    else if(i==numStrips){
      #Set extent of cropping area
      cropped_extent <- extent(x_starting,xmax(rasPatchid),ymin(rasPatchid),ymax(rasPatchid))
      #create names for cropped maps
      strip_originalids <- paste(outputFolder, "/", "PatchID",i,".asc",sep="")
      cost_name <- paste(outputFolder, "/", "Cost",i,".asc",sep="")
      #crop maps and save output
      thisPatchStrip <- crop(rasPatchid, cropped_extent)
      writeRaster(thisPatchStrip, filename=strip_originalids, type="ascii")
      thisCostStrip <- crop(rasCost, cropped_extent)
      writeRaster(thisCostStrip, filename=cost_name, type="ascii")
    }
  }
  cat("Adjusted strip width (original units): ", strip_width, "\n")
  cat("Adjusted strip width (number of columns): ", strip_ncol, "\n")
  cat("Adjusted percent overlap: ", 100*(overlap_ncol/strip_ncol), "\n")
  
  
  ######################################
  # Step 2. Extract MPG for each strip #
  ######################################
  
  .doMPG <- function(whichMPG, outputFolder) {
        require(grainscape)
        strip_patch <- raster(paste(outputFolder, "/", "PatchID", whichMPG,".asc", sep=""))
        strip_cost <- raster(paste(outputFolder, "/", "Cost", whichMPG,".asc",sep=""))
        MPG_name <- paste("MPG", whichMPG, sep="")
        check <- class(try(gsMPG(cost=strip_cost, patch=strip_patch >= 1, outputFolder=paste(outputFolder,"/", MPG_name, sep=""))))
  }
  
  if (cpu > 1) {
    library(parallel)
    useCpu <- min(cpu, numStrips)
    cat("Extracting MPG in each of", numStrips, "strips in parallel using", useCpu, "processors\n")    
    cl <- makeCluster(useCpu)
    check <- parLapplyLB(cl, 1:numStrips, function(x) .doMPG(x, outputFolder))
    stopCluster(cl)                     
  }
  else {
    cat("Extracting MPG in each of", numStrips, "strips in serial\n")
    check <- lapply(1:numStrips, function(x) .doMPG(x, outputFolder))
  }
  
  allCheck <- do.call(c, check)
  if (any(allCheck != "gsMPG")) {
    stop(paste("grainscape:  Failure in MPG extraction of strip(s) ", paste(which(allCheck != "gsMPG"), sep=", "), sep=""), call.=FALSE)
  }


  #######################################################################################################
  # Step 3. Re-code patch ids from MPG analysis to match original input patch ids AND renumber link ids #
  # A) Create lookup table with original patch ids and new patch ids                                    #
  # B) Use lookup table to replace new ids with original ids in MPG analysis files & output maps:       #
  #    patchid, linkid, voronoi, patchStats, linkStats                                                  #
  # C) Renumber linkIds such that the link ids are unique across all strips in linkId & linkStats       #
  #######################################################################################################
  linkId_start<-1
  for(i in 1:numStrips){
    strip_originalids <- raster(paste(outputFolder, "/PatchID",i,".asc",sep=""))
    
    #load in the patch id layer for this strip produced by MPG analysis
    MPG_name <- paste("MPG", i, sep="")
    strip_newids <- raster(paste(outputFolder,"/", MPG_name, "/patchid.asc", sep=""))
    #make NAs equal to zero to be consistent with original patch id raster
    strip_newids[is.na(strip_newids)]<-0
    
    #A) Create a lookup table to relate original patch ids to new patch ids using a contingency table
    lookup_patchids<-as.data.frame(crosstab(strip_newids, strip_originalids))
    #Keep those rows where the Freq > 0 (ie where patch ids co-occur)
    lookup_patchids<-lookup_patchids[lookup_patchids$Freq>0,]
    #Remove those rows where Var1 == Var2 (usually 0 == 0)
    if(length(which(as.character(lookup_patchids$Var1) == as.character(lookup_patchids$Var2)))>0){
      lookup_patchids<-lookup_patchids[-which(as.character(lookup_patchids$Var1) == as.character(lookup_patchids$Var2)),]
    }
    #Bind Var1 and Var2 factors into a 2-column matrix
    lookup_patchids<-cbind(as.integer(as.character(lookup_patchids$Var1)), as.integer(as.character(lookup_patchids$Var2)))
    
    #B) Use lookup table to replace new patch ids with original patch ids in MPG analysis files & output maps
    #Read in output rasters from SELES mpg analysis
    patchid<-raster(paste(outputFolder,"/", MPG_name, "/patchid.asc", sep=""))
    linkid<-raster(paste(outputFolder,"/", MPG_name, "/linkidmpg.asc", sep=""))
    voronoi<-raster(paste(outputFolder,"/", MPG_name, "/voronoi.asc", sep=""))
    
    #Stack mpg raster layers that involve the patch ids
    mpg_rasters_newid <- stack(patchid, voronoi)
    #Reclassify mpg raster layers from new patch ids to original patch ids
    mpg_rasters_originalid <- reclassify(mpg_rasters_newid, lookup_patchids)
    #Fix spatial extent (SELES mpg analysis slightly changes spatial extent for unknown reason)
    extent(mpg_rasters_originalid) <- extent(strip_originalids)
    
    #Save mpg raster layers with original patch ids
    writeRaster(raster(mpg_rasters_originalid,1), paste(outputFolder,"/", MPG_name,"/patchid_originalids.asc", sep=""), overwrite=TRUE)
    writeRaster(raster(mpg_rasters_originalid,2), paste(outputFolder,"/", MPG_name,"/voronoi_originalids.asc", sep=""), overwrite=TRUE)
    
    #Read in output text files from SELES mpg analysis
    patchStats <- read.table(paste(outputFolder,"/", MPG_name, "/PatchStatsMPG.txt", sep=""), header=TRUE)
    linkStats <- read.table(paste(outputFolder,"/", MPG_name, "/LinkStatsMPG.txt", sep=""), header=TRUE)
    
    #Reclassify mpg text files from new patch ids to original patch ids
    #PatchStatsMPG
    #Order patchStats based on linkId
    patchStats<-patchStats[order(patchStats$patchId),]
    #Replce new patch ids (note that here the patch ids are the same as the vector indices due to ordering) with original patch ids
    patchStats$patchId_original<-replace(patchStats$patchId,lookup_patchids[,1],lookup_patchids[,2])
    #Re-arrange column order and save patchStats with original ids
    patchStats<-patchStats[,c("patchId_original", "nnSLDist", "nnDist", "nnCost", "Centroid", "size", "edge", "patchId")]
    patchStats<-patchStats[order(patchStats$patchId_original),]
    write.table(patchStats, paste(outputFolder, "/", MPG_name, "/PatchStatsMPG_originalids.txt", sep=""), row.names=FALSE)
    
    #LinkStats
    #Replce new patch ids with original patch ids for both smallest and largest nodes in each link
    linkStats$nodeId1_original<-linkStats$nodeId1
    linkStats$nodeId2_original<-linkStats$nodeId2
    for(j in 1:nrow(lookup_patchids)){
      linkStats$nodeId1_original<-replace(linkStats$nodeId1_original, which(linkStats$nodeId1==lookup_patchids[j,1]), lookup_patchids[j,2])
      linkStats$nodeId2_original<-replace(linkStats$nodeId2_original, which(linkStats$nodeId2==lookup_patchids[j,1]), lookup_patchids[j,2])
    }
    #Identify smallest and largest nodes in each link
    linkStats$MINnodeId_original<-apply(cbind(linkStats$nodeId1_original,linkStats$nodeId2_original), 1, min)
    linkStats$MAXnodeId_original<-apply(cbind(linkStats$nodeId1_original,linkStats$nodeId2_original), 1, max)
    #Order linkStats based on smallest then largest nodes
    linkStats<-linkStats[order(linkStats$MINnodeId, linkStats$MAXnodeId),]
    
    #C) Renumber link ids so that link ids are not the same in different strips (SELES MPG analyses always numbers links from 1 to n)
    linkStats$linkId_renumber<-c(linkId_start:(linkId_start-1+nrow(linkStats)))
    #Re-arrange column order and save linkStats with original ids
    linkStats<-linkStats[, c("linkId_renumber", "linkId", "linkType", "MINnodeId_original", "MAXnodeId_original", "SLDist", "Dist", "Cost", "NumEdges", "NumSegments", "StartLoc", "EndLoc", "nodeId1", "nodeId2", "nodeId1_original", "nodeId2_original")]
    write.table(linkStats, paste(outputFolder, "/", MPG_name, "/LinkStatsMPG_originalids.txt", sep=""), row.names=FALSE)  
    linkId_start<-max(linkStats$linkId_renumber) + 1
    
    lookup_linkIdrenumber<-cbind(linkStats$linkId, linkStats$linkId_renumber)
    linkid<-reclassify(linkid, lookup_linkIdrenumber)
    extent(linkid)<-extent(strip_originalids)
    writeRaster(linkid, paste(outputFolder,"/", MPG_name,"/linkid_renumberids.asc", sep=""), overwrite=TRUE)
    
    rm(linkStats, patchStats, MPG_name, linkid, patchid, voronoi, strip_newids, strip_originalids, mpg_rasters_newid, mpg_rasters_originalid, j, lookup_patchids, lookup_linkIdrenumber)
  }  
  
  rm(i, linkId_start)
  
  ############################################################################################
  # Step 4. Stitch together strip MPG's into a single large MPG                              #
  # Read in the voronoi tessellations of a number of map strips                              #
  # Merges the MPGLink text files and the link MPG raster maps based on the Voronoi polygons #
  # This involves a series of deletions based on link redundancies and edge effects          #
  # Output a merged MPGLink text file and MPGLink rasters                                    #
  ############################################################################################
  for(i in 1:(numStrips-1)){
    strip1<-i
    strip2<-i+1
    
    #Load the voronoi layer, linkid, patchStats, and linkStats with original patch ids for strip 1
    #Strip 1
    MPG_name<-paste("MPG", strip1, sep="")
    voronoi1<-raster(paste(outputFolder,"/", MPG_name, "/voronoi_originalids.asc", sep=""))
    if(i==1){
      linkid1<-raster(paste(outputFolder,"/", MPG_name, "/linkid_renumberids.asc", sep=""))
      linkStats1<-read.table(paste(outputFolder,"/", MPG_name, "/LinkStatsMPG_originalids.txt", sep=""),header=TRUE)
    }
    patchStats1<-read.table(paste(outputFolder,"/", MPG_name, "/PatchStatsMPG_originalids.txt", sep=""), header=TRUE)
    #Strip 2
    MPG_name<-paste("MPG", strip2, sep="")
    voronoi2<-raster(paste(outputFolder,"/", MPG_name, "/voronoi_originalids.asc", sep=""))
    linkid2<-raster(paste(outputFolder,"/", MPG_name, "/linkid_renumberids.asc", sep=""))
    linkStats2<-read.table(paste(outputFolder,"/", MPG_name, "/LinkStatsMPG_originalids.txt", sep=""),header=TRUE)
    patchStats2<-read.table(paste(outputFolder,"/", MPG_name, "/PatchStatsMPG_originalids.txt", sep=""),header=TRUE)
    
    #Delete any links that are loops (ie from a patch to itself) which were caused by cutting the study area into strips
    #Strip 1
    linkid1_loops<-linkStats1[(linkStats1$MINnodeId_original==linkStats1$MAXnodeId_original),"linkId_renumber"]
    linkStats1_deleted<-linkStats1[!(linkStats1$MINnodeId_original==linkStats1$MAXnodeId_original),]
    if(length(linkid1_loops)>0){
      lookup_linkid1loops<-cbind(linkid1_loops, NA)
      linkid1_deleted<-reclassify(linkid1, lookup_linkid1loops)
    } else
      if(length(linkid1_loops)==0){
        linkid1_deleted<-linkid1
      }
    #Strip 2
    linkid2_loops<-linkStats2[(linkStats2$MINnodeId_original==linkStats2$MAXnodeId_original),"linkId_renumber"]
    linkStats2_deleted<-linkStats2[!(linkStats2$MINnodeId_original==linkStats2$MAXnodeId_original),]
    if(length(linkid2_loops)>0){
      lookup_linkid2loops<-cbind(linkid2_loops, NA)
      linkid2_deleted<-reclassify(linkid2, lookup_linkid2loops)
    }else
      if(length(linkid2_loops)==0){
        linkid2_deleted<-linkid2
      }
    
    #Delete any duplicate links (ie that connect the same two patches) from both linkStats and linkid which were caused by cutting the study area into strips
    #Delete all duplicates and keep only the link with the minimum distance
    #Strip 1
    linkStats1_deleted<-linkStats1_deleted[order(linkStats1_deleted[,disttype]),]
    linkid1_duplicates<-subset(linkStats1_deleted$linkId_renumber, duplicated(linkStats1_deleted[,c("MINnodeId_original", "MAXnodeId_original")]))  
    linkStats1_deleted<-linkStats1_deleted[!duplicated(linkStats1_deleted[,c("MINnodeId_original", "MAXnodeId_original")]),]  
    linkStats1_deleted<-linkStats1_deleted[order(linkStats1_deleted$MINnodeId_original, linkStats1_deleted$MAXnodeId_original),]
    if(length(linkid1_duplicates)>0){
      lookup_linkid1duplicates<-cbind(linkid1_duplicates, NA)
      linkid1_deleted<-reclassify(linkid1_deleted, lookup_linkid1duplicates)
    }
    #Strip 2
    linkStats2_deleted<-linkStats2_deleted[order(linkStats2_deleted[,disttype]),]
    linkid2_duplicates<-subset(linkStats2_deleted$linkId_renumber, duplicated(linkStats2_deleted[,c("MINnodeId_original", "MAXnodeId_original")]))  
    linkStats2_deleted<-linkStats2_deleted[!duplicated(linkStats2_deleted[,c("MINnodeId_original", "MAXnodeId_original")]),]  
    linkStats2_deleted<-linkStats2_deleted[order(linkStats2_deleted$MINnodeId_original, linkStats2_deleted$MAXnodeId_original),]
    if(length(linkid2_duplicates)>0){
      lookup_linkid2duplicates<-cbind(linkid2_duplicates, NA)
      linkid2_deleted<-reclassify(linkid2_deleted, lookup_linkid2duplicates)
    }
    
    #Crop voronoi rasters to isolate the area of overlap
    x_shift<-(xmax(voronoi1)-xmin(voronoi1))/2
    #Strip 1
    crop_extent1<-extent(xmin(voronoi1)+x_shift,xmax(voronoi1),ymin(voronoi1),ymax(voronoi1))
    voronoi1_overlap<-crop(voronoi1,crop_extent1)
    #Strip 2
    crop_extent2<-extent(xmin(voronoi2),xmin(voronoi2)+x_shift,ymin(voronoi2),ymax(voronoi2))
    voronoi2_overlap<-crop(voronoi2,crop_extent2)
    
    #Make a difference map of the two cropped voronoi layers
    voronoi_difference<-abs(voronoi1_overlap-voronoi2_overlap)
    
    #If the two voronoi maps are identical then the difference map will be zero throughout and no links need to be deleted from either linkStats files or linkdid rasters
    if(cellStats(voronoi_difference,max)==0){
      linkStats1_deleted<-linkStats1_deleted
      linkStats2_deleted<-linkStats2_deleted
      linkid1_deleted<-linkid1_deleted
      linkid2_deleted<-linkid2_deleted
    } else
      
      #Otherwise, if the two voronoi maps are not identical, identify the voronois that differ and the links to be deleted
      if(cellStats(voronoi_difference,max)>0){
        #Create a map of voronoi differences for the right-hand side (RHS) of the overlap region
        #Discepancies between the voronoi maps on the RHS can be attributed to edge effects in strip 1
        x_shift<-(xmax(voronoi1_overlap)-xmin(voronoi1_overlap))/2
        crop_splitoverlap<-extent(xmin(voronoi1_overlap)+x_shift,xmax(voronoi1_overlap),ymin(voronoi1_overlap),ymax(voronoi1_overlap))
        voronoi_difference_RHS<-crop(voronoi_difference,crop_splitoverlap)
        voronoi1_overlap_RHS<-crop(voronoi1_overlap,crop_splitoverlap)
        voronoi2_overlap_RHS<-crop(voronoi2_overlap,crop_splitoverlap)
        
        #Create a map of voronoi differences for the left-hand side (LHS) of the overlap region
        #Discepancies between the voronoi maps on the LHS can be attributed to edge effects in strip 2
        crop_splitoverlap<-extent(xmin(voronoi1_overlap),xmax(voronoi1_overlap)-x_shift,ymin(voronoi1_overlap),ymax(voronoi1_overlap))
        voronoi_difference_LHS<-crop(voronoi_difference,crop_splitoverlap)
        voronoi1_overlap_LHS<-crop(voronoi1_overlap,crop_splitoverlap)
        voronoi2_overlap_LHS<-crop(voronoi2_overlap,crop_splitoverlap)
        
        #Mask voronoi1_overlap_RHS with voronoi_difference_RHS to keep only those voronoi polygons from strip 1 on the RHS that are different
        voronoi_difference_RHS[voronoi_difference_RHS==0]<-NA
        voronoi1_difference_RHS<-mask(voronoi1_overlap_RHS, voronoi_difference_RHS)
        voronoi2_difference_RHS<-mask(voronoi2_overlap_RHS, voronoi_difference_RHS)
        
        #Idenify patch ids in strip 1 and strip 2 that differ on the RHS
        patchid1_deletions<-unique(c(unique(voronoi1_difference_RHS[]), unique(voronoi2_difference_RHS[])))
        
        #Mask voronoi1_overlap_RHS with voronoi_difference_RHS to keep only those voronoi polygons from strip 1 on the LHS that are different
        voronoi_difference_LHS[voronoi_difference_LHS==0]<-NA
        voronoi1_difference_LHS<-mask(voronoi1_overlap_LHS, voronoi_difference_LHS)
        voronoi2_difference_LHS<-mask(voronoi2_overlap_LHS, voronoi_difference_LHS)
        
        #Idenify patch ids in strip 1 and strip 2 that differ on the LHS
        patchid2_deletions<-unique(c(unique(voronoi1_difference_LHS[]), unique(voronoi2_difference_LHS[])))
        
        if(i==1){
          voronoi1_keep<-unique(voronoi1_difference_LHS[])
          voronoi2_keep<-unique(voronoi2_difference_RHS[])[!(unique(voronoi2_difference_RHS[]) %in% voronoi1_keep)]
          cat("i=1 ", voronoi1_keep, "\n")
          cat("i=1 ", voronoi2_keep, "\n")
          
          voronoi_merged<-voronoi1
          
          if(length(voronoi1_keep)>0){
            for(k in 1:length(voronoi1_keep)){
              voronoi1_isolate<-match(voronoi1[], voronoi1_keep[k])*voronoi1_keep[k]
              voronoi_merged<-merge(voronoi_merged, voronoi1_isolate)  
            }
          }
          if(length(voronoi2_keep)>0){
            for(l in 1:length(voronoi2_keep)){
              voronoi2_isolate<-match(voronoi2[], voronoi2_keep[l])*voronoi2_keep[l]
              voronoi_merged<-merge(voronoi2_isolate, voronoi_merged)
            }
          }
          voronoi3_keep<-unique(c(unique(voronoi1_keep), unique(voronoi2_keep)))  
        }
        if(i>1){
          voronoi1_keep<-unique(voronoi1_difference_LHS[])[!(unique(voronoi1_difference_LHS[]) %in% voronoi3_keep)]
          voronoi2_keep<-unique(voronoi2_difference_RHS[])[!(unique(voronoi2_difference_RHS[]) %in% voronoi3_keep)]
          cat("i>1 ", voronoi1_keep, "\n")
          cat("i>1 ", voronoi2_keep, "\n")
          
          if(length(voronoi1_keep)>0){
            for(k in 1:length(voronoi1_keep)){
              voronoi1_isolate<-match(voronoi1[], voronoi1_keep[k])*voronoi1_keep[k]
              voronoi_merged<-merge(voronoi_merged, voronoi1_isolate)  
            }
          }
          if(length(voronoi2_keep)>0){
            for(l in 1:length(voronoi2_keep)){
              voronoi2_isolate<-match(voronoi2[], voronoi2_keep[l])*voronoi2_keep[l]
              voronoi_merged<-merge(voronoi2_isolate, voronoi_merged)
            }
          }
          voronoi3_keep<-unique(c(unique(voronoi1_keep[]), unique(voronoi2_keep[])))  
          
        }
        if(i==(numStrips-1)){
          voronoi_merged<-merge(voronoi_merged, voronoi2)
        }
        
        ########################################
        #   Links to be deleted from strip 1   #
        ########################################
        #link ids are identified to be deleted if: 
        #1) either end node corresponds to a patch id in patchid1_deletions   
        linkid1_deletions<-linkStats1_deleted[((linkStats1_deleted$MINnodeId_original %in% patchid1_deletions)==TRUE | (linkStats1_deleted$MAXnodeId_original %in% patchid1_deletions)==TRUE),c("linkId_renumber","MINnodeId_original","MAXnodeId_original")]
        #2) both end nodes are in strip 2
        linkid1_deletions<-linkid1_deletions[((linkid1_deletions$MAXnodeId_original %in% patchStats2$patchId_original) & (linkid1_deletions$MINnodeId_original %in% patchStats2$patchId_original)),]
        #3) both end nodes are not in patchid2_deletions
        linkid1_deletions<-linkid1_deletions[((linkid1_deletions$MINnodeId_original %in% patchid2_deletions)==FALSE | (linkid1_deletions$MAXnodeId_original %in% patchid2_deletions)==FALSE),c("linkId_renumber","MINnodeId_original","MAXnodeId_original")]
        
        linkid1_deletions<-subset(linkid1_deletions, !duplicated(linkid1_deletions[,c("MINnodeId_original", "MAXnodeId_original")]))
        
        #Delete links from linkStats1 identified above
        linkStats1_deleted<-subset(linkStats1_deleted, !(linkStats1_deleted$linkId_renumber %in% linkid1_deletions$linkId_renumber))
        
        #Delete links from linkid1 identified above
        lookup_linkid1deletions<-cbind(linkid1_deletions$linkId_renumber, NA)
        linkid1_deleted<-reclassify(linkid1_deleted, lookup_linkid1deletions)
        
        ########################################
        #   Links to be deleted from strip 2   #
        ########################################      
        #link ids are identified to be deleted if:
        #1) either end node corresponds to a patch id in patchid2_deletions
        linkid2_deletions<-linkStats2_deleted[((linkStats2_deleted$MINnodeId_original %in% patchid2_deletions)==TRUE | (linkStats2_deleted$MAXnodeId_original %in% patchid2_deletions)==TRUE),c("linkId_renumber","MINnodeId_original","MAXnodeId_original")]
        #2) both end nodes are in strip 1
        linkid2_deletions<-linkid2_deletions[((linkid2_deletions$MAXnodeId_original %in% patchStats1$patchId_original) & (linkid2_deletions$MINnodeId_original %in% patchStats1$patchId_original)),]
        #3) both end nodes are not in patchid1_deletions
        linkid2_deletions<-linkid2_deletions[((linkid2_deletions$MINnodeId_original %in% patchid1_deletions)==FALSE | (linkid2_deletions$MAXnodeId_original %in% patchid1_deletions)==FALSE),c("linkId_renumber","MINnodeId_original","MAXnodeId_original")]
        
        linkid2_deletions<-subset(linkid2_deletions, !duplicated(linkid2_deletions[,c("MINnodeId_original", "MAXnodeId_original")]))
        #delete duplicate links between strip 1 and strip 2
        alldeletions<-rbind(linkid1_deletions, linkid2_deletions)
        duplicate_deletions<-subset(alldeletions, duplicated(alldeletions[,c("MINnodeId_original", "MAXnodeId_original")]))
        linkid2_deletions<-linkid2_deletions[!(linkid2_deletions[,"linkId_renumber"] %in% duplicate_deletions[,"linkId_renumber"]),]
        
        #Delete links from linkStats2 identified above
        linkStats2_deleted<-subset(linkStats2_deleted, !(linkStats2_deleted$linkId_renumber %in% linkid2_deletions$linkId_renumber))
        
        #Delete links from linkid2 identified above
        lookup_linkid2deletions<-cbind(linkid2_deletions$linkId_renumber, NA)
        linkid2_deleted<-reclassify(linkid2_deleted, lookup_linkid2deletions)
        
        ##########################################
        #   Merge linkStats and linkid rasters   #
        ##########################################
        #Merge linkStats1_deleted and linkStats2_deleted
        linkStats_merged<-rbind(linkStats1_deleted, linkStats2_deleted)
        
        #Delete duplicates caused by merging links
        #Sort by disttype first to delete all duplicates and keep only the link with the minimum distance
        linkStats_merged<-linkStats_merged[order(linkStats_merged[,disttype]),]
        duplicatesToDelete<-linkStats_merged[duplicated(linkStats_merged[,c("MINnodeId_original", "MAXnodeId_original")]),]
        linkStats_merged<-linkStats_merged[!linkStats_merged$linkId_renumber %in% duplicatesToDelete$linkId_renumber,]
        linkStats1<-linkStats_merged
        
        #Merge linkid1_deleted and linkid2_deleted
        linkid_merged<-merge(linkid1_deleted, linkid2_deleted)
        
        #Delete links from linkid_merged identified above
        lookup_linkidmergeddeletions<-cbind(duplicatesToDelete$linkId_renumber, NA)
        linkid_merged<-reclassify(linkid_merged, lookup_linkidmergeddeletions)
        
        linkid1<-linkid_merged
      }      
  }    
  
  #Order linkStats_merged based on smallest then largest nodes
  linkStats_merged<-linkStats_merged[order(linkStats_merged$MINnodeId, linkStats_merged$MAXnodeId),]
  linkStats_merged$linkId_final<-c(1:nrow(linkStats_merged))
  #Re-arrange column order and save linkStats_merged
  linkStats_merged<-linkStats_merged[, c("linkId_final", "linkType", "MINnodeId_original", "MAXnodeId_original", "SLDist", "Dist", "Cost", "NumEdges", "NumSegments", "StartLoc", "EndLoc", "nodeId1", "nodeId2", "nodeId1_original", "nodeId2_original", "linkId", "linkId_renumber")]
  write.table(linkStats_merged, file=paste(outputFolder, "/LinkStatsMPG_merged.txt", sep=""), row.names=FALSE)
  
  writeRaster(voronoi_merged, filename=paste(outputFolder, "/voronoi_merged.asc", sep=""), type="ascii")
  
  lookup_linkIdfinal<-cbind(linkStats_merged$linkId_renumber, linkStats_merged$linkId_final)
  linkid_merged<-reclassify(linkid_merged, lookup_linkIdfinal)
  writeRaster(linkid_merged, filename=paste(outputFolder, "/linkidmpg_merged.asc", sep=""), type="ascii")
  
  lookup_linklengthfinal<-cbind(linkStats_merged$linkId_final, linkStats_merged$Dist)
  linklength_merged<-reclassify(linkid_merged, lookup_linklengthfinal)
  writeRaster(linklength_merged, filename=paste(outputFolder, "/linklengthmpg_merged.asc", sep=""), type="ascii")      
  
  lookup_linkcostfinal<-cbind(linkStats_merged$linkId_final, linkStats_merged$Cost)
  linkcost_merged<-reclassify(linkid_merged, lookup_linkcostfinal)
  writeRaster(linkcost_merged, filename=paste(outputFolder, "/linkcostmpg_merged.asc", sep=""), type="ascii")      
  
  mpgstitch<-list()
  
  mpgstitch$mpg<-NA
  mpgstitch$patchId<-rasPatchid
  mpgstitch$patchId[mpgstitch$patchId==0]<-NA
  mpgstitch$voronoi<-voronoi_merged
  mpgstitch$lcpLinkId<-linkid_merged
  mpgstitch$lcpPerimWeight<-linkcost_merged
  mpgstitch$lcpPerimLength<-linklength_merged
  mpgstitch$mpgPlot<-rasCost
  mpgstitch$runTime<-NA     
  
  mpgstitch$mpgPlot<-!is.na(mpgstitch$lcpPerimWeight)
  mpgstitch$mpgPlot[mpgstitch$mpgPlot == 0]<-NA
  mpgstitch$mpgPlot[mpgstitch$patchId >= 1]<-2
  uniquePatches<-unique(mpgstitch$patchId[])[!is.na(unique(mpgstitch$patchId[]))]
  patchEdge<-!is.na(mpgstitch$patchId)
  patchEdge[patchEdge == 0]<-NA
  patchEdge<-raster::edge(patchEdge, type = "inner")
  patchEdge[patchEdge == 0]<-NA
  patchEdge<-mask(mpgstitch$patchId, patchEdge)
  patchArea<-freq(mpgstitch$patchId)
  patchArea<-patchArea[!is.na(patchArea[, 1]), 2]
  patchEdgeArea<-freq(patchEdge)
  patchEdgeArea<-patchEdgeArea[!is.na(patchEdgeArea[, 1]), 2]
  patch<-data.frame(name = uniquePatches, patchId = uniquePatches, patchArea = patchArea, patchEdgeArea = patchEdgeArea, coreArea = patchArea - patchEdgeArea)
  cellXY<-coordinates(mpgstitch$patchId)
  rasX<-mpgstitch$patchId
  rasY<-rasX
  rasX[]<-cellXY[, 1]
  rasY[]<-cellXY[, 2]
  centroids<-cbind(zonal(rasX, mpgstitch$patchId, fun = "mean"), zonal(rasY, mpgstitch$patchId, fun = "mean")[, 2])
  toGraphV<-cbind(patch, centroidX = centroids[, 2], centroidY = centroids[, 3])
  .selesCellToRasterXY <- function(ras, loc) {
    x <- ncol(ras)
    xyFromCell(ras, cellFromRowCol(ras, x - (trunc(loc/x)), 
                                   x * ((loc/x) - trunc(loc/x))) + 1)
  }
  startPerim<-.selesCellToRasterXY(rasCost, linkStats_merged[, "StartLoc"])
  endPerim<-.selesCellToRasterXY(rasCost, linkStats_merged[, "EndLoc"])
  
  toGraphE <- data.frame(v1 = linkStats_merged[, "MINnodeId_original"], v2 = linkStats_merged[, "MAXnodeId_original"], linkId = linkStats_merged[, "linkId_final"], lcpPerimWeight = linkStats_merged[, "Cost"], eucPerimWeight = linkStats_merged[, "SLDist"], lcpPerimLength = linkStats_merged[, "Dist"], startPerimX = startPerim[, 1], startPerimY = startPerim[, 2], endPerimX = endPerim[, 1], endPerimY = endPerim[, 2])
  mpgstitch$mpg <- graph.data.frame(toGraphE, directed = FALSE, vertices = toGraphV)
  class(mpgstitch) <- "gsMPG"
  mpgstitch$runTime <- paste(signif((proc.time() - st)[3], 2)," seconds", sep = "")
  cat("Elapsed:", mpgstitch$runTime, "\n")
  
  if (!keepOutput) unlink(outputFolder, recursive=TRUE)

  return(mpgstitch)                                                                             
}