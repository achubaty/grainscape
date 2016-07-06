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
  if (!is.na(projection(cost)) && (!grepl("UTM|utm", toupper(projection(cost))))) {
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
  patchEdge<-raster::boundaries(patchEdge, type = "inner")
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
