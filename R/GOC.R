#' Produce a grains of connectivity model at multiple scales (patch-based or lattice GOC)
#'
#' @description
#' Produce a grains of connectivity (GOC) model at multiple scales (resistance thresholds)
#' by scalar analysis.
#' Patch-based or lattice GOC modelling can be done with this function.
#'
#' @param ...  Additional arguments.
#'
#' @export
#'
GOC <- function(x, ...) UseMethod("GOC")



#' @param x         A \code{mpg} object produced by \code{\link{MPG}}.
#'                  For lattice GOC \code{MPG} must be run with patch set as an integer value.
#'
#' @param nThresh   Optional.  An integer giving the number of thresholds (or scales)
#'                  at which to create GOC models.  Thresholds are selected to produce
#'                  a maximum number of unique grains (i.e., models).
#'                  \code{nThresh} thresholds are also approximately evenly spread
#'                  between 0 and the threshold at which all patches or focal points
#'                  on the landscape are connected.  This is a simple way to get
#'                  a representative subset of all possible GOC models.
#'                  Provide either \code{nThresh} or \code{doThresh} not both.
#'
#' @param doThresh  Optional.  A vector giving the link thresholds at which to create GOC models.
#'                  Use \code{link{threshold}} to identify thresholds of interest.
#'                  Provide either \code{nThresh} or \code{doThresh} not both.
#'
#' @param weight    A string giving the link weight or attribute to use for threshold.
#'                  \code{"lcpPerimWeight"} uses the accumulated resistance or least-cost
#'                  path distance from the perimeters of patches as the link weight.
#'                  \code{"eucPerimWeight"} use the Euclidean distance from the
#'                  perimeters of patches as the link weight.
#'
#' @param sp  Logical.  If \code{TRUE} the \code{rgeos} package is used to create
#'            a vector of class \code{\link{SpatialPolygonsDataFrame}} describing
#'            the finest grain of connectivity.  This is very useful for visualizing
#'            grains of connectivity models, especially for print purposes.
#'            Equally, using the \code{maptools} or \code{rgdal} packages these
#'            polygons can be exported as shapefiles for use in other GIS applications.
#'            But, please see details.
#'
#' @param verbose Set \code{verbose=0} for no progress information to console.
#'
#' @details
#' This function can take a long time to run when \code{sp = TRUE}.
#' Time taken is dependent on the dimensions of the \code{mpg$voronoi} raster.
#'
#' @return  A \code{GOC} object containing:
#'
#' \describe{
#'   \item{\code{voronoi}}{ is a raster describing the regions of proximity in
#'   resistance units around the focal patches or points (\code{RasterLayer});}
#'
#'   \item{\code{voronoiSP}}{is a vector representation of these regions of
#'   proximity (\code{SpatialPolygons}; if \code{sp = TRUE});}
#'
#'   \item{\code{summary}}{summarizes the grains of connectivity generated and
#'   their properties;}
#'
#'   \item{\code{th}}{is a list of length \code{nThresh} or \code{length(doThresh)}
#'   giving the GOC graph at each threshold.}
#' }
#'
#' Each element of \code{th} contains a \code{goc} object giving the GOC graph as class \code{\link{igraph}}.
#' Vertex attributes describes qualities of each polygon including the coordinates of each polygon centroid,
#' the area of these polygons, and the original patch IDs in the MPG that are included in each polygon.
#' All areal measurements are given as raster cell counts.
#' A variety of edge attributes are also given in the GOC graph.
#' See \code{\link{distance}} for more information.
#'
#' @note Researchers should consider whether the use of a patch-based GOC or a lattice
#' GOC model is appropriate based on the patch-dependency of the organism under study.
#' Patch-based models make most sense when animals are restricted to, or dependent on, a resource patch.
#' Lattice models can be used as a generalized and functional approach to scaling resistance surfaces.
#'
#' See \code{\link{MPG}} for warning related to areal measurements.
#'
#' @references
#' Fall, A., M.-J. Fortin, M. Manseau, D. O'Brien. (2007) Spatial graphs: Principles and applications for habitat connectivity. Ecosystems 10:448:461.
#'
#' Galpern, P., M. Manseau, P.J. Wilson. (2012) Grains of connectivity: analysis at multiple spatial scales in landscape genetics. Molecular Ecology 21:3996-4009.
#'
#' @author Paul Galpern
#' @docType methods
#' @export
#' @importFrom igraph components delete_edges E 'E<-' edge_attr ends graph_from_data_frame is_igraph V 'V<-' vcount
#' @importFrom raster freq rasterToPolygons reclassify zonal
#' @importFrom sp coordinates
#' @importFrom stats median
#' @rdname GOC
#' @seealso \code{\link{MPG}}, \code{\link{visualize}},
#'          \code{\link{distance}}, \code{\link{point}}
#'
#' @examples
#' \dontrun{
#' library(raster)
#'
#' ## Load raster landscape
#' tiny <- raster(system.file("extdata/tiny.asc", package = "grainscape"))
#'
#' ## Create a resistance surface from a raster using an is-becomes reclassifyifyification
#' tinyCost <- reclassify(tiny, rcl = cbind(c(1, 2, 3, 4), c(1, 5, 10, 12)))
#'
#' ## Produce a patch-based MPG where patches are resistance features=1
#' tinyPatchMPG <- MPG(cost = tinyCost, patch = tinyCost == 1)
#'
#' ## Extract a representative subset of 5 grains of connectivity
#' tinyPatchGOC <- GOC(tinyPatchMPG, nThresh = 5)
#'
#' ## Examine the properties of the GOC graph of grain 3 of 5
#' print(tinyPatchGOC$th[[3]]$goc, vertex = TRUE, edge = TRUE)
#'
#' ## Extract specified grains of connectivity and produce a vector SpatialPolygons
#' ## representation of the finest grain of connectivity (Threshold=0)
#' tinyPatchGOC <- GOC(tinyPatchMPG, doThresh = c(0, 20, 40), sp = TRUE)
#' }
#'
GOC.mpg <- function(x, ..., nThresh = NULL, doThresh = NULL,
                    weight = "lcpPerimWeight", sp = FALSE, verbose = 0) {
  if (isTRUE(sp) && !requireNamespace("rgeos", quietly = TRUE)) {
    stop("grainscape:  rgeos package must be installed to use sp = TRUE")
  }

  threshGraph <- vector("list")
  baseGraph <- x$mpg

  linkWeight <- try(edge_attr(baseGraph, weight), silent = TRUE)

  if (inherits(linkWeight, "try-error")) {
    stop("grainscape: weight must be the name of an existing link attribute to threshold (e.g., 'lcpPerimWeight')", call. = FALSE)
  }

  if (is.null(nThresh) && is.null(doThresh)) {
    stop("grainscape: either nThresh or doThresh must be specified", call. = FALSE)
  } else if (!is.null(nThresh) && !is.null(doThresh)) {
    stop("grainscape: only one of nThresh or doThresh must be specified", call. = FALSE)
  } else if (is.null(doThresh)) {
    ## Determine nThresh unique thresholds covering the full range of possibilities
    ## in terms of the number of polygons
    allUniqueThresh <- t(sapply(sort(c(0, unique(linkWeight))), function(z) {
      cbind(x, components(delete_edges(x$mpg, which(linkWeight > z)))$no)
    }))
    doThresh <- allUniqueThresh[!duplicated(allUniqueThresh[, 2]), 1]
    doThresh <- doThresh[round(seq(1, length(doThresh), length = nThresh))]
  }

  threshGraph$voronoi <- x$voronoi
  threshGraph$summary <- data.frame(maxLink = doThresh)

  ## Optionally retain a vectorized version of the smallest grain of connectivity
  ## that can later be used to create larger grains by aggregation
  if (sp) {
    if (verbose >= 2) message("Creating SpatialPolygons for smallest grain.")
    if (verbose >= 3) {
      message("  Time for completion is dependent on the number of patches and the dimensions of the raster.")
    }
    threshGraph$voronoiSP <- rasterToPolygons(threshGraph$voronoi, dissolve = TRUE)
  }

  allLinks <- ends(baseGraph, E(baseGraph))

  ## Check MPG for orphaned patches
  ## A workaround has not yet been implemented
  id <- sapply(V(baseGraph)$name, function(z) sum(allLinks == as.integer(z)))
  unlinkedPatches <- as.integer(V(baseGraph)$name[which(id == 0)])
  if (length(unlinkedPatches) > 0) {
    for (iPatch in unlinkedPatches) {
      warning("patchId=", iPatch, " has no connecting links in the MPG.",
              " This is likely caused by a patch surrounded in missing values (NA cells).\n",
              "At present, all patches must be linked to at least one other patch in the MPG for GOC analyses.\n",
              "Replacing NA cells in the cost or sa rasters may be required\n", call. = FALSE)
    }
    stop("grainscape:  cost, patch and/or sa rasters used to create the MPG present a limit case for GOC analyses.  Generated warnings may indicated cause.\nWorkaround for these cases has not yet been implemented.  Please contact the package author for more information.", call. = FALSE)
  }

  linkId <- edge_attr(baseGraph, "linkId")
  cellXY <- coordinates(threshGraph$voronoi)
  threshGraph$th <- vector("list", length(doThresh))

  for (iThresh in 1:length(doThresh)) {
    if (verbose >= 1) message("Threshold ", iThresh, " of ", length(doThresh))
    tGraph <- delete_edges(baseGraph, which(linkWeight > doThresh[iThresh]))

    ## Determine the component structure of the threshold graph
    componentList <- components(tGraph)

    ## Determine if there is more than one component in the graph (if not, return NA)
    if (componentList$no > 1) {
      components <- componentList$membership

      ## Determine which edges have endpoints in different components, and create a lookup data frame
      linkComponentLookup <- cbind(linkId, edge_attr(baseGraph, weight), allLinks,
                                   t(apply(allLinks, 1, function(z) {
                                     c(components[z[1]], components[z[2]])
                             }))) %>%
        apply(., 2, as.numeric) %>%
        as.data.frame(stringsAsFactors = FALSE)
      linkComponentLookup <- linkComponentLookup[linkComponentLookup[, 5] != linkComponentLookup[, 6],]
      colnames(linkComponentLookup) <- c("linkId", "linkWeight", "node1", "node2", "compNode1", "compNode2")

      ## Deal with the case when there are exactly 2 components
      if (ncol(linkComponentLookup) == 1) {
        linkComponentLookup <- data.frame(t(linkComponentLookup))
      }

      ## Exclude cases when there are patches that have no edges
      if (nrow(linkComponentLookup) > 0) {
        ## Standardize component link names (i.e., give a link from component 2 to component 1
        ## the same name as a link from component 1 to component 2)
        linkComponentLookup$compLinkId <- rep(NA_real_, nrow(linkComponentLookup))
        done <- rep(FALSE, nrow(linkComponentLookup))

        for (i in 1:nrow(linkComponentLookup)) {
          if (!done[i]) {
            c1 <- linkComponentLookup[i, "compNode1"]
            c2 <- linkComponentLookup[i, "compNode2"]
            sameLink <- (linkComponentLookup[, "compNode1"] == c1) &
              (linkComponentLookup[, "compNode2"] == c2) |
              ((linkComponentLookup[, "compNode1"] == c2) &
                 (linkComponentLookup[, "compNode2"] == c1))
            linkComponentLookup[sameLink, "compLinkId"] <- paste(c1, c2, sep = "_")
            done[sameLink] <- TRUE
          }
        }

        ## Build data.frame for component graph
        ## Find maximum, minimum, mean, and median edge weights between components
        maxWeight <- as.vector(sapply(unique(linkComponentLookup[, "compLinkId"]), function(z) {
          max(linkComponentLookup[linkComponentLookup$compLinkId == z, "linkWeight"])
        }))
        linkIdMaxWeight <- as.vector(sapply(unique(linkComponentLookup[, "compLinkId"]), function(z)
          linkComponentLookup[linkComponentLookup$compLinkId == z, "linkId"][which.max(linkComponentLookup[linkComponentLookup$compLinkId == z, "linkWeight"])]))
        minWeight <- as.vector(sapply(unique(linkComponentLookup[, "compLinkId"]), function(z)
          min(linkComponentLookup[linkComponentLookup$compLinkId == z, "linkWeight"])))
        linkIdMinWeight <- as.vector(sapply(unique(linkComponentLookup[, "compLinkId"]), function(z)
          linkComponentLookup[linkComponentLookup$compLinkId == z, "linkId"][which.min(linkComponentLookup[linkComponentLookup$compLinkId == z,"linkWeight"])]))
        medianWeight <- as.vector(sapply(unique(linkComponentLookup[, "compLinkId"]), function(z)
          median(linkComponentLookup[linkComponentLookup$compLinkId == z, "linkWeight"])))
        meanWeight <- as.vector(sapply(unique(linkComponentLookup[, "compLinkId"]), function(z)
          mean(linkComponentLookup[linkComponentLookup$compLinkId == z, "linkWeight"])))
        numEdgesWeight <- as.vector(sapply(unique(linkComponentLookup[, "compLinkId"]), function(z)
          sum(linkComponentLookup$compLinkId == z)))

        ## Get all linkIds between components and add them as a comma-delimited list
        linkIdAll <- as.character(sapply(unique(linkComponentLookup[, "compLinkId"]), function(z)
          paste(linkComponentLookup[linkComponentLookup$compLinkId == z, "linkId"], collapse = ", ")))

        ## Convert back from string representation of component linkIds to numeric
        componentGraphNodes <- do.call(rbind,strsplit(unique(linkComponentLookup$compLinkId), "_"))

        ## Produce component graph with all edge attributes, and vertex attributes containing a comma-delimited string of vertex names
        componentGraph <- data.frame(componentGraphNodes, maxWeight, linkIdMaxWeight,
                                     minWeight, linkIdMinWeight, medianWeight,
                                     meanWeight, numEdgesWeight, linkIdAll) %>%
          graph_from_data_frame(directed = FALSE)

        V(componentGraph)$polygonId <- V(componentGraph)$name
        sourcePatchId <- sapply(as.numeric(as.character(V(componentGraph)$polygonId)), function(z)
          paste(as.character(V(baseGraph)$patchId[components == z]), collapse = ", "))

        ## Produce a raster representing this grain of connectivity
        gocRaster <- threshGraph$voronoi

        rawreclassifyVor <- cbind(sourcePatchId, V(componentGraph)$polygonId)
        reclassifyVor <- matrix(0, 1, 2)
        for (j in 1:nrow(rawreclassifyVor)) {
          reclassifyVor <- rbind(reclassifyVor,
                                 cbind(as.integer(strsplit(rawreclassifyVor[j,1], ", ")[[1]]),
                                       as.integer(rawreclassifyVor[j,2])))
        }
        reclassifyVor <- reclassifyVor[2:nrow(reclassifyVor), ]

        gocRaster <- reclassify(gocRaster, rcl = reclassifyVor)

        ## Find centroids of each polygon and add as vertex attributes
        uniquePolygons <- V(componentGraph)$polygonId

        rasX <- gocRaster
        rasY <- rasX
        rasX[] <- cellXY[, 1]
        rasY[] <- cellXY[, 2]

        centroids <- cbind(zonal(rasX, gocRaster, fun = 'mean'),
                           zonal(rasY, gocRaster, fun = 'mean')[, 2])
        centroids <- centroids[centroids[, 1] >= 0, 2:3]
        centroids <- centroids[as.integer(uniquePolygons),]

        V(componentGraph)$centroidX <- centroids[, 1]
        V(componentGraph)$centroidY <- centroids[, 2]

        ## Find areas of each polygon and add as a vertex attribute
        polygonArea <- freq(gocRaster)
        polygonArea <- polygonArea[polygonArea[, 1] >= 0, 2]
        polygonArea <- polygonArea[as.integer(uniquePolygons)]

        V(componentGraph)$polygonArea <- polygonArea

        ## Find the total patch area, total patch edge area, and total core area
        ##   in each polygon and add as vertex attributes.
        patchAreaLookup <- cbind(V(baseGraph)$patchId,
                                 V(baseGraph)$patchArea.count,
                                 V(baseGraph)$patchEdgeArea.count,
                                 V(baseGraph)$coreArea.count)
        V(componentGraph)$totalPatchArea <- as.numeric(unlist(sapply(sourcePatchId, function(z)
          sum(patchAreaLookup[patchAreaLookup[, 1] %in% as.numeric(strsplit(z, ", ")[[1]]), 2]))))
        V(componentGraph)$totalPatchEdgeArea <- as.numeric(unlist(sapply(sourcePatchId, function(z)
          sum(patchAreaLookup[patchAreaLookup[, 1] %in% as.numeric(strsplit(z, ", ")[[1]]), 3]))))
        V(componentGraph)$totalCoreArea <- as.numeric(unlist(sapply(sourcePatchId, function(z)
          sum(patchAreaLookup[patchAreaLookup[, 1] %in% as.numeric(strsplit(z, ", ")[[1]]), 4]))))
        V(componentGraph)$patchId <- sourcePatchId

        ## Find distances between each polygon centroid
        eucCentroidWeight <- apply(as_edgelist(componentGraph), 1, function(z) {
          x1 <- which(uniquePolygons == z[1])
          x2 <- which(uniquePolygons == z[2])

          return(sqrt((centroids[x2, 1] - centroids[x1, 1])^2 + (centroids[x2, 2] - centroids[x1, 2])^2))
        })
        E(componentGraph)$eucCentroidWeight <- eucCentroidWeight

        threshGraph$th[[iThresh]]$goc <- componentGraph
      } else {
        threshGraph$th[[iThresh]]$goc <- NA
      }
    } else {
      threshGraph$th[[iThresh]]$goc <- NA
    }
  }

  ## Add data to the summary table
  threshGraph$summary$nPolygon <- unlist(lapply(threshGraph$th, function(z) {
    if (is_igraph(x$goc)) vcount(x$goc) else NA
  }))
  threshGraph$summary$maxPolygonArea <- unlist(lapply(threshGraph$th, function(z) {
    if (is_igraph(x$goc)) max(V(x$goc)$polygonArea) else NA
  }))
  threshGraph$summary$minPolygonArea <- unlist(lapply(threshGraph$th, function(z) {
    if (is_igraph(x$goc)) min(V(x$goc)$polygonArea) else NA
  }))
  threshGraph$summary$meanPolygonArea <- unlist(lapply(threshGraph$th, function(z) {
    if (is_igraph(x$goc)) mean(V(x$goc)$polygonArea) else NA
  }))
  threshGraph$summary$medianPolygonArea <- unlist(lapply(threshGraph$th, function(z) {
    if (is_igraph(x$goc)) median(V(x$goc)$polygonArea) else NA
  }))

  ## Find ECS (Expected cluster size; O'Brien et al, 2006) using totalPatchArea
  threshGraph$summary$ECS <- unlist(lapply(threshGraph$th, function(z) {
    if (is_igraph(x$goc)) sum(V(x$goc)$totalPatchArea^2)/sum(V(x$goc)$totalPatchArea) else NA
  }))
  ## Find ECSCore (Expected cluster size; O'Brien et al, 2006) using totalCoreArea
  threshGraph$summary$ECSCore <- unlist(lapply(threshGraph$th, function(z) {
    if (is_igraph(x$goc)) sum(V(x$goc)$totalCoreArea^2)/sum(V(x$goc)$totalCoreArea) else NA
  }))
  class(threshGraph) <- "goc"

  return(threshGraph)
}
