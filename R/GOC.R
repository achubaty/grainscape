#' Produce a grains of connectivity model at multiple scales (patch-based or lattice GOC)
#'
#' @description
#' Produce a grains of connectivity (GOC) model at multiple scales (resistance thresholds)
#' by scalar analysis.
#' Patch-based or lattice GOC modelling can be done with this function.
#'
#' @param x         A \code{mpg} object produced by \code{\link{MPG}}.
#'                  For lattice GOC \code{MPG} must be run with patch set as an integer value.
#'
#' @param nThresh   Optional. An integer giving the number of thresholds (or scales)
#'                  at which to create GOC models. Thresholds are selected to produce
#'                  a maximum number of unique grains (i.e., models).
#'                  \code{nThresh} thresholds are also approximately evenly spread
#'                  between 0 and the threshold at which all patches or focal points
#'                  on the landscape are connected. This is a simple way to get
#'                  a representative subset of all possible GOC models.
#'                  Provide either \code{nThresh} or \code{doThresh} not both.
#'
#' @param doThresh  Optional. A vector giving the link thresholds at which to create GOC models.
#'                  Use \code{\link{threshold}} to identify thresholds of interest.
#'                  Provide either \code{nThresh} or \code{doThresh} not both.
#'
#' @param weight    A string giving the link weight or attribute to use for threshold.
#'                  \code{"lcpPerimWeight"} uses the accumulated resistance or least-cost
#'                  path distance from the perimeters of patches as the link weight.
#'                  \code{"eucPerimWeight"} use the Euclidean distance from the
#'                  perimeters of patches as the link weight.
#'
#'
#' @param verbose Set \code{verbose=0} for no progress information to console.
#'
#' @param ...     Additional arguments (not used).
#'
#' @details
#' Grain or scalar analysis of connectivity may be appropriate for a variety of purposes, not
#' limited to visualization and improving connectivity estimates for highly-mobile organisms.
#' See Galpern \emph{et al.} (2012), Galpern & Manseau (2013a, 2013b) for applications
#' and review of these capabilities.
#'
#' @return  A \code{\link[=goc-class]{goc}} object.
#'
#' @note Researchers should consider whether the use of a patch-based GOC or a lattice
#' GOC model is appropriate based on the patch-dependency of the organism under study.
#' Patch-based models make most sense when animals are restricted to, or dependent on,
#' a resource patch.
#' Lattice models can be used as a generalized and functional approach to scaling
#' resistance surfaces.
#'
#' See \code{\link{MPG}} for warning related to areal measurements.
#'
#' @references
#' Fall, A., M.-J. Fortin, M. Manseau, D. O'Brien. (2007) Spatial graphs:
#' Principles and applications for habitat connectivity. Ecosystems 10:448:461.
#'
#' Galpern, P., M. Manseau. (2013a) Finding the functional grain: comparing methods
#' for scaling resistance surfaces. Landscape Ecology 28:1269-1291.
#'
#' Galpern, P., M. Manseau. (2013b) Modelling the influence of landscape connectivity
#' on animal distribution: a functional grain approach. Ecography 36:1004-1016.
#'
#' Galpern, P., M. Manseau, A. Fall. (2011) Patch-based graphs of landscape connectivity:
#' a guide to construction, analysis, and application for conservation.
#' Biological Conservation 144:44-55.
#'
#' Galpern, P., M. Manseau, P.J. Wilson. (2012) Grains of connectivity: analysis
#' at multiple spatial scales in landscape genetics. Molecular Ecology 21:3996-4009.
#'
#'
#' @author Paul Galpern
#' @export
#' @importFrom raster freq rasterToPolygons reclassify zonal
#' @importFrom sp coordinates
#' @importFrom stats median
#' @include classes.R
#' @rdname GOC
#' @seealso \code{\link{MPG}}, \code{\link{grain}},
#'          \code{\link{distance}}, \code{\link{point}}
#'
#' @example inst/examples/example_preamble.R
#' @example inst/examples/example_preamble_MPG.R
#' @example inst/examples/example_preamble_GOC.R
#' @example inst/examples/example_GOC.R
#'
setGeneric("GOC", function(x, ...) {
  standardGeneric("GOC")
})

#' @export
#' @rdname GOC
setMethod(
  "GOC",
  signature = "mpg",
  definition = function(x, nThresh = NULL, doThresh = NULL,
                        weight = "lcpPerimWeight", verbose = 0, ...) {
    dots <- list(...)
    if (!is.null(dots$sp))
      warning("Argument 'sp' is deprecated and will be ignored.")

    baseGraph <- x@mpg

    linkWeight <- try(edge_attr(baseGraph, weight), silent = TRUE)

    if (inherits(linkWeight, "try-error")) {
      stop("weight must be the name of an existing link attribute to threshold",
           " (e.g., 'lcpPerimWeight')")
    }

    if (is.null(nThresh) && is.null(doThresh)) {
      stop("either nThresh or doThresh must be specified.")
    } else if (!is.null(nThresh) && !is.null(doThresh)) {
      stop("only one of nThresh or doThresh must be specified.")
    } else if (is.null(doThresh)) {
      ## Determine nThresh unique thresholds covering the full range of possibilities
      ## in terms of the number of polygons
      allUniqueThresh <- t(sapply(sort(c(0, unique(linkWeight))), function(z) {
        cbind(z, components(delete_edges(x@mpg, which(linkWeight > z)))$no)
      }))
      doThresh <- allUniqueThresh[!duplicated(allUniqueThresh[, 2]), 1]
      doThresh <- doThresh[round(seq(1, length(doThresh), length = nThresh))]
      ids <- 1:length(doThresh)
    } else {
      ids <- doThresh
    }

    voronoi <- x@voronoi

    summary.df <- data.frame(id = ids, maxLink = doThresh, stringsAsFactors = FALSE)

    allLinks <- ends(baseGraph, E(baseGraph))

    ## Report on orphaned patches in the MPG
    id <- sapply(V(baseGraph)$name, function(z) sum(allLinks == as.integer(z)))
    unlinkedPatches <- as.integer(V(baseGraph)$name[which(id == 0)])
    if (length(unlinkedPatches) > 0) {
      for (iPatch in unlinkedPatches) {
        warning("patchId=", iPatch, " has no connecting links in the MPG.",
                " This may be caused by a patch surrounded in missing values (NA cells).\n")
        baseGraph <- delete_vertices(baseGraph, as.character(iPatch))
      }
    }

    linkId <- edge_attr(baseGraph, "linkId")
    cellXY <- coordinates(voronoi)
    th <- vector("list", length(doThresh))

    for (iThresh in 1:length(doThresh)) {
      if (verbose >= 1) message("Threshold ", iThresh, " of ", length(doThresh))
      tGraph <- delete_edges(baseGraph, which(linkWeight > doThresh[iThresh]))

      ## Determine the component structure of the threshold graph
      componentList <- components(tGraph)

      ## Determine if there is more than one component in the graph (if not, return NA)
      if (componentList$no > 1) {
        components <- componentList$membership

        ## Determine which edges have endpoints in different components,
        ## and create a lookup data frame
        linkComponentLookup <- cbind(linkId, edge_attr(baseGraph, weight), allLinks,
                                     t(apply(allLinks, 1, function(z) {
                                       c(components[z[1]], components[z[2]])
                               }))) %>%
          apply(., 2, as.numeric) %>%
          as.data.frame(stringsAsFactors = FALSE)
        linkComponentLookup <- linkComponentLookup[linkComponentLookup[, 5] !=
                                                     linkComponentLookup[, 6], ]
        colnames(linkComponentLookup) <- c("linkId", "linkWeight", "node1", "node2",
                                           "compNode1", "compNode2")

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
              sameLink <- (linkComponentLookup[, "compNode1"] == c1) & # nolint
                (linkComponentLookup[, "compNode2"] == c2) |
                ((linkComponentLookup[, "compNode1"] == c2) & # nolint
                   (linkComponentLookup[, "compNode2"] == c1)) # nolint
              linkComponentLookup[sameLink, "compLinkId"] <- paste(c1, c2, sep = "_")
              done[sameLink] <- TRUE
            }
          }

          ## Build data.frame for component graph
          ## Find maximum, minimum, mean, and median edge weights between components
          lCLid <- linkComponentLookup[, "compLinkId"]

          maxWeight <- as.vector(sapply(unique(lCLid), function(z) {
            ids <- linkComponentLookup$compLinkId == z
            max(linkComponentLookup[ids, "linkWeight"])
          }))
          linkIdMaxWeight <- as.vector(sapply(unique(lCLid), function(z) {
            ids <- linkComponentLookup$compLinkId == z
            linkComponentLookup[ids, "linkId"][
              which.max(linkComponentLookup[ids, "linkWeight"])]
          }))
          minWeight <- as.vector(sapply(unique(lCLid), function(z) {
            ids <- linkComponentLookup$compLinkId == z
            min(linkComponentLookup[ids, "linkWeight"])
          }))
          linkIdMinWeight <- as.vector(sapply(unique(lCLid), function(z) {
            ids <- linkComponentLookup$compLinkId == z
            linkComponentLookup[ids, "linkId"][
              which.min(linkComponentLookup[ids, "linkWeight"])]
          }))
          medianWeight <- as.vector(sapply(unique(lCLid), function(z) {
            ids <- linkComponentLookup$compLinkId == z
            median(linkComponentLookup[ids, "linkWeight"])
          }))
          meanWeight <- as.vector(sapply(unique(lCLid), function(z) {
            ids <- linkComponentLookup$compLinkId == z
            mean(linkComponentLookup[ids, "linkWeight"])
          }))
          numEdgesWeight <- as.vector(sapply(unique(lCLid), function(z) {
            ids <- linkComponentLookup$compLinkId == z
            sum(ids)
          }))

          ## Get all linkIds between components and add them as a comma-delimited list
          linkIdAll <- as.character(sapply(unique(lCLid), function(z) {
            ids <- linkComponentLookup$compLinkId == z
            paste(linkComponentLookup[ids, "linkId"], collapse = ", ")
          }))

          ## Convert back from string representation of component linkIds to numeric
          componentGraphNodes <- unique(linkComponentLookup$compLinkId) %>%
            strsplit(., "_") %>%
            do.call(rbind, .)

          ## Produce component graph with all edge attributes, and vertex attributeas.characters
          ## containing a comma-delimited string of vertex names
          componentGraph <- data.frame(componentGraphNodes, maxWeight, linkIdMaxWeight,
                                       minWeight, linkIdMinWeight, medianWeight,
                                       meanWeight, numEdgesWeight, linkIdAll) %>%
            graph_from_data_frame(directed = FALSE)

          V(componentGraph)$polygonId <- V(componentGraph)$name
          sourcePatchId <- as.character(V(componentGraph)$polygonId) %>%
            as.numeric() %>%
            sapply(., function(z) {
              paste(as.character(V(baseGraph)$patchId[components == z]), collapse = ", ")
            })

          ## Produce a raster representing this grain of connectivity
          gocRaster <- voronoi

          rawreclassifyVor <- cbind(sourcePatchId, V(componentGraph)$polygonId)
          reclassifyVor <- matrix(0, 1, 2)
          for (j in 1:nrow(rawreclassifyVor)) {
            reclassifyVor <- rbind(
              reclassifyVor,
              cbind(as.integer(strsplit(rawreclassifyVor[j, 1], ", ")[[1]]),
                    as.integer(rawreclassifyVor[j, 2]))
            )
          }
          reclassifyVor <- reclassifyVor[2:nrow(reclassifyVor), ]

          gocRaster <- reclassify(gocRaster, rcl = reclassifyVor)

          ## Find centroids of each polygon and add as vertex attributes
          uniquePolygons <- V(componentGraph)$polygonId

          rasX <- gocRaster
          rasY <- rasX
          rasX[] <- cellXY[, 1]
          rasY[] <- cellXY[, 2]

          centroids <- cbind(zonal(rasX, gocRaster, fun = "mean"),
                             zonal(rasY, gocRaster, fun = "mean")[, 2])
          centroids <- centroids[centroids[, 1] %in% as.integer(uniquePolygons), ]
          row.names(centroids) <- centroids[, 1]
          centroids <- centroids[uniquePolygons, 2:3]


          V(componentGraph)$centroidX <- centroids[, 1]
          V(componentGraph)$centroidY <- centroids[, 2]

          ## Find areas of each polygon and add as a vertex attribute
          polygonArea <- freq(gocRaster)
          row.names(polygonArea) <- polygonArea[, 1]
          polygonArea <- polygonArea[uniquePolygons, 2]

          V(componentGraph)$polygonArea <- polygonArea

          ## Find the total patch area, total patch edge area, and total core area
          ##   in each polygon and add as vertex attributes.
          patchAreaLookup <- cbind(V(baseGraph)$patchId,
                                   V(baseGraph)$patchArea,
                                   V(baseGraph)$patchEdgeArea,
                                   V(baseGraph)$coreArea)

          V(componentGraph)$totalPatchArea <- sapply(sourcePatchId, function(z) {
            sum(patchAreaLookup[patchAreaLookup[, 1] %in% as.numeric(strsplit(z, ", ")[[1]]), 2])
          }) %>%
            unlist() %>%
            as.numeric()

          V(componentGraph)$totalPatchEdgeArea <- sapply(sourcePatchId, function(z) {
            sum(patchAreaLookup[patchAreaLookup[, 1] %in% as.numeric(strsplit(z, ", ")[[1]]), 3])
          }) %>%
            unlist() %>%
            as.numeric()

          V(componentGraph)$totalCoreArea <- sapply(sourcePatchId, function(z) {
            sum(patchAreaLookup[patchAreaLookup[, 1] %in% as.numeric(strsplit(z, ", ")[[1]]), 4])
          }) %>%
            unlist() %>%
            as.numeric()

          V(componentGraph)$patchId <- sourcePatchId

          ## Find distances between each polygon centroid
          eucCentroidWeight <- apply(as_edgelist(componentGraph), 1, function(z) {
            x1 <- which(uniquePolygons == z[1])
            x2 <- which(uniquePolygons == z[2])

            out <- sqrt((centroids[x2, 1] - centroids[x1, 1]) ^ 2 + # nolint
                          (centroids[x2, 2] - centroids[x1, 2]) ^ 2)
            return(out)
          })
          E(componentGraph)$eucCentroidWeight <- eucCentroidWeight

          th[[iThresh]]$goc <- componentGraph
        } else {
          th[[iThresh]]$goc <- NA
        }
      }
    }

    ## Add data to the summary table
    summary.df$nPolygon <- unlist(lapply(th, function(z) {
      if (is_igraph(z$goc)) vcount(z$goc) else NA
    }))
    summary.df$maxPolygonArea <- unlist(lapply(th, function(z) {
      if (is_igraph(z$goc)) max(V(z$goc)$polygonArea) else NA
    }))
    summary.df$minPolygonArea <- unlist(lapply(th, function(z) {
      if (is_igraph(z$goc)) min(V(z$goc)$polygonArea) else NA
    }))
    summary.df$meanPolygonArea <- unlist(lapply(th, function(z) {
      if (is_igraph(z$goc)) mean(V(z$goc)$polygonArea) else NA
    }))
    summary.df$medianPolygonArea <- unlist(lapply(th, function(z) {
      if (is_igraph(z$goc)) median(V(z$goc)$polygonArea) else NA
    }))

    ## Find ECS (Expected cluster size; O'Brien et al, 2006) using totalPatchArea
    summary.df$ECS <- unlist(lapply(th, function(z) { # nolint
      if (is_igraph(z$goc)) sum(V(z$goc)$totalPatchArea ^ 2) / sum(V(z$goc)$totalPatchArea) else NA
    }))
    ## Find ECSCore (Expected cluster size; O'Brien et al, 2006) using totalCoreArea
    summary.df$ECSCore <- unlist(lapply(th, function(z) { # nolint
      if (is_igraph(z$goc)) sum(V(z$goc)$totalCoreArea ^ 2) / sum(V(z$goc)$totalCoreArea) else NA
    }))

    threshGraph <- new("goc", voronoi = voronoi,
                       summary = summary.df, th = th)

    return(threshGraph)
})
