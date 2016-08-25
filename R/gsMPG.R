#' Extract a minimum planar graph (MPG) model from a landscape resistance surface
#'
#' @description This function is used to extract a minimum planar graph (MPG) and
#' it is also the first step in grains of connectivity (GOC) modelling.
#' Both patch-based and lattice MPGs can be extracted.
#'
#' @details Use this function to create a minimum planar graph (MPG) that can be
#' further analyzed using \code{\link{igraph}} routines.
#' It is also the first step in grains of connectivity (GOC) modelling.
#'
#' @note Researchers should consider whether the use of a patch-based MPG or a lattice
#' MPG model is appropriate based on the patch-dependency of the organism under study.
#' Patch-based models make most sense when animals are restricted to, or dependent on,
#' a resource patch.  Lattice models can be used as a generalized and functional
#' approach to scaling resistance surfaces.\cr
#' Areal measurements are given as raster cell counts.
#' If the raster projection is one where cell sizes are approximately constant in area (e.g., UTM),
#' or the raster covers a relatively small geographic extent (e.g., < 1000 km in dimension)
#' areal measurements will often be adequate.
#' Reprojection of rasters should be considered to minimize these effects in
#' other cases (see \code{\link{projectRaster}}).
#'
#' @param cost  A raster of class \code{RasterLayer} giving a landscape resistance
#'              surface, where the values of each raster cell are proportional to
#'              the resistance to movement, dispersal, or gene flow for an organism
#'              in the landscape feature they represent.
#'              Missing values \code{NA} are acceptable (but see below).
#'              Negative values are not.
#'              To extract an MPG with Euclidean links (i.e., and not least-cost
#'              path links) set \code{cost[] <- 1}.
#'
#' @param patch  A raster of class \code{RasterLayer} for a patch-based analysis
#'               OR an integer for a lattice analysis.
#'               If a raster is given it must be of the same extent, origin and
#'               projection as \code{cost} and be binary, without missing values,
#'               where patches=1 and non-patches=0.
#'               For lattice analyses, an integer gives the spacing in raster
#'               cells between focal points in the lattice.
#'
#' @param sa  Optional.  A raster of class \code{RasterLayer} of the same extent,
#'            origin and projection as \code{cost} indicating the study area
#'            (i.e., cells on the landscape to include in the analysis).
#'            If not supplied \code{sa} is the full extent of \code{cost}.
#'            To mask out areas of the landscape to exclude from analysis
#'            (e.g., at the edges of a map), supply a binary raster where
#'            included cells=1 and excluded cells=0.
#'
#' @param filterPatch  Optional.  Remove patches from the analysis that are smaller
#'                     than a given number of cells.
#'
#' @param spreadFactor  Optional.  Fine-grained control over the accuracy of Voronoi polygons.
#'                      To reduce accuracy and increase speed, set this as
#'                      \code{spreadFactor=10} or \code{spreadFactor=100}.
#'
#' @return A \code{gsMPG} object, consisting of a list of objects.\cr\cr
#'         The main elements:\cr
#'         \code{$mpg} is the minimum planar graph as class \code{igraph}\cr
#'         \code{$patchId} is the input \code{patch} raster with patch cells
#'         assigned to their id (\code{RasterLayer})\cr
#'         \code{$voronoi} is the Voronoi tessellation of the patches and
#'         resistance surface (\code{RasterLayer})\cr
#'         \code{$lcpPerimWeight} gives the paths of the links between patches
#'         and their accumulated costs (\code{RasterLayer})\cr
#'         \code{$lcpLinkId} gives the paths of the links between patches and
#'         their id (\code{RasterLayer})\cr
#'         \code{$mpgPlot} provides a quick way of visualizing the mpg (\code{RasterLayer})\cr\cr
#'
#'         The \code{$mpg} has useful vertex and edge attributes.
#'         Vertex attributes give attributes of patches including patch area,
#'         the area of patch edges, the core area of each patch, and the coordinates
#'         of the patch centroid.
#'         All areal measurements are given as raster cell counts.
#'         Edge attributes give attributes of the graph links including link
#'         weights giving accumulated resistance/least-cost path distance,
#'         Euclidean distance, and the start and end coordinates of each link.
#'
#' @references
#' Fall, A., M.-J. Fortin, M. Manseau, D. O'Brien.  (2007) Spatial graphs:  Principles and applications for habitat connectivity.  Ecosystems.  10:448:461\cr
#' Galpern, P., M. Manseau, P.J. Wilson. (2012) Grains of connectivity: analysis at multiple spatial scales in landscape genetics.  Molecular Ecology 21:3996-4009.
#'
#' @author Paul Galpern, Sam Doctolero, Alex Chubaty
#' @docType methods
#' @export
#' @importFrom igraph '%>%' graph_from_data_frame
#' @importFrom raster boundaries cellFromRowCol cellFromRowColCombine compareRaster getValues mask projection raster res writeRaster xyFromCell
#' @importFrom sp coordinates
#' @importFrom utils read.table
#' @rdname gsMPG
#' @seealso \code{\link{gsGOC}, \link{gsThreshold}}
#'
#' @examples
#' \dontrun{
#' require(igraph)
#' require(raster)
#'
#' ## Load raster landscape
#' tiny <- raster(system.file("extdata/tiny.asc", package = "grainscape2"))
#'
#' ## Create a resistance surface from a raster using an is-becomes reclassifyification
#' tinyCost <- reclassify(tiny, rcl = cbind(c(1, 2, 3, 4), c(1, 5, 10, 12)))
#'
#' ## Produce a patch-based MPG where patches are resistance features=1
#' tinyPatchMPG <- gsMPG(cost = tinyCost, patch = (tinyCost == 1))
#'
#' ## Explore the graph structure and node/link attributes
#' gsGraphDataFrame(tinyPatchMPG)
#'
#' ## Find the mean patch area (see igraph manual for use of V() and E())
#' mean(V(tinyPatchMPG$mpg)$patchArea.value)
#'
#' ## Quick visualization of the MPG
#' plot(tinyPatchMPG$mpgPlot, col = c("grey", "black"), legend = FALSE)
#'
#' ## Visualize the minimum spanning tree of the MPG
#' tinyPatchMST <- mst(tinyPatchMPG$mpg)
#' MSTlinks <- edge_attr(tinyPatchMST, "linkId")
#' plot(tinyPatchMPG$patchId, col = "black", legend = FALSE)
#' plot((tinyPatchMPG$lcpLinkId * -1) %in% MSTlinks, add = TRUE, legend = FALSE, col = c(NA, "grey"))
#'
#' ## Additional graph extraction scenarios
#' ## Produce a lattice MPG where focal points are spaced 10 cells apart
#' tinyLatticeMPG <- gsMPG(cost = tinyCost, patch = 10)
#'
#' ## Produce a patch-based MPG with a study area consisting of half of the map
#' tinySa <- tinyCost
#' tinySa[] <- 1
#' tinySa[1:5000] <- 0
#' tinyPatchMPG <- gsMPG(cost = tinyCost, patch = (tinyCost == 1), sa = tinySa)
#' }
#'
gsMPG <- function(cost, patch, sa = NULL, filterPatch = NULL, spreadFactor = 0) {
  ## Check that cost raster is of class RasterLayer
  if ((class(cost) != "RasterLayer")) {
    stop("grainscape2: cost raster must be of class RasterLayer", call. = FALSE)
  }

  ## Prepare a lattice patch if patch is numeric
  if (class(patch) == "numeric") {
    ## Produce the lattice patch rasters
    focalPointDistFreq <- patch
    patch <- cost
    patch[] <- 0
    patch[cellFromRowColCombine(patch,
                                seq(1, nrow(patch), by = focalPointDistFreq) + focalPointDistFreq/2,
                                seq(1, ncol(patch), by = focalPointDistFreq) + focalPointDistFreq/2)] <- 1
    ## Remove lattice points that fall on NA cost cells
    patch[is.na(cost)] <- 0
  } else if ((class(patch) != "RasterLayer")) {
    stop("grainscape2: patch must be a raster (patch-based model) OR an integer (lattice model).", call. = FALSE)
  }

  ## Check that input rasters are of class RasterLayer
  if ((class(cost) != "RasterLayer")) {
    stop("grainscape2: cost raster must be of class RasterLayer.", call. = FALSE)
  }

  ## Check patch and cost are comparable
  if (!compareRaster(patch, cost, res = TRUE, orig = TRUE, stopiffalse = FALSE)) {
    stop("grainscape2: patch and cost rasters must be identical in extent, projection, origin and resolution.", call. = FALSE)
  }

  ## Check additional geographic features of input rasters
  if (res(cost)[1] != res(cost)[2]) {
    warning(paste0("grainscape2:  raster cells are not square;  assuming a square cell of ",
                   res(cost)[1], " units."), call. = FALSE)
  }

  ## Check projection
  if (!is.na(projection(cost)) && (!grepl("UTM|utm", toupper(projection(cost))))) {
    warning("grainscape2:  projection suggests that all cells may not be of equal area; Note that grainscape2 assumes equal area in all calculations.", call. = FALSE)
  }

  ## use `cost` raster as template for `rasCost`, `rasPatch`, and `rasSa`
  rasCost <- rasPatch <- rasSa <- cost

  rasCost[] <- getValues(cost)
  rasPatch[] <- getValues(patch)

  ## Check filterPatch
  if (is.null(filterPatch)) {
    ## Set filterPatch smaller than area of cell
    filterPatch <- (prod(res(rasCost))/10000) * 0.01
  } else {
    ## Set filterPatch as area in hectares
    filterPatch <- (prod(res(rasCost))/10000) * abs(filterPatch)
  }

  ## Check sa is comparable with other rasters
  if (!is.null(sa)) {
    if (class(sa) != "RasterLayer") {
      stop("grainscape2:  sa raster must be of class RasterLayer", call. = FALSE)
    }
    if (!compareRaster(cost, sa, res = TRUE, orig = TRUE, stopiffalse = FALSE)) {
      stop("grainscape2: patch, cost and sa rasters must be identical in extent, projection, origin and resolution", call. = FALSE)
    }
    rasSa[] <- getValues(sa)
    rasCost[is.na(rasSa)] <- NA
    rasPatch[is.na(rasSa)] <- NA
  } else {
    rasSa[] <- 1
  }

  ## Check that patch raster is binary
  if (!all(unique(rasPatch[]) %in% c(TRUE, FALSE))) {
    stop("grainscape2:  patch must be a binary raster (=1 for patches; =0 for non-patches).  Missing values (NA) should be set to 0.", call. = FALSE)
  }

  ## Check that cost raster is not equal to NA at patches
  if (sum(is.na(rasCost[rasPatch == 1]) > 0)) {
    stop("grainscape2:  cost raster must not contain missing values at patch cells", call. = FALSE)
  }

  ## Call the habitat connectivity engine
  hce <- habConnEngine(cost = rasCost, patches = rasPatch)

  ## Establish mpg object
  mpg <- list()
  mpg$landscapeType <- "cost"
  mpg$landscape <- rasCost

  mpg$patchId <- hce$patchLinks
  mpg$patchId[hce$patchLinks < 0] <- NA

  mpg$voronoi <- hce$voronoi

  mpg$lcpLinkId <- hce$patchLinks
  mpg$lcpLinkId[hce$patchLinks >= 0] <- NA

  mpg$lcpPerimWeight <- reclassify(mpg$lcpLinkId, rcl = matrix(c(
    hce$linkData$LinkId, hce$linkData$PerimWeight), ncol = 2))

  mpg$mpgPlot <- hce$patchLinks              # TO BE REMOVED?

  ## Get additional patch information
  uniquePatches <- sort(unique(mpg$voronoi[]))

  ## Patch edge
  patchEdge <- mpg$patchId
  patchEdge[patchEdge == 0] <- NA
  patchEdge <- raster::boundaries(patchEdge, type = "inner")
  patchEdge[patchEdge == 0] <- NA
  patchEdge <- mask(mpg$patchId, patchEdge)

  ## Patch area and core area
  patchArea <- freq(mpg$patchId, useNA = "no")
  patchEdgeArea <- freq(patchEdge, useNA = "no")
  patch <- data.frame(name = uniquePatches, patchId = uniquePatches,
                      patchArea = patchArea, patchEdgeArea = patchEdgeArea,
                      coreArea = patchArea - patchEdgeArea)

  ## Find centroids of each patch
  cellXY <- coordinates(mpg$patchId)
  r <- mpg$patchId
  r[r == 0] <- NA
  rasX <- rasY <- r
  rasX[] <- cellXY[, 1]
  rasY[] <- cellXY[, 2]
  centroids <- cbind(zonal(rasX, r, fun = 'mean', na.rm = TRUE),
                     zonal(rasY, r, fun = 'mean', na.rm = TRUE)[, 2])

  toGraphV <- cbind(patch, centroidX = centroids[, 2], centroidY = centroids[, 3])

  toGraphE <- data.frame(v1 = hce$linkData$StartId,
                         v2 = hce$linkData$EndId,
                         linkId = hce$linkData$LinkId * -1L,
                         lcpPerimWeight = hce$linkData$PerimWeight,
                         startPerimX = hce$linkData$StartRow,
                         startPerimY = hce$linkData$StartColumn,
                         endPerimX = hce$linkData$EndRow,
                         endPerimY = hce$linkData$EndColumn)
  mpg$mpg <- graph_from_data_frame(toGraphE, directed = FALSE, vertices = toGraphV)

  class(mpg) <- "gsMPG"

  return(mpg)
}
