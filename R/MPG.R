#' Extract a minimum planar graph (MPG) model from a landscape resistance surface
#'
#' @description Extracts a minimum planar graph (MPG) and is also the first step
#' in grains of connectivity (GOC) modelling.
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
#' approach to scaling resistance surfaces.
#'
#' Areal measurements are given as raster cell counts.
#' If the raster projection is one where cell sizes are approximately constant in area (e.g., UTM),
#' or the raster covers a relatively small geographic extent (e.g., < 1000 km in dimension)
#' areal measurements will often be adequate.
#' Reprojection of rasters should be considered to minimize these effects in
#' other cases (see \code{\link{projectRaster}}).
#'
#' @param cost   A \code{RasterLayer} giving a landscape resistance surface,
#'               where the values of each raster cell are proportional to the
#'               resistance to movement, dispersal, or gene flow for an organism
#'               in the landscape feature they represent.
#'               Missing values \code{NA} are acceptable (but see below).
#'               Negative values are not.
#'               To extract an MPG with Euclidean links (i.e., and not least-cost
#'               path links) set \code{cost[] <- 1}.
#'
#' @param patch  A raster of class \code{RasterLayer} for a patch-based analysis
#'               OR an integer for a lattice analysis.
#'               If a raster is given it must be of the same extent, origin and
#'               projection as \code{cost} and be binary, without missing values,
#'               where patches=1 and non-patches=0.
#'               For lattice analyses, an integer gives the spacing in raster
#'               cells between focal points in the lattice.
#'
#' @param ...  Additional arguments (not used).
#'
#' @return A \code{\link[=mpg-class]{mpg}} object.
#'
#' @references
#' Fall, A., M.-J. Fortin, M. Manseau, D. O'Brien. (2007) Spatial graphs: Principles and applications for habitat connectivity. Ecosystems 10:448:461.
#'
#' Galpern, P., M. Manseau, P.J. Wilson. (2012) Grains of connectivity: analysis at multiple spatial scales in landscape genetics.  Molecular Ecology 21:3996-4009.
#'
#' @author Paul Galpern, Sam Doctolero, Alex Chubaty
#' @docType methods
#' @export
#' @importFrom raster boundaries cellFromRowCol cellFromRowColCombine compareRaster
#' @importFrom raster getValues mask projection raster res writeRaster xyFromCell
#' @importFrom sp coordinates
#' @importFrom stats na.omit
#' @importFrom utils read.table
#' @include classes.R
#' @rdname MPG
#' @seealso \code{\link{GOC}, \link{threshold}}
#'
#' @examples
#' \dontrun{
#' library(igraph)
#' library(raster)
#'
#' ## Load raster landscape
#' tiny <- raster(system.file("extdata/tiny.asc", package = "grainscape"))
#'
#' ## Create a resistance surface from a raster using an is-becomes reclassifyification
#' tinyCost <- reclassify(tiny, rcl = cbind(c(1, 2, 3, 4), c(1, 5, 10, 12)))
#'
#' ## Produce a patch-based MPG where patches are resistance features=1
#' tinyPatchMPG <- MPG(cost = tinyCost, patch = (tinyCost == 1))
#'
#' ## Explore the graph structure and node/link attributes
#' graphdf(tinyPatchMPG)
#'
#' ## Find the mean patch area (see igraph manual for use of V() and E())
#' mean(V(tinyPatchMPG@mpg)$patchArea.value)
#'
#' ## Quick visualization of the MPG
#' plot(tinyPatchMPG, col = c("grey", "black"), legend = FALSE)
#'
#' ## Visualize the minimum spanning tree of the MPG
#' tinyPatchMST <- mst(tinyPatchMPG@mpg)
#' MSTlinks <- edge_attr(tinyPatchMST, "linkId")
#' plot(tinyPatchMPG@patchId, col = "black", legend = FALSE)
#' plot((tinyPatchMPG@lcpLinkId * -1) %in% MSTlinks, add = TRUE, legend = FALSE, col = c(NA, "grey"))
#'
#' ## Additional graph extraction scenarios
#' ## Produce a lattice MPG where focal points are spaced 10 cells apart
#' tinyLatticeMPG <- MPG(cost = tinyCost, patch = 10)
#' plot(tinyLatticeMPG)
#' }
#'
setGeneric("MPG", function(cost, patch, ...) {
  standardGeneric("MPG")
})

#' @export
#' @rdname MPG
setMethod(
  "MPG",
  signature = c(cost = "RasterLayer", patch = "RasterLayer"),
  definition = function(cost, patch, ...) {
  ## Check patch and cost are comparable
  if (!compareRaster(patch, cost, res = TRUE, orig = TRUE, stopiffalse = FALSE)) {
    stop("grainscape: patch and cost rasters must be identical in extent, projection, origin and resolution.", call. = FALSE)
  }

  ## Check additional geographic features of input rasters
  if (res(cost)[1] != res(cost)[2]) {
    warning(paste0("grainscape:  raster cells are not square;  assuming a square cell of ",
                   res(cost)[1], " units."), call. = FALSE)
  }

  ## Check projection
  if (!is.na(projection(cost)) && (!grepl("UTM|utm", toupper(projection(cost))))) {
    warning("grainscape:  projection suggests that all cells may not be of equal area; Note that grainscape assumes equal area in all calculations.", call. = FALSE)
  }

  ## use `cost` raster as template for `rasCost` and `rasPatch`
  rasCost <- cost
  rasPatch <- patch

  rasCost[] <- getValues(cost)
  rasPatch[] <- getValues(patch)

  ## Check that patch raster is binary, first corecing NAs to zeroes
  rasPatch[is.na(rasPatch)] <- 0
  if (!all(unique(rasPatch[]) %in% c(FALSE, TRUE))) {
    stop("grainscape:  patch must be a binary raster (=1 for patches; =0 for non-patches).", call. = FALSE)
  }

  ## Check that cost raster is not equal to NA at patches
  if (sum(is.na(rasCost[rasPatch == 1]) > 0)) {
    stop("grainscape:  cost raster must not contain missing values at patch cells", call. = FALSE)
  }

  ## Call the habitat connectivity engine
  hce <- .habConnEngine(cost = rasCost, patches = rasPatch)

  ## Establish mpg object
  patchId <- hce@patchLinks
  patchId[hce@patchLinks < 0] <- NA

  voronoi <- hce@voronoi

  lcpLinkId <- hce@patchLinks
  lcpLinkId[hce@patchLinks >= 0] <- NA

  lcpPerimWeight <- reclassify(lcpLinkId, rcl = matrix(c(
    hce@linkData$LinkId, hce@linkData$PerimWeight), ncol = 2))

  mpgPlot <- hce@patchLinks

  ## Get additional patch information
  uniquePatches <- voronoi[voronoi > 0] %>% na.omit() %>% unique() %>% sort()

  ## Patch edge
  patchEdge <- patchId
  patchEdge <- raster::boundaries(patchEdge, type = "inner")
  patchEdge[patchEdge == 0] <- NA
  patchEdge <- mask(patchId, patchEdge)

  ## Patch area and core area
  patchArea <- freq(patchId, useNA = "no")
  patchEdgeArea <- freq(patchEdge, useNA = "no")
  patch <- data.frame(name = uniquePatches, patchId = uniquePatches,
                      patchArea = patchArea, patchEdgeArea = patchEdgeArea,
                      coreArea = patchArea - patchEdgeArea)

  ## Find centroids of each patch
  cellXY <- coordinates(patchId)
  r <- rasX <- rasY <- patchId
  r[r == 0] <- NA
  rasX[] <- cellXY[, 1]
  rasY[] <- cellXY[, 2]
  centroids <- cbind(zonal(rasX, r, fun = 'mean', na.rm = TRUE),
                     zonal(rasY, r, fun = 'mean', na.rm = TRUE)[, 2]) %>%
    as.data.frame()
  colnames(centroids) <- c("zone", "meanX", "meanY")

  toGraphV <- cbind(patch, centroidX = centroids$meanX, centroidY = centroids$meanY) %>%
    as.data.frame()

  toGraphE <- data.frame(v1 = hce@linkData$StartId,
                         v2 = hce@linkData$EndId,
                         linkId = hce@linkData$LinkId * -1L,
                         lcpPerimWeight = hce@linkData$PerimWeight,
                         startPerimX = hce@linkData$StartRow,
                         startPerimY = hce@linkData$StartColumn,
                         endPerimX = hce@linkData$EndRow,
                         endPerimY = hce@linkData$EndColumn)
  mpg.igraph <- graph_from_data_frame(toGraphE, directed = FALSE, vertices = toGraphV)

  mpg <- new('mpg', mpg = mpg.igraph, patchId = patchId, voronoi = voronoi,
             lcpPerimWeight = lcpPerimWeight, lcpLinkId = lcpLinkId, mpgPlot = mpgPlot)

  return(mpg)
})

#' @export
#' @rdname MPG
setMethod(
  "MPG",
  signature = c(cost = "RasterLayer", patch = "numeric"),
  definition = function(cost, patch, ...) {
    ## Produce the lattice patch rasters
    focalPointDistFreq <- patch
    patch <- cost
    patch[] <- 0
    patch[cellFromRowColCombine(patch,
                                seq(1, nrow(patch), by = focalPointDistFreq) + focalPointDistFreq/2,
                                seq(1, ncol(patch), by = focalPointDistFreq) + focalPointDistFreq/2)] <- 1
    ## Remove lattice points that fall on NA cost cells
    patch[is.na(cost)] <- 0

    MPG(cost = cost, patch = patch, ...)
})
