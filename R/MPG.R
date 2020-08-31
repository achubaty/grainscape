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
#' a resource patch. Lattice models can be used as a generalized and functional
#' approach to scaling resistance surfaces.
#'
#' Rasters should be projected and not in geographic coordinates (i.e. \code{projection(cost)}
#' should not contain \code{"+proj=longlat"}) or the function will issue a warning.
#' In unprojected cases consider using \code{\link{projectRaster}} to change to an appropriate
#' coordinate system for the location and extent of interest that balances both distance and areal
#' accuracy. See \url{https://www.spatialreference.org/} for location-specific suggestions.
#' Use of geographic coordinates will result in inaccurate areal and distance measurements,
#' rendering the models themselves inaccurate.
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
#' @author Paul Galpern, Sam Doctolero, Alex Chubaty
#' @export
#' @importFrom raster boundaries cellFromRowCol cellFromRowColCombine compareRaster
#' @importFrom raster getValues mask projection raster res writeRaster
#' @importFrom raster xFromCol xyFromCell yFromRow
#' @importFrom sp coordinates
#' @importFrom stats na.omit
#' @importFrom utils read.table
#' @include classes.R
#' @rdname MPG
#' @seealso \code{\link{GOC}, \link{threshold}}
#'
#' @example inst/examples/example_preamble.R
#' @example inst/examples/example_preamble_MPG.R
#' @example inst/examples/example_MPG.R
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
    stop("patch and cost rasters must be identical in extent, projection, origin and resolution.")
  }

  if (!is.na(projection(cost)) && grepl("longlat", projection(cost))) {
    warning("input rasters in geographic coordinates (i.e. '+proj=longlat') are unlikely",
            " to produce reliable estimates of area or distance.",
            " For accurate results, project rasters with an appropriate coordinate",
            " system for the location and extent of interest.", immediate. = TRUE)
  }

  ## use `cost` raster as template for `rasCost` and `rasPatch`
  rasCost <- cost
  rasPatch <- patch

  rasCost[] <- getValues(cost)
  rasPatch[] <- getValues(patch)

  ## Check that patch raster is binary, first coercing NAs to zeroes
  rasPatch[is.na(rasPatch)] <- 0
  if (!all(unique(rasPatch[]) %in% c(FALSE, TRUE))) {
    stop("patch must be a binary raster (=1 for patches; =0 for non-patches).")
  }

  ## Check that cost raster is not equal to NA at patches
  if (sum(is.na(rasCost[rasPatch == 1]) > 0)) {
    stop("cost raster must not contain missing values at patch cells.")
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
  uniquePatches <- voronoi[voronoi > 0] %>% na.omit() %>% unique() %>% sort() # nolint

  ## Patch edge
  patchEdge <- patchId
  patchEdge <- raster::boundaries(patchEdge, type = "inner")
  patchEdge[patchEdge == 0] <- NA
  patchEdge <- mask(patchId, patchEdge)

  ## Patch area and core area
  patchArea <- freq(patchId, useNA = "no")[, 2] * res(cost)[1] * res(cost)[2]
  patchEdgeArea <- freq(patchEdge, useNA = "no")[, 2] * res(cost)[1] * res(cost)[2]
  patch <- data.frame(name = uniquePatches, patchId = uniquePatches,
                      patchArea = patchArea, patchEdgeArea = patchEdgeArea,
                      coreArea = patchArea - patchEdgeArea)

  ## Find centroids of each patch
  cellXY <- coordinates(patchId)
  r <- rasX <- rasY <- patchId
  r[r == 0] <- NA
  rasX[] <- cellXY[, 1]
  rasY[] <- cellXY[, 2]
  centroids <- cbind(zonal(rasX, r, fun = "mean", na.rm = TRUE),
                     zonal(rasY, r, fun = "mean", na.rm = TRUE)[, 2]) %>%
    as.data.frame()
  colnames(centroids) <- c("zone", "meanX", "meanY")

  toGraphV <- cbind(patch, centroidX = centroids$meanX, centroidY = centroids$meanY) %>%
    as.data.frame()

  toGraphE <- data.frame(v1 = hce@linkData$StartId,
                         v2 = hce@linkData$EndId,
                         linkId = hce@linkData$LinkId * -1L,
                         lcpPerimWeight = hce@linkData$PerimWeight,
                         startPerimX = xFromCol(cost, hce@linkData$StartColumn),
                         startPerimY = yFromRow(cost, hce@linkData$StartRow),
                         endPerimX = xFromCol(cost, hce@linkData$EndColumn),
                         endPerimY = yFromRow(cost, hce@linkData$EndRow))
  mpgIgraph <- graph_from_data_frame(toGraphE, directed = FALSE, vertices = toGraphV)

  mpg <- new("mpg", mpg = mpgIgraph, patchId = patchId, voronoi = voronoi,
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

    ids <- cellFromRowColCombine(
      patch,
      seq(1, nrow(patch), by = focalPointDistFreq) + focalPointDistFreq / 2,
      seq(1, ncol(patch), by = focalPointDistFreq) + focalPointDistFreq / 2
    )
    patch[ids] <- 1
    ## Remove lattice points that fall on NA cost cells
    patch[is.na(cost)] <- 0

    MPG(cost = cost, patch = patch, ...)
})
