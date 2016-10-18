#' Produce a minimum planar graph (MPG) at multiple scales
#'
#' @description
#' Perform a scalar analysis of a minimum planar graph (MPG) by building the
#' graph at a series of link thresholds.
#' As the threshold value increases more nodes in the graph become connected,
#' forming increasingly fewer components, until the graph becomes connected (e.g., Brooks, 2003).
#' N.B. Grains of connectivity (GOC) done by \code{\link{GOC}} is also a scalar
#' analysis using Voronoi tessellations rather than patches (see Galpern et al., 2012).
#'
#' @param ...  Additional arguments.
#'
#' @export
#'
threshold <- function(x, ...) UseMethod("threshold")



#' @param x       A \code{mpg} object produced by \code{\link{MPG}}.
#'
#' @param weight  A string giving the link weight or attribute to use for threshold.
#'                \code{"lcpPerimWeight"} uses the accumulated resistance or least-cost path
#'                distance from the perimeters of patches as the link weight.
#'                \code{"eucPerimWeight"} use the Euclidean distance from the
#'                perimeters of patches as the link weight.
#'
#' @param nThresh  Optional.  An integer giving the number of thresholds (or scales)
#'                 at which to create GOC models.
#'                 Thresholds are selected to produce a maximum number of unique grains (i.e., models).
#'                 \code{nThresh} thresholds are also approximately evenly spread between 0 and
#'                 the threshold at which all patches or focal points on the landscape are connected.
#'                 This is a simple way to get a representative subset of all possible GOC models.
#'                 Provide either \code{nThresh} or \code{doThresh} not both.
#'
#' @param doThresh  Optional.  A vector giving the link thresholds at which to create GOC models.
#'                  Use \code{link{threshold}} to identify thresholds of interest.
#'                  Provide either \code{nThresh} or \code{doThresh} not both.
#'
#' @return A list object with the following elements:
#'
#' \describe{
#'   \item{\code{summary}}{summarizes the thresholded graphs generated and their properties;}
#'   \item{\code{th}}{a list of length \code{nThresh} or \code{length(doThresh)}
#'   giving the thresholded graph (class \code{igraph}) at each threshold.}
#' }
#'
#' @note See \code{\link{MPG}} for warning related to areal measurements.
#'
#' @references
#' Brooks, C.P. (2003) A scalar analysis of landscape connectivity. Oikos 102:433-439.
#'
#' Fall, A., M.-J. Fortin, M. Manseau, D. O'Brien. (2007) Spatial graphs: Principles and applications for habitat connectivity. Ecosystems 10:448:461.
#'
#' Galpern, P., M. Manseau, P.J. Wilson. (2012) Grains of connectivity: analysis at multiple spatial scales in landscape genetics.  Molecular Ecology 21:3996-4009.
#'
#' @author Paul Galpern
#' @docType methods
#' @export
#' @importFrom igraph '%>%' clusters delete.edges edge_attr
#' @rdname threshold
#' @seealso \code{\link{MPG}}
#'
#' @examples
#' \dontrun{
#' # Load raster landscape
#' tiny <- raster(system.file("extdata/tiny.asc", package = "grainscape"))
#'
#' ## Create a resistance surface from a raster using an is-becomes reclassification
#' tinyCost <- reclassify(tiny, rcl = cbind(c(1, 2, 3, 4), c(1, 5, 10, 12)))
#'
#' ## Produce a patch-based MPG where patches are resistance features=1
#' tinyPatchMPG <- MPG(cost = tinyCost, patch = tinyCost == 1)
#'
#' ## Threshold this graph at a representative subset of 10 thresholds
#' tinyThresh <- threshold(tinyPatchMPG, nThresh = 10)
#'
#' ## Examine the properties of one of these threshold graphs
#' print(tinyThresh$th[[7]], vertex = TRUE, edge = TRUE)
#' }
#'
threshold.mpg <- function(x, ..., weight = "lcpPerimWeight", nThresh = NULL, doThresh = NULL) {
  baseGraph <- x$mpg

  threshGraph <- vector("list")

  linkWeight <- try(edge_attr(baseGraph, weight), silent = TRUE)
  if (inherits(linkWeight, "try-error")) {
    stop("grainscape: weight must be the name of an existing link attribute",
         " to threshold (e.g., 'lcpPerimWeight')", call. = FALSE)
  }

  if (is.null(nThresh) && is.null(doThresh)) {
    stop("grainscape: either nThresh or doThresh must be specified", call. = FALSE)
  } else if (!is.null(nThresh) && !is.null(doThresh)) {
    stop("grainscape: only one of nThresh or doThresh must be specified", call. = FALSE)
  } else if (is.null(doThresh)) {
    doThresh <- seq(0, max(linkWeight), length = nThresh)
  }

  threshGraph$summary <- data.frame(maxLink = doThresh)

  threshGraph$th <- lapply(1:length(doThresh), function(i) {
    delete.edges(baseGraph, which(linkWeight > doThresh[i]))
  })

  threshGraph$summary$nComponents <- lapply(threshGraph$th, function(z) {
    clusters(z)$no
  }) %>%
    unlist()

  return(threshGraph)
}

