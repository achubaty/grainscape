#' Produce a minimum planar graph (MPG) at multiple scales
#'
#' @description
#' Perform a scalar analysis of a minimum planar graph (MPG) by building the
#' graph at a series of link thresholds.
#' As the threshold value increases more nodes in the graph become connected,
#' forming increasingly fewer components, until the graph becomes connected (e.g., Brooks, 2003).
#' N.B. Grains of connectivity (GOC) done by \code{\link{GOC}} is also a scalar
#' analysis using Voronoi tessellations rather than patches (see Galpern \emph{et al.}, 2012).
#'
#' @param x       A \code{mpg} object produced by \code{\link{MPG}}.
#'
#' @param weight  A string giving the link weight or attribute to use for threshold.
#'                \code{"lcpPerimWeight"} uses the accumulated resistance or least-cost path
#'                distance from the perimeters of patches as the link weight.
#'                \code{"eucPerimWeight"} use the Euclidean distance from the
#'                perimeters of patches as the link weight.
#'
#' @param nThresh  Optional. An integer giving the number of thresholds (or scales)
#'                 at which to create GOC models.
#'                 Thresholds are selected to produce a maximum number of unique
#'                 grains (i.e., models).
#'                 \code{nThresh} thresholds are also approximately evenly spread
#'                 between 0 and the threshold at which all patches or focal points
#'                 on the landscape are connected.
#'                 This is a simple way to get a representative subset of all
#'                 possible GOC models.
#'                 Provide either \code{nThresh} or \code{doThresh} not both.
#'
#' @param doThresh  Optional. A vector giving the link thresholds at which to create GOC models.
#'                  Use \code{\link{threshold}} to identify thresholds of interest.
#'                  Provide either \code{nThresh} or \code{doThresh} not both.
#'
#' @param ...      Additional arguments (not used).
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
#' @author Paul Galpern and Alex Chubaty
#' @export
#' @include classes.R
#' @rdname threshold
#' @seealso \code{\link{MPG}}
#'
#' @example inst/examples/example_preamble.R
#' @example inst/examples/example_preamble_MPG.R
#' @example inst/examples/example_threshold.R
#'
setGeneric("threshold", function(x, ...) {
  standardGeneric("threshold")
})

#' @export
#' @rdname threshold
setMethod(
  "threshold",
  signature = "mpg",
  definition = function(x, weight = "lcpPerimWeight", nThresh = NULL, doThresh = NULL, ...) {
    baseGraph <- x@mpg

    threshGraph <- vector("list")

    linkWeight <- try(edge_attr(baseGraph, weight), silent = TRUE)

    if (inherits(linkWeight, "try-error")) {
      stop("weight must be the name of an existing link attribute to threshold",
           " (e.g., 'lcpPerimWeight')")
    }

    if (is.null(nThresh) && is.null(doThresh)) {
      stop("either nThresh or doThresh must be specified")
    } else if (!is.null(nThresh) && !is.null(doThresh)) {
      stop("only one of nThresh or doThresh must be specified")
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
})
