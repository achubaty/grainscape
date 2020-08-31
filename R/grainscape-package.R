#' Landscape Connectivity, Habitat, and Protected Area Networks
#'
#' @description
#'
#' Given a landscape resistance surface, creates minimum planar graph and
#' grains of connectivity models that can be used to calculate effective
#' distances for landscape connectivity at multiple scales.
#'
#' @details
#'
#' Landscape connectivity modelling to understand the movement and dispersal of
#' organisms has been done using raster resistance surfaces and landscape graph methods.
#' Grains of connectivity (GOC) models combine elements of both approaches to produce
#' a continuous and scalable tool that can be applied in a variety of study systems.
#' The purpose of this package is to implement grains of connectivity analyses.
#' Routines accept raster-based resistance surfaces as input and return raster,
#' vector and graph-based data structures to represent connectivity at multiple scales.
#' Effective distances describing connectivity between geographic locations can
#' be determined at multiple scales.
#' Analyses of this sort can contribute to corridor identification, landscape genetics,
#' as well as other connectivity assessments.
#' Minimum planar graph (MPG; Fall \emph{et al.}, 2007) models of resource patches on
#' landscapes can also be generated using the software.
#'
#' MPG calculations and generalization of the Voronoi tessellation used in GOC models
#' is based on the routines in SELES software (Fall and Fall, 2001).
#' Routines also depend on the \code{sp} (Pebesma and Bivand, 2005),
#' \pkg{raster} (Hijmans and van Etten, 2011), \pkg{igraph} (Csardi and Nepusz, 2006),
#' and optionally \pkg{rgeos} packages (Bivand and Rundel, 2012).
#'
#' A paper describing the use of this package for landscape connectivity modelling is
#' available at \url{https://doi.org/10.1111/2041-210X.13350}.
#'
#' A detailed tutorial is available as a vignette (see \code{browseVignettes('grainscape')}).
#'
#' @import igraph
#' @import methods
#' @importFrom Rcpp evalCpp
#' @useDynLib grainscape, .registration = TRUE
#'
#' @references
#'
#' Bivand, R.S. and C. Rundel. (2016). rgeos: Interface to Geometry Engine - Open Source (GEOS).
#' R package version 0.3-19, \url{https://CRAN.R-project.org/package=rgeos}.
#'
#' Csardi, G. and T. Nepusz. (2006). The igraph software package for complex network research.
#' InterJournal Complex Systems 1695. \url{https://igraph.org}.
#'
#' Fall, A. and J. Fall. (2001). A domain-specific language for models of landscape dynamics.
#' Ecological Modelling 141:1-18.
#'
#' Fall, A., M.-J. Fortin, M. Manseau, D. O'Brien. (2007) Spatial graphs: Principles
#' and applications for habitat connectivity. Ecosystems 10:448:461.
#'
#' Galpern, P., M. Manseau. (2013a) Finding the functional grain: comparing methods
#' for scaling resistance surfaces. Landscape Ecology 28:1269-1291.
#'
#' Galpern, P., M. Manseau. (2013b) Modelling the influence of landscape connectivity
#' on animal distribution: a functional grain approach. Ecography 36:1004-1016.
#'
#' Galpern, P., M. Manseau, A. Fall. (2011)  Patch-based graphs of landscape connectivity:
#' A guide to construction, analysis and application for conservation.
#' Biological Conservation 144:44-55.
#'
#' Galpern, P., M. Manseau, P.J. Wilson. (2012) Grains of connectivity: analysis
#' at multiple spatial scales in landscape genetics. Molecular Ecology 21:3996-4009.
#'
#' Hijmans, R.J. and J. van Etten. (2016). raster: Geographic analysis and modeling
#' with raster data. R package version 2.5-8, \url{https://CRAN.R-project.org/package=raster}.
#'
#' Pebesma, E.J. and R.S. Bivand. (2005). Classes and methods for spatial data in R.
#' R News 5 (2), \url{https://cran.r-project.org/doc/Rnews/}.
#'
#' @name grainscape-package
#' @keywords connectivity
#' @keywords minimum planar graph
#' @keywords spatial graph
#'
"_PACKAGE"

#' Test maps included with \code{grainscape}
#'
#' Intended for users to explore the functionality of the package using simple
#' and artificial land cover maps.
#' These maps have four or five discrete land cover classes (integers from 1 to 5)
#' intended to represent distinct land cover types.
#' Typical analyses begin by reclassifying these to reflect resistance to movement.
#'
#' @details
#'
#' \describe{
#'   \item{\code{patchy.asc}}{A caricatured map of four land cover classes, where
#'   patches are large and easy to identify polygonal regions for heuristic purposes.
#'   This unrealistic map can be used to illustrate the method and understand how it works.
#'   The map also serves a similar heuristic purpose in a review of graph-based
#'   connectivity methods (Galpern \emph{et al.}, 2011). (400 x 400 raster cells.)}
#'
#'   \item{\code{fragmented.asc}}{A simulated land cover map with five land cover
#'   classes using an algorithm that produces fragmentation. (400 x 400 raster cells.)}
#'
#'   \item{\code{tiny.asc}}{Similar to \code{fragmented.asc} but smaller in extent
#'   for lightning-fast computation and experimental use. (100 x 100 raster cells.)}
#' }
#'
#' @docType data
#' @format raster
#' @keywords maps
#' @name grainscape-maps
#' @rdname grainscape-maps
NULL
