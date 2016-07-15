#' Produce a data.frame containing the structure and associated attributes for a gsMPG, gsGOC, or igraph object
#'
#' @description
#' Given a gsMPG, gsGOC, or any igraph object produce a data.frame containing the
#' node (vertex) and link (edge) structure as well as the associated attributes for these.
#' This provides an easy way to create data tables describing graphs, particularly
#' helpful for users unfamiliar with the structure of igraph objects.
#'
#' @param gsObj  A \code{gsMPG}, \code{gsGOC}, or \code{igraph} object.
#'
#' @return A list object:\cr\cr
#' \code{$v} giving node (vertex) names and associated attributes\cr
#' \code{$e} giving link (edge) lists and associated attributes\cr\cr
#' Please see \code{\link{gsMPG}} and \code{\link{gsGOC}} for details about the attributes.\cr\cr
#' For \code{\link{gsGOC}} objects which typically contain multiple thresholds,
#' an enumerated list of the same length as the number of thresholds is returned
#' each containing \code{$v} and \code{$e} elements.
#'
#' @references
#' Fall, A., M.-J. Fortin, M. Manseau, D. O'Brien.  (2007) Spatial graphs:  Principles and applications for habitat connectivity.  Ecosystems.  10:448:461\cr\cr
#' Galpern, P., M. Manseau, P.J. Wilson. (2012) Grains of connectivity: analysis at multiple spatial scales in landscape genetics.  Molecular Ecology 21:3996-4009.\cr
#'
#' @author Paul Galpern
#' @docType methods
#' @export
#' @importFrom igraph get.edge.attribute get.edgelist get.vertex.attribute is.igraph list.edge.attributes list.vertex.attributes
#' @importFrom utils type.convert
#' @rdname gsGraphDataFrame
#' @seealso \code{\link{gsMPG}}, \code{\link{gsGOC}}
#'
#' @examples
#' \dontrun{
#' ## Load raster landscape
#' tiny <- raster(system.file("extdata/tiny.asc", package = "grainscape"))
#'
#' ## Create a resistance surface from a raster using an is-becomes reclassification
#' tinyCost <- reclassify(tiny, rcl = cbind(c(1, 2, 3, 4), c(1, 5, 10, 12)))
#'
#' ## Produce a patch-based MPG where patches are resistance features=1
#' tinyPatchMPG <- gsMPG(cost = tinyCost, patch = tinyCost == 1)
#'
#' ## Extract a representative subset of 5 grains of connectivity
#' tinyPatchGOC <- gsGOC(tinyPatchMPG, nThresh = 5)
#'
#' ## Create a data.frame with the structure and attributes of a gsMPG object
#' tinyPatchMPG_df <- gsGraphDataFrame(tinyPatchMPG)
#'
#' ## Create a data.frame with the structure and attributes of a gsGOC object
#' tinyPatchGOC_df <- gsGraphDataFrame(tinyPatchGOC)
#'
#' ## Create a data.frame with the structure and attributes of any igraph object
#' gsGraphDataFrame(tinyPatchGOC$th[[1]]$goc)
#' }
#'
gsGraphDataFrame <- function(gsObj) {
  if (!(class(gsObj) %in% c("gsMPG", "gsGOC", "igraph"))) {
    stop("grainscape2: gsObj must be a gsMPG, gsGOC or igraph object", call. = FALSE)
  }

  if (class(gsObj) == "gsMPG") {
    theseGraphs <- vector("list", 1)
    theseGraphs[[1]] <- gsObj$mpg
  } else if (class(gsObj) == "igraph") {
    theseGraphs <- vector("list", 1)
    theseGraphs[[1]] <- gsObj
  } else {
    theseGraphs <- lapply(gsObj$th, function(x) x$goc)
  }

  results <- vector("list", length(theseGraphs))

  for (i in 1:length(theseGraphs)) {
    thisGraph <- theseGraphs[[i]]

    if (is.igraph(thisGraph))  {
      results[[i]] <- list()
      results[[i]]$v <- data.frame(sapply(list.vertex.attributes(thisGraph), function(x) {
        get.vertex.attribute(thisGraph, x)
      }), stringsAsFactors = FALSE)
      results[[i]]$e <- data.frame(get.edgelist(thisGraph), sapply(list.edge.attributes(thisGraph), function(x) {
        get.edge.attribute(thisGraph, x)
      }), stringsAsFactors = FALSE)
      edgeDfNames <- names(results[[i]]$e)
      names(results[[i]]$e) <- c("e1", "e2", edgeDfNames[3:length(edgeDfNames)])

      ## Clean-up storage mode structure of data.frames
      results[[i]]$e <- as.data.frame(sapply(results[[i]]$e, as.character), stringsAsFactors = FALSE)
      results[[i]]$v <- as.data.frame(sapply(results[[i]]$v, as.character), stringsAsFactors = FALSE)
      results[[i]]$e <- as.data.frame(lapply(results[[i]]$e, function(x) {
        type.convert(x, as.is = TRUE)
      }), stringsAsFactors = FALSE)
      results[[i]]$v <- as.data.frame(lapply(results[[i]]$v, function(x) {
        type.convert(x, as.is = TRUE)
      }), stringsAsFactors = FALSE)
    } else {
      results[[i]]$v <- NA
      results[[i]]$e <- NA
    }
  }

  return(results)
}
