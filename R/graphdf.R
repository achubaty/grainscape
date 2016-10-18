#' Produce a \code{data.frame} containing the structure and associated attributes
#'
#' Produce a \code{data.frame} containing the node (vertex) and link (edge)
#' structure as well as the associated attributes for these.
#' This provides an easy way to create data tables describing graphs, particularly
#' helpful for users unfamiliar with the structure of \code{igraph} objects.
#'
#' @param x  A \code{goc}, \code{mpg}, \code{igraph}, or \code{list} object.
#'
#' @return A list object containing:
#'
#' \describe{
#'   \item{\code{v}}{node (vertex) names and associated attributes;}
#'
#'   \item{\code{e}}{link (edge) lists and associated attributes.}
#' }
#'
#' Please see \code{\link{MPG}} and \code{\link{GOC}} for details about the attributes.
#'
#' For \code{\link{GOC}} objects which typically contain multiple thresholds,
#' an enumerated list of the same length as the number of thresholds is returned
#' each containing \code{v} and \code{e} elements.
#'
#' @author Paul Galpern
#' @docType methods
#' @export
#' @importFrom igraph as_edgelist edge_attr is_igraph vertex_attr
#' @importFrom utils type.convert
#' @rdname graphdf
#' @seealso \code{\link{MPG}}, \code{\link{GOC}}
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
#' tinyPatchMPG <- MPG(cost = tinyCost, patch = tinyCost == 1)
#'
#' ## Extract a representative subset of 5 grains of connectivity
#' tinyPatchGOC <- GOC(tinyPatchMPG, nThresh = 5)
#'
#' ## Create a data.frame with the structure and attributes of a MPG object
#' tinyPatchMPG_df <- graphdf(tinyPatchMPG)
#'
#' ## Create a data.frame with the structure and attributes of a GOC object
#' tinyPatchGOC_df <- graphdf(tinyPatchGOC)
#'
#' ## Create a data.frame with the structure and attributes of any igraph object
#' graphdf(tinyPatchGOC$th[[1]]$goc)
#' }
#'
graphdf <- function(x) UseMethod("graphdf")



#' @export
#' @rdname graphdf
graphdf.list <- function(x) {
  results <- vector("list", length(x))

  for (i in 1:length(x)) {
    thisGraph <- x[[i]]

    if (is_igraph(thisGraph))  {
      results[[i]] <- list()
      results[[i]]$v <- data.frame(sapply(names(vertex_attr(thisGraph)), function(z) {
        vertex_attr(thisGraph, z)
      }), stringsAsFactors = FALSE)
      results[[i]]$e <- data.frame(as_edgelist(thisGraph), sapply(names(edge_attr(thisGraph)), function(z) {
        edge_attr(thisGraph, z)
      }), stringsAsFactors = FALSE)
      edgeDfNames <- names(results[[i]]$e)
      names(results[[i]]$e) <- c("e1", "e2", edgeDfNames[3:length(edgeDfNames)])

      ## Clean-up storage mode structure of data.frames
      results[[i]]$e <- as.data.frame(sapply(results[[i]]$e, as.character), stringsAsFactors = FALSE)
      results[[i]]$v <- as.data.frame(sapply(results[[i]]$v, as.character), stringsAsFactors = FALSE)
      results[[i]]$e <- as.data.frame(lapply(results[[i]]$e, function(z) {
        type.convert(z, as.is = TRUE)
      }), stringsAsFactors = FALSE)
      results[[i]]$v <- as.data.frame(lapply(results[[i]]$v, function(z) {
        type.convert(z, as.is = TRUE)
      }), stringsAsFactors = FALSE)
    } else {
      results[[i]]$v <- NA
      results[[i]]$e <- NA
    }
  }

  return(results)
}

#' @export
#' @rdname graphdf
graphdf.goc <- function(x) {
  theseGraphs <- lapply(x$th, function(x) x$goc)
  graphdf.list(theseGraphs)
}

#' @export
#' @rdname graphdf
graphdf.mpg <- function(x) {
  theseGraphs <- vector("list", 1)
  theseGraphs[[1]] <- x$mpg
  graphdf.list(theseGraphs)
}

#' @export
#' @rdname graphdf
graphdf.igraph <- function(x) {
  theseGraphs <- vector("list", 1)
  theseGraphs[[1]] <- x
  graphdf.list(theseGraphs)
}
