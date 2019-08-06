#' Produce a \code{data.frame} containing the structure and associated attributes
#'
#' Produce a \code{data.frame} containing the node (vertex) and link (edge)
#' structure as well as the associated attributes for these.
#' This provides an easy way to create data tables describing graphs, particularly
#' helpful for users unfamiliar with the structure of \code{igraph} objects.
#'
#' @param x    A \code{goc}, \code{mpg}, \code{igraph}, or \code{list} object.
#'
#' @param ...  Additional arguments (not used).
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
#' @author Paul Galpern and Alex Chubaty
#' @export
#' @importFrom utils type.convert
#' @include classes.R
#' @rdname graphdf
#' @seealso \code{\link{MPG}}, \code{\link{GOC}}
#'
#' @example inst/examples/example_preamble.R
#' @example inst/examples/example_preamble_MPG.R
#' @example inst/examples/example_preamble_GOC.R
#' @example inst/examples/example_graphdf.R
#'
setGeneric("graphdf", function(x, ...) {
  standardGeneric("graphdf")
})

#' @export
#' @rdname graphdf
setMethod(
  "graphdf",
  signature = "list",
  definition = function(x, ...) {
    results <- vector("list", length(x))

    for (i in 1:length(x)) {
      thisGraph <- x[[i]]

      if (is_igraph(thisGraph))  {
        results[[i]] <- list()
        results[[i]]$v <- data.frame(sapply(names(vertex_attr(thisGraph)), function(z) {
          vertex_attr(thisGraph, z)
        }), stringsAsFactors = FALSE)
        results[[i]]$e <- data.frame(as_edgelist(thisGraph),
                                     sapply(names(edge_attr(thisGraph)), function(z) {
          edge_attr(thisGraph, z)
        }), stringsAsFactors = FALSE)
        edgeDfNames <- names(results[[i]]$e)
        names(results[[i]]$e) <- c("e1", "e2", edgeDfNames[3:length(edgeDfNames)])

        ## Clean-up storage mode structure of data.frames
        results[[i]]$e <- as.data.frame(sapply(results[[i]]$e, as.character),
                                        stringsAsFactors = FALSE)
        results[[i]]$v <- as.data.frame(sapply(results[[i]]$v, as.character),
                                        stringsAsFactors = FALSE)
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
})

#' @export
#' @rdname graphdf
setMethod(
  "graphdf",
  signature = "goc",
  definition = function(x, ...) {
    theseGraphs <- lapply(x@th, function(z) z$goc)
    graphdf(theseGraphs)
})

#' @export
#' @rdname graphdf
setMethod(
  "graphdf",
  signature = "grain",
  definition = function(x, ...) {
    theseGraphs <- list(x@th)
    graphdf(theseGraphs)
})

#' @export
#' @rdname graphdf
setMethod(
  "graphdf",
  signature = "mpg",
  definition = function(x, ...) {
    theseGraphs <- vector("list", 1)
    theseGraphs[[1]] <- x@mpg
    graphdf(theseGraphs)
})

#' @export
#' @rdname graphdf
setMethod(
  "graphdf",
  signature = "igraph",
  definition = function(x, ...) {
    theseGraphs <- vector("list", 1)
    theseGraphs[[1]] <- x
    graphdf(theseGraphs)
})
