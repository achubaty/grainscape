#' Produce a `data.frame` containing the structure and associated attributes
#'
#' Produce a `data.frame` containing the node (vertex) and link (edge)
#' structure as well as the associated attributes for these.
#' This provides an easy way to create data tables describing graphs, particularly
#' helpful for users unfamiliar with the structure of `igraph` objects.
#'
#' @param x    A `goc`, `mpg`, `igraph`, or `list` object.
#'
#' @param ...  Additional arguments (not used).
#'
#' @return A list object containing:
#'
#' \describe{
#'   \item{`v`}{node (vertex) names and associated attributes;}
#'
#'   \item{`e`}{link (edge) lists and associated attributes.}
#' }
#'
#' Please see [MPG()] and [GOC()] for details about the attributes.
#'
#' For [GOC()] objects which typically contain multiple thresholds,
#' an enumerated list of the same length as the number of thresholds is returned
#' each containing `v` and `e` elements.
#'
#' @author Paul Galpern and Alex Chubaty
#' @export
#' @importFrom utils type.convert
#' @include classes.R
#' @rdname graphdf
#' @seealso [MPG()], [GOC()]
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

    for (i in seq_along(x)) {
      thisGraph <- x[[i]]

      if (is_igraph(thisGraph)) {
        results[[i]] <- list()
        results[[i]]$v <- names(vertex_attr(thisGraph)) |>
          sapply(FUN = function(z) vertex_attr(thisGraph, z)) |>
          data.frame(stringsAsFactors = FALSE)

        edgeAttr <- names(edge_attr(thisGraph)) |>
          sapply(FUN = function(z) edge_attr(thisGraph, z)) |>
          rbind()
        results[[i]]$e <- as_edgelist(thisGraph) |>
          data.frame(edgeAttr, stringsAsFactors = FALSE) |>
          stats::setNames(c("e1", "e2", colnames(edgeAttr)))

        ## Clean-up storage mode structure of data.frames
        results[[i]]$e <- sapply(results[[i]]$e, as.character) |>
          as.data.frame(stringsAsFactors = FALSE)
        results[[i]]$v <- sapply(results[[i]]$v, as.character) |>
          as.data.frame(stringsAsFactors = FALSE)
        results[[i]]$e <- lapply(results[[i]]$e, function(z) type.convert(z, as.is = TRUE)) |>
          as.data.frame(stringsAsFactors = FALSE)
        results[[i]]$v <- lapply(results[[i]]$v, function(z) type.convert(z, as.is = TRUE)) |>
          as.data.frame(stringsAsFactors = FALSE)
      } else {
        results[[i]]$v <- NA
        results[[i]]$e <- NA
      }
    }

    return(results)
  }
)

#' @export
#' @rdname graphdf
setMethod(
  "graphdf",
  signature = "goc",
  definition = function(x, ...) {
    theseGraphs <- lapply(x@th, function(z) z$goc)
    graphdf(theseGraphs)
  }
)

#' @export
#' @rdname graphdf
setMethod(
  "graphdf",
  signature = "grain",
  definition = function(x, ...) {
    theseGraphs <- list(x@th)
    graphdf(theseGraphs)
  }
)

#' @export
#' @rdname graphdf
setMethod(
  "graphdf",
  signature = "mpg",
  definition = function(x, ...) {
    theseGraphs <- vector("list", 1)
    theseGraphs[[1]] <- x@mpg
    graphdf(theseGraphs)
  }
)

#' @export
#' @rdname graphdf
setMethod(
  "graphdf",
  signature = "igraph",
  definition = function(x, ...) {
    theseGraphs <- vector("list", 1)
    theseGraphs[[1]] <- x
    graphdf(theseGraphs)
  }
)
