\name{gsGraphDataFrame}
\alias{gsGraphDataFrame}
\title{
Produce a data.frame containing the structure and associated attributes for a gsMPG, gsGOC, or igraph object
}
\description{
Given a gsMPG, gsGOC, or any igraph object produce a data.frame containing the node (vertex) and link (edge)
structure as well as the associated attributes for these.  This provides an easy way to create data tables
describing graphs, particularly helpful for users unfamiliar with the structure of igraph objects
}
\usage{
gsGraphDataFrame(gsObj)
}
\arguments{
  \item{gsObj}{
  A \code{gsMPG}, \code{gsGOC}, or \code{igraph} object}
}


\value{
A list object:\cr\cr
\code{$v} giving node (vertex) names and associated attributes\cr
\code{$e} giving link (edge) lists and associated attributes\cr\cr

Please see \code{\link{gsMPG}} and \code{\link{gsGOC}} for details about the attributes.\cr\cr

For \code{\link{gsGOC}} objects which typically contain multiple thresholds, an enumerated list
of the same length as the number of thresholds is returned each containing \code{$v} and \code{$e}
elements.
}
\references{
Fall, A., M.-J. Fortin, M. Manseau, D. O'Brien.  (2007) Spatial graphs:  Principles and applications for habitat connectivity.  Ecosystems.  10:448:461\cr\cr
Galpern, P., M. Manseau, P.J. Wilson. (2012) Grains of connectivity: analysis at multiple spatial scales in landscape genetics.  Molecular Ecology 21:3996-4009.\cr

}
\author{
Paul Galpern (\email{pgalpern@gmail.com})
}

\seealso{
\code{\link{gsMPG}}, \code{\link{gsGOC}}
}

\examples{

\dontrun{
## Load raster landscape
tiny <- raster(system.file("extdata/tiny.asc", package="grainscape"))

## Create a resistance surface from a raster using an is-becomes reclassification
tinyCost <- reclassify(tiny, rcl=cbind(c(1, 2, 3, 4), c(1, 5, 10, 12)))


## Produce a patch-based MPG where patches are resistance features=1
tinyPatchMPG <- gsMPG(cost=tinyCost, patch=tinyCost==1)

## Extract a representative subset of 5 grains of connectivity
tinyPatchGOC <- gsGOC(tinyPatchMPG, nThresh=5)

## Create a data.frame with the structure and attributes of a gsMPG object
tinyPatchMPG_df <- gsGraphDataFrame(tinyPatchMPG)

## Create a data.frame with the structure and attributes of a gsGOC object
tinyPatchGOC_df <- gsGraphDataFrame(tinyPatchGOC)

## Create a data.frame with the structure and attributes of any igraph object
gsGraphDataFrame(tinyPatchGOC$th[[1]]$goc)

}


}