\name{gsGOCDistance}
\alias{gsGOCDistance}
\title{
Find the grains of connectivity network distance 
}
\description{
Given a \code{\link{gsGOC}} object find the shortest network distance between pairs of points using the GOC graph.  This can be used as an effective distance for landscape connectivity assessments.
}
\usage{
gsGOCDistance(gsGOC, coords, weight = "meanWeight")
}
\arguments{
  \item{gsGOC}{
  A \code{gsGOC} object produced by \code{\link{gsGOC}}
}
  \item{coords}{
  A two column matrix or a \code{\link{SpatialPoints}} object giving the coordinates of points of interest
}
  \item{weight}{
  The GOC graph link weight to use in calculating the distance.  Please see details for explanation.
}
}
\details{
GOC graphs with the following links weights can be used:\cr\cr
\code{weight="meanWeight"} links represent the mean weight of all links connecting MPG nodes in neighbouring polygons.  It is recommended.\cr
\code{weight="maxWeight"} links represent the maximum weight of all links connecting MPG nodes in neighbouring polygons.\cr
\code{weight="minWeight"} links represent the minimum weight of all links connecting MPG nodes in neighbouring polygons.\cr
\code{weight="medianWeight"} links represent the median weight of all links connecting MPG nodes in neighbouring polygons.\cr
\code{weight="numEdgesWeight"} links represent the number of links connecting MPG nodes in neighbouring polygons.\cr
\code{weight="eucCentroidWeight"} links represent the Euclidean distances between the centroids of neighbouring polygons.\cr
}
\value{
A list object giving a distance matrix for each threshold in the \code{gsGOC} object.
Distance matrices give the pairwise grains of connectivity network distances between sampling locations.
Matrix indices correspond to rows in the \code{coords} matrix.
}
\references{
Fall, A., M.-J. Fortin, M. Manseau, D. O'Brien.  (2007) Spatial graphs:  Principles and applications for habitat connectivity.  Ecosystems.  10:448:461\cr\cr
Galpern, P., M. Manseau, P.J. Wilson. (2012) Grains of connectivity: analysis at multiple spatial scales in landscape genetics.  Molecular Ecology.  In press.\cr
}
\author{
Paul Galpern (\email{pgalpern@gmail.com})
}


\seealso{
\code{\link{gsGOC}}, \code{\link{gsGOCPoint}}
}
\examples{

\dontrun{

## Load raster landscape
tiny <- raster(system.file("extdata/tiny.asc", package="grainscape"))

## Create a resistance surface from a raster using an is-becomes reclassification
tinyCost <- reclass(tiny, rcl=cbind(c(1, 2, 3, 4), c(1, 5, 10, 12)))

## Produce a patch-based MPG where patches are resistance features=1
tinyPatchMPG <- gsMPG(cost=tinyCost, patch=tinyCost==1)

## Extract a representative subset of 5 grains of connectivity
tinyPatchGOC <- gsGOC(tinyPatchMPG, nThresh=5)

## Three sets of coordinates in the study area
loc <- cbind(c(30, 60, 90), c(30, 60, 90))

## Find the GOC network distance matrices between these poitns
## for each of the 5 grains of connectivity
tinyDist <- gsGOCDistance(tinyPatchGOC, loc)
}
}