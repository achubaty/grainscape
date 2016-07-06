\name{gsGOCPoint}
\alias{gsGOCPoint}
\title{
Identify the polygons containing locations in grains of connectivity (GOC) tessellations
}
\description{
Given a \code{\link{gsGOC}} object identify the polygon containing a location at multiple scales.  
}
\usage{
gsGOCPoint(gsGOC, coords)
}
\arguments{
  \item{gsGOC}{
  A \code{gsGOC} object produced by \code{\link{gsGOC}}
}
  \item{coords}{
  A two column matrix or a \code{\link{SpatialPoints}} object giving the coordinates of points of interest
}
}

\value{
A list object with elements:
\code{$pointPolygon} is a matrix with elements giving the id of the polygon from the \code{gsGOC}, where rows give points of interest and columns give thresholds\cr
\code{$pointTotalPatchArea} is a matrix with elements giving the area of patches in a polygon (in cell counts), where rows give points of interest and columns give thresholds\cr
\code{$pointTotalCoreArea} is the same for core area of patches\cr
\code{$pointECS} gives the patch area (in cell counts) averaged for all points of interest (defined by O'Brien et al. 2006)\cr
\code{$pointECSCore} is the same for the core area of patches\cr
}
\references{
Fall, A., M.-J. Fortin, M. Manseau, D. O'Brien.  (2007) Spatial graphs:  Principles and applications for habitat connectivity.  Ecosystems.  10:448:461\cr\cr
Galpern, P., M. Manseau, P.J. Wilson. (2012) Grains of connectivity: analysis at multiple spatial scales in landscape genetics.  Molecular Ecology 21:3996-4009.\cr\cr
O'Brien, D., M. Manseau, A. Fall, and M.-J. Fortin. (2006) Testing the importance of spatial configuration of winter habitat for woodland caribou: An application of graph theory. Biological Conservation 130:70-83.\cr

}
\author{
Paul Galpern (\email{pgalpern@gmail.com})
}
\note{
See \code{\link{gsMPG}} for warning related to areal measurements.
}

\seealso{
\code{\link{gsGOC}}, \code{\link{gsGOCDistance}}
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

## Three sets of coordinates in the study area
loc <- cbind(c(30, 60, 90), c(30, 60, 90))

## Find the GOC polygon containing these three locations
## for each of the 5 grains of connectivity
tinyPts <- gsGOCPoint(tinyPatchGOC, loc)
}
}