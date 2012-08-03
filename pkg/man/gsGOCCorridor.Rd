\name{gsGOCCorridor}
\alias{gsGOCCorridor}
\title{
Visualize corridors between two points using a grains of connectivity (GOC) tessellation at a given scale in vector format
}
\description{
Given a series of GOC models built at different scales in a \code{gsGOC} object, visualize the corridor (or shortest path) between two points
using one of the tessellations (i.e. scales) in these models.  Visualization is exclusively in vector format.  \code{\link{gsGOC}} must be run
using the \code{sp=TRUE} option.
}
\usage{
gsGOCCorridor(gsGOC, whichThresh, coords, doPlot=FALSE, weight="meanWeight")
}
\arguments{
  \item{gsGOC}{
  A \code{gsGOC} object created by \code{\link{gsGOC}}
}
  \item{whichThresh}{
  Integer giving the index of the threshold to visualize.
}
 \item{coords}{
  A two column matrix or a \code{\link{SpatialPoints}} object giving coordinates at the end points of the corridor
}

  \item{doPlot}{
  Logical.  If \code{TRUE} plots a vector visualization of the corridor at the given scale.  For full
  control, manually produce plots using the outputs of this function.
  }
  item{weight}{
  The GOC graph link weight to use in calculating the distance.  Please see details in \code{\link{gsGOCDistance}}.
}
  
}

\value{
A list object:\cr\cr

\code{$voronoiSP} vector representation of polygons in the tessellation (\code{SpatialPolygonsDataFrame})\cr
\code{$linksSP} vector representation of links in the grains of connectivity graph (\code{SpatialLinesDataFrame})\cr
\code{$nodesSP} vector representation of the nodes in the grains of connectivity graph (\code{SpatialPoints})\cr
\code{$shortestLinksSP} vector representation of the links in the shortest path between coordinates (\code{SpatialLines})\cr
\code{$shortestNodesSP} vector representation of the nodes in the shortest path between coordinates (\code{SpatialPoints})\cr
\code{$corridorLength} gives the length of the shortest path between coordinates in accumulated resistance units\cr
}
\references{
Fall, A., M.-J. Fortin, M. Manseau, D. O'Brien.  (2007) Spatial graphs:  Principles and applications for habitat connectivity.  Ecosystems.  10:448:461\cr\cr
Galpern, P., M. Manseau, P.J. Wilson. (2012) Grains of connectivity: analysis at multiple spatial scales in landscape genetics.  Molecular Ecology.  In press.\cr

}
\author{
Paul Galpern (\email{pgalpern@gmail.com})
}

\seealso{
\code{\link{gsGOC} \code{\link{gsGOCVisualize}}}
}
\examples{

\dontrun{
## Load raster landscape
tiny <- raster(system.file("extdata/tiny.asc", package="grainscape"))

## Create a resistance surface from a raster using an is-becomes reclassification
tinyCost <- reclass(tiny, rcl=cbind(c(1, 2, 3, 4), c(1, 5, 10, 12)))


## Produce a patch-based MPG where patches are resistance features=1
tinyPatchMPG <- gsMPG(cost=tinyCost, patch=tinyCost==1)

## Extract a representative subset of 5 grains of connectivity using sp=TRUE
tinyPatchGOC <- gsGOC(tinyPatchMPG, nThresh=5, sp=TRUE)

## Quick visualization of a corridor
corridorStartEnd <- rbind(c(10,10), c(90,90))
gsGOCCorridor(tinyPatchGOC, whichThresh=3, coords=corridorStartEnd, doPlot=TRUE)

## More control over a corridor visualization
tinyPatchCorridor <- gsGOCCorridor(tinyPatchGOC, whichThresh=3, coords=corridorStartEnd)
plot(tinyPatchCorridor$voronoiSP, col="lightgrey", border="white", lwd=2)
plot(tinyPatchCorridor$linksSP, col="darkred", lty="dashed", add=TRUE)
plot(tinyPatchCorridor$nodesSP, col="darkred", pch=21, bg="white", add=TRUE)
plot(tinyPatchCorridor$shortestLinksSP, col="darkred", lty="solid", lwd=2, add=TRUE)
plot(tinyPatchCorridor$shortestNodesSP, col="darkred", pch=21, bg="darkred", add=TRUE)
mtext(paste("Corridor shortest path length:",
             round(tinyPatchCorridor$corridorLength, 2),
             "resistance units"), side=1)

}
}