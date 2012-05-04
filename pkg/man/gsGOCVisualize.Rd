\name{gsGOCVisualize}
\alias{gsGOCVisualize}
\title{
Visualize grains of connectivity (GOC) tessellations at a given scale
}
\description{
Given a series of GOC models built at different scales in a \code{gsGOC} object, visualize one the tessellations (i.e. scales) in these models.
Visualization is by default in raster format.  Vector based visualization is also possible.
}
\usage{
gsGOCVisualize(gsGOC, whichThresh, sp = FALSE)
}
\arguments{
  \item{gsGOC}{
  A \code{gsGOC} object created by \code{\link{gsGOC}}
}
  \item{whichThresh}{
  Integer giving the index of the threshold to visualize.
}
  \item{sp}{
  Logical.  If \code{TRUE} then produce a \code{\link{SpatialPolygonsDataFrame}} representation of the selected threshold.  Requires also running \code{\link{gsGOC}} with
  \code{sp=TRUE}, and that the \code{rgeos} package is installed.
}
}

\value{
A list object:\cr\cr
\code{$summary} giving the properties of the visualized scale of the GOC model\cr
\code{$voronoi} giving the tessellation (\code{RasterLayer})\cr
\code{$centroids} the centroids of the polygons in the tessellation (\code{SpatialPoints})\cr
\code{$voronoiSP} vector representation of polygons in the tessellation (\code{SpatialPolygonsDataFrame}; if \code{sp=TRUE})
}
\references{
Fall, A., M.-J. Fortin, M. Manseau, D. O'Brien.  (2007) Spatial graphs:  Principles and applications for habitat connectivity.  Ecosystems.  10:448:461\cr\cr
Galpern, P., M. Manseau, P.J. Wilson. (2012) Grains of connectivity: analysis at multiple spatial scales in landscape genetics.  Molecular Ecology.  In press.\cr

}
\author{
Paul Galpern (\email{pgalpern@gmail.com})
}

\seealso{
\code{\link{gsGOC}}
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

## Visualize the model at the finest scale/grain/threshold
plot(gsGOCVisualize(tinyPatchGOC, whichThresh=1)$voronoi,
   col=sample(rainbow(100)), legend=FALSE)

## Extract a representative subset of 5 grains of connectivity for vector visualization
tinyPatchGOC <- gsGOC(tinyPatchMPG, nThresh=5, sp=TRUE)

## Visualize the model at a selected scale/grain/threshold using vector polygons
plot(tinyPatchMPG$patchId, col="grey", legend=FALSE)
plot(gsGOCVisualize(tinyPatchGOC, whichThresh=3, sp=TRUE)$voronoiSP,
  add=TRUE, lwd=2)

}


}