\name{gsGOC}
\alias{gsGOC}
\title{
Produce a grains of connectivity model at multiple scales (patch-based or lattice GOC)
}
\description{
Given a \code{\link{gsMPG}} object produce a grains of connectivity (GOC) model at multiple scales (resistance thresholds) by scalar analysis.  Patch-based or lattice GOC modelling can be done with this function.
}
\usage{
gsGOC(gsMPG, nThresh = NULL, doThresh = NULL, weight = "lcpPerimWeight",
    sp = FALSE, verbose = 3)
}
\arguments{
  \item{gsMPG}{
  A \code{gsMPG} object produced by \code{\link{gsMPG}}.  For lattice GOC \code{\link{gsMPG}} must be run with patch set as an integer value.
}
  \item{nThresh}{
  Optional.  An integer giving the number of thresholds (or scales) at which to create GOC models.  Thresholds are selected to produce a maximum number
  of unique grains (i.e. models). \code{nThresh} thresholds are also approximately evenly spread between 0 and the threshold at which all
  patches or focal points on the landscape are connected.  This is a simple way to get a representative subset of all possible GOC models.
  Provide either \code{nThresh} or \code{doThresh} not both.
}
  \item{doThresh}{
  Optional.  A vector giving the link thresholds at which to create GOC models.  Use \code{link{gsThreshold}} to identify thresholds of interest.
  Provide either \code{nThresh} or \code{doThresh} not both.
}
  \item{weight}{
  A string giving the link weight or attribute to use for threshold.  \code{"lcpPerimWeight"} uses the accumulated resistance or least-cost path
  distance from the perimeters of patches as the link weight.  \code{"eucPerimWeight"} use the Euclidean distance from the perimeters of patches as
  the link weight.
}
  \item{sp}{
  Logical.  If \code{TRUE} the \code{rgeos} package is used to create a vector of class\cr \code{\link{SpatialPolygonsDataFrame}} describing the finest
  grain of connectivity.  This is very useful for visualizing grains of connectivity models, especially for print purposes.  Equally, using the
  \code{maptools} or \code{rgdal} packages these polygons can be exported as shapefiles for use in other GIS applications.  But, please
  see details.
}
  \item{verbose}{
  Set \code{verbose=1} for no progress information to console.
}
}
\details{
This function can take a long time to run when \code{sp=TRUE}.  Time taken is dependent on the dimensions of the \code{gsMPG$voronoi} raster.  Also,
as of this release (May, 2012) there was still a memory leak in \code{rgeos} caused by its parent \code{GEOS} library.  In extreme circumstances \code{sp=TRUE}
may fail or cause a crash of the R process.
}
\value{
A \code{gsGOC} object, consisting of a list of objects.\cr\cr
The main elements:\cr
\code{$voronoi} is a raster describing the regions of proximity in resistance units around the focal patches or points (\code{RasterLayer})\cr
\code{$voronoiSP} is a vector representation of these regions of proximity (\code{SpatialPolygons}; if \code{sp=TRUE})\cr
\code{$summary} summarizes the grains of connectivity generated and their properties\cr
\code{$th} is a list of length \code{nThresh} or \code{length(doThresh)} giving the GOC graph at each threshold.\cr\cr
Each element of \code{$th} contains a \code{$goc} object giving the GOC graph as class \code{\link{igraph}}.  Vertex attributes describes
qualities of each polygon including the coordinates of each polygon centroid, the area of these polygons, and the original patch IDs in the MPG that
are included in each polygon.  All areal measurements are given as raster cell counts.  
A variety of edge attributes are also given in the GOC graph.  See \code{\link{gsGOCDistance}} for more information.
}
\note{
Researchers should consider whether the use of a patch-based
GOC or a lattice GOC model is appropriate based on the patch-dependency of the organism under study.  Patch-based models make
most sense when animals are restricted to, or dependent on, a resource patch.  Lattice models can be used as a generalized
and functional approach to scaling resistance surfaces.\cr

See \code{\link{gsMPG}} for warning related to areal measurements.
}
\references{
Fall, A., M.-J. Fortin, M. Manseau, D. O'Brien.  (2007) Spatial graphs:  Principles and applications for habitat connectivity.  Ecosystems.  10:448:461\cr\cr
Galpern, P., M. Manseau, P.J. Wilson. (2012) Grains of connectivity: analysis at multiple spatial scales in landscape genetics.  Molecular Ecology 21:3996-4009.\cr
}
\author{
Paul Galpern (\email{pgalpern@gmail.com})
}


\seealso{
\code{\link{gsMPG}}, \code{\link{gsGOCVisualize}}, \code{\link{gsGOCDistance}}, \code{\link{gsGOCPoint}}
}
\examples{

\dontrun{

## Load raster landscape
tiny <- raster(system.file("extdata/tiny.asc", package="grainscape"))

## Create a resistance surface from a raster using an is-becomes reclassifyifyification
tinyCost <- reclassifyify(tiny, rcl=cbind(c(1, 2, 3, 4), c(1, 5, 10, 12)))

## Produce a patch-based MPG where patches are resistance features=1
tinyPatchMPG <- gsMPG(cost=tinyCost, patch=tinyCost==1)


## Extract a representative subset of 5 grains of connectivity
tinyPatchGOC <- gsGOC(tinyPatchMPG, nThresh=5)

## Examine the properties of the GOC graph of grain 3 of 5
print(tinyPatchGOC$th[[3]]$goc, vertex=TRUE, edge=TRUE)

## Extract specified grains of connectivity and produce a vector SpatialPolygons
## representation of the finest grain of connectivity (Threshold=0)
tinyPatchGOC <- gsGOC(tinyPatchMPG, doThresh=c(0, 20, 40), sp=TRUE)

}



}