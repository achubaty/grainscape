\name{gsThreshold}
\alias{gsThreshold}
\title{
Produce a minimum planar graph (MPG) at multiple scales
}
\description{
This function performs a scalar analysis of a minimum planar graph (MPG) by building the graph at a series of link thresholds.  As the threshold value increases
more nodes in the graph become connected, forming increasingly fewer components, until the graph becomes connected (e.g. Brooks, 2003).   N.B. Grains of
connectivity (GOC) done by \code{\link{gsGOC}} is also a scalar analysis using Voronoi tessellations rather than patches (see Galpern et al., 2012). 
}
\usage{
gsThreshold(gsMPG, weight = "lcpPerimWeight", nThresh = NULL, doThresh = NULL)
}
\arguments{
  \item{gsMPG}{
  A \code{gsMPG} object produced by \code{\link{gsMPG}}.
}
  \item{weight}{
  A string giving the link weight or attribute to use for threshold.  \code{"lcpPerimWeight"} uses the accumulated resistance or least-cost path
  distance from the perimeters of patches as the link weight.  \code{"eucPerimWeight"} use the Euclidean distance from the perimeters of patches as
  the link weight.
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
}

\value{
A list object with the following elements:\cr
\code{$summary} summarizes the thresholded graphs generated and their properties\cr
\code{$th} is a list of length \code{nThresh} or \code{length(doThresh)} giving the thresholded graph (class \code{igraph}) at each threshold.\cr\cr
}
\note{
See \code{\link{gsMPG}} for warning related to areal measurements.
}
\references{
Brooks, C.P. (2003) A scalar analysis of landscape connectivity.  Oikos 102:433-439.\cr\cr
Fall, A., M.-J. Fortin, M. Manseau, D. O'Brien.  (2007) Spatial graphs:  Principles and applications for habitat connectivity.  Ecosystems.  10:448:461\cr\cr
Galpern, P., M. Manseau, P.J. Wilson. (2012) Grains of connectivity: analysis at multiple spatial scales in landscape genetics.  Molecular Ecology.  In press.\cr
}
\author{
Paul Galpern (\email{pgalpern@gmail.com})
}

\seealso{
\code{\link{gsMPG}}
}
\examples{

\dontrun{
# Load raster landscape
tiny <- raster(system.file("data/tiny.asc", package="grainscape"))

## Create a resistance surface from a raster using an is-becomes reclassification
tinyCost <- reclass(tiny, rcl=cbind(c(1, 2, 3, 4), c(1, 5, 10, 12)))

## Produce a patch-based MPG where patches are resistance features=1
tinyPatchMPG <- gsMPG(cost=tinyCost, patch=tinyCost==1)

## Threshold this graph at a representative subset of 10 thresholds
tinyThresh <- gsThreshold(tinyPatchMPG, nThresh=10)

## Examine the properties of one of these threshold graphs
print(tinyThresh$th[[7]], vertex=TRUE, edge=TRUE)
}
}