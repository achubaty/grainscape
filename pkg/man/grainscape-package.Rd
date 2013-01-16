\name{grainscape-package}
\alias{grainscape}
\docType{package}
\title{
Grains of connectivity and minimum planar graph modelling of landscape connectivity using resistance surfaces
}
\description{
Given a landscape resistance surface, creates grains of connectivity and minimum planar graph models that can be used to calculate
effective distances for landscape connectivity at multiple scales. Distributed with \code{SELES}
(Spatially Explicit Landscape Event Simulator; Fall and Fall, 2001) software. The package will currently run only on a Windows-based platform.
}
\details{
Landscape connectivity modelling to understand the movement and dispersal of organisms has been done using raster resistance surfaces and landscape graph methods.  Grains of connectivity (GOC) models combine elements of both approaches to produce a continuous and scalable tool that can be applied in a variety of study systems.  The purpose of this package is to implement grains of connectivity analyses.  Routines accept raster-based resistance surfaces as input and return raster, vector and graph-based data structures to represent connectivity at multiple scales.  Effective distances describing connectivity between geographic locations can be determined at multiple scales.  Analyses of this sort can contribute to corridor identification, landscape genetics, as well as other connectivity assessments.  Minimum planar graph (MPG; Fall et al., 2007) models of resource patches on landscapes can also be generated using the software.\cr

SELES software (http://seles.info/ Fall and Fall, 2001) is distributed with the package, and is used to produce the MPG and perform a generalization of the Voronoi tessellation used in GOC models.  Routines also depend on the \code{sp}, \code{raster}, \code{igraph} and optionally \code{rgeos} packages (Pebesma and Bivand, 2005, Csardi and Nepusz, 2006, Hijmans and van Etten, 2011, Bivand and Rundel, 2012).\cr\cr
An R vignette detailing the use of this package for landscape connectivity modelling is in preparation.\cr\cr
Please cite this package using the Galpern et al. (2012) reference below.

A detailed tutorial is available as a vignette.  If it is not attached with your package install, you can download it at http://grainscape.r-forge.r-project.org/.)

}
\author{
Paul Galpern, Andrew Fall, Micheline Manseau

Maintainer: Paul Galpern <pgalpern@gmail.com>
}
\references{
Primary references:\cr

Fall, A., M.-J. Fortin, M. Manseau, D. O'Brien.  Spatial graphs:  Principles and applications for habitat connectivity.  Ecosystems.  10:448:461.\cr\cr
Galpern, P., M. Manseau, P.J. Wilson. (2012) Grains of connectivity: analysis at multiple spatial scales in landscape genetics.  Molecular Ecology. 21:3996-4009.\cr\cr
Supporting packages and software:\cr

Bivand, R.S. and C. Rundel. (2012). rgeos: Interface to Geometry Engine - Open Source (GEOS). R package version 0.2-1.\cr\cr
Csardi, G. and T. Nepusz. (2006). The igraph software package for complex network research. InterJournal Complex Systems 1695.\cr\cr
Fall, A. and J. Fall. (2001). A domain-specific language for models of landscape dynamics. Ecological Modelling 141:1-18.\cr\cr
Hijmans, R.J. and J. van Etten. (2011). raster: Geographic analysis and modeling with raster data. R package version 1.9-58.\cr\cr
Pebesma, E.J. and R.S. Bivand. (2005). Classes and methods for spatial data in R. R News 5:http://cran.r-project.org/doc/Rnews/.\cr
}
\keyword{ package }
