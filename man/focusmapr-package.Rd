\name{focusmapr-package}
\alias{focusmapr-package}
\alias{focusmapr}
\docType{package}
\title{
Computation of Focus Maps
}
\description{
The package implements the concept of Focus Maps, a raster description of the spatial data sampling probability (or "importance") as a result of a combination of several raster or vector input layers. Every input layer is "mapped" (usually normalized) and "pooled"
}
\details{
\tabular{ll}{
Package: \tab focusmapr\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-06-11\cr
License: \tab GPL \cr
}
}
\author{
Massimiliano Pittore
}

\keyword{ package }

\examples{

# loading two raster layers from disk
r1 <- system.file("extdata/p1dens.tif", package="focusmapr")
r2 <- system.file("extdata/p2dens.tif", package="focusmapr")
layers <- LoadRasterLayers(c(r1,r2),resamp=TRUE)
# normalize the layers
norm_layers <- lapply(layers,NormalizeLayer)
# create the focus map with a multiplicative pooling and inequal weights
focmap <- FocusMap(norm_layers,pooling="loglinear",weigths=c(.7,.3))

}
