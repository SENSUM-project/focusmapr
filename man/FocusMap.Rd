\name{FocusMap}
\alias{FocusMap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
create a new \code{raster} object representing a Focus Map from a list of input layers
%%  ~~function to do ... ~~
}
\description{
the function applies a pooling method to the list of input rasters, returning the result in form of a raster with same extent and resolution. 
The pooling methods are specified by the argument \code{pooling}
}
\usage{
FocusMap(layers, pooling = "loglinear", weigths)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{layers}{
%%     ~~Describe \code{layers} here~~
A list of \code{raster} objects with same resolution
}
  \item{pooling}{
%%     ~~Describe \code{pooling} here~~
The desired pooling method. Possible values are
\item{linear}{additive pooling}
\item{loglinear}{multiplicative pooling}
}
  \item{weigths}{
%%     ~~Describe \code{weigths} here~~
a vector of (real numbers) weights. The lenght of the vector is equal to the number of input layers. Weights sum to one for the linear pooling, and are not constrained in case of loglinear pooling.
}
}

\value{
a raster with same extent of the intersection of input layers representing the estimated focus map
}

\author{ Massimiliano Pittore }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{

# loading two raster layers from disk
r1 <- system.file("extdata/p1dens.tif", package="focusmapr")
r2 <- system.file("extdata/p2dens.tif", package="focusmapr")
layers <- LoadRasterLayers(c(r1,r2),resamp=TRUE)
# normalize the layers
norm_layers <- lapply(layers,NormalizeLayer)

# create the focus map with a multiplicative pooling and inequal weights
focmap1 <- FocusMap(norm_layers,pooling="loglinear",weigths=c(.7,.3))

# create another focus map with a additive pooling and equal weights
focmap2 <- FocusMap(norm_layers,pooling="linear",weigths=c(.5,.5))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ focus map }
