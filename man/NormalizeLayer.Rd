\name{NormalizeLayer}
\alias{NormalizeLayer}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Performs a mapping of input layer through normalization
%%  ~~function to do ... ~~
}
\description{
Normalizes a set of input rasters. Normalization either simply re-scales the raster values to fit the interval [0,1], or uses a rejection bounds specified by the argument \code{rej}. If rejection bounds are used, normalization is applied only within the percentiles defined by the rejection bounds. Rejection bounds are useful to avoid bias from outliers.
}
\usage{
NormalizeLayer(rast, norm_type = "linear", rej = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rast}{
list of input \code{raster} layers
}
  \item{norm_type}{
Type of normalization. Currently only \code{norm_type="linear"} is implemented.}
  \item{rej}{
Rejection bounds. Specify two percentiles outside which normalization is not applied. if \code{rej=c(0,1)} is applied, this is equivalent to simple normalization.
}
}

\value{
a list of normalized \code{raster} layers
}

\author{Massimiliano Pittore}

%% ~Make other sections like Warning with \section{Warning }{....} 

\examples{
# loading two raster layers from disk
r1 <- system.file("extdata/p1dens.tif", package="focusmapr")
r2 <- system.file("extdata/p2dens.tif", package="focusmapr")
layers <- LoadRasterLayers(c(r1,r2))
# normalize the layers with a 0.01 rejection bound
norm_layers<-lapply(layers,FUN=function(x) NormalizeLayer(x,rej=c(0.01,0.99)))
	
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~normalization }
