\name{ResampleLayers}
\alias{ResampleLayers}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Resample the input layers (i.e. changes resolution)
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
the function resamples all layers in the list except the reference, which is specified by the \code{ref_index} argument. The procedure changes the resolution of the processed layers.
}
\usage{
ResampleLayers(rasters, ref_index = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rasters}{
%%     ~~Describe \code{rasters} here~~
a list of \code{raster} objects
}
  \item{ref_index}{
%%     ~~Describe \code{ref_index} here~~
index of the raster layer to be used as reference for resampling. By default=1 (first element of the list)
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
a list of layers with updated resolution
}

\author{
%%  ~~who you are~~
Massimiliano Pittore
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
r1 <- system.file("extdata/p1dens.tif", package="focusmapr")
r2 <- system.file("extdata/p2dens.tif", package="focusmapr")
layers <- LoadRasterLayers(c(r1,r2))
# use the resolution of the second layer for resampling the first
resamp_layers<-ResampleLayers(layers,ref_index=2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ resolution }
\keyword{ resampling }% __ONLY ONE__ keyword per line
