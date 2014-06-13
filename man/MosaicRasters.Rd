\name{MosaicRasters}
\alias{MosaicRasters}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create a tile mosaic of input rasters
%%  ~~function to do ... ~~
}
\description{
Builds a single mosaic from a set of raster tiles. It uses the \code{do.Mosaic} function of the package \code{raster} (\seealso{\link{raster}})
}
\usage{
MosaicRasters(paths)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{paths}{
a list of input rasters
}
}
\value{
a single mosaic raster
}

\author{Massimiliano Pittore}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ mosaic }
