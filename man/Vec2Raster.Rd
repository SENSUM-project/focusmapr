\name{Vec2Raster}
\alias{Vec2Raster}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert a vector (shapefile) layer into a raster layer
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Vec2Raster(vec, ras_attr, res)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vec}{
%%     ~~Describe \code{vec} here~~
spatial object of type \code{SpatialPolygonsDataFrame} with a set of numeric attributes (at least one) 
}
  \item{ras_attr}{
%%     ~~Describe \code{ras_attr} here~~
attribute of the vector layer used to generate the raster. The attribute must exist, and be a number
}
  \item{res}{
%%     ~~Describe \code{res} here~~
resolution of the output raster. This argument must be provided. The resolution must be specified in the same unit of measure related to the input CRS.
}
}
\details{
The drivers available will depend on the installation of GDAL/OGR, and can vary; the ogrDrivers() function shows which are available, and which may be written (but all are assumed to be readable). Note that stray files in data source directories (such as *.dbf) may lead to spurious errors that accompanying *.shp are missing.
}
\value{
a raster with same extent as input vector, and resolution specified by the user.
}

\author{Massimiliano Pittore}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ vector }
