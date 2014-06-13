\name{LoadRasterLayers}
\alias{LoadRasterLayers}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
bulk load a set of raster layers from file
%%  ~~function to do ... ~~
}
\description{
Given a list of filenames (including path) the function loads the different raster layers, returning a list or object of type raster (from package raster). If needed, the function performs flippng, resample and reprojection of the individual layer to obtain an harmonized list.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
LoadRasterLayers(layers_paths, repro = FALSE, resamp = FALSE, flip = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{layers_paths}{
  list of filenames (including complete path) for the layers to be loaded. Supported file types are the ’native’ raster package
format and those that can be read via rgdal \seealso{\link{http://cran.at.r-project.org/web/packages/raster/raster.pdf#Rfn.readGDAL}{rgdal}}.
%%     ~~Describe \code{layers_paths} here~~
}
  \item{repro}{
%%     ~~Describe \code{repro} here~
logical, if YES reprojects the layer into a default WGS84 reference system
}
  \item{resamp}{
%%     ~~Describe \code{resamp} here~~
logical, if YES resamples all the raster layers following the resolution of the first layer. \seealso{\link{ResampleLayers}}.
}
  \item{flip}{
%%     ~~Describe \code{flip} here~~
logical, if YES a flipping of the input layer around the 'y' axis is performed.\note{The flip options is handy to fix the occasional flipping of rasters created with QGis.}
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
list of \code{raster} objects (\seealso{\link{raster}}).
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
Massimiliano Pittore
}

%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
# reads two raster files from a folder

# N.B.: For your own files, omit the 'system.file' and 'package="focusmapr"' bits 
# these are just to get the path to files installed with the package

r1 <- system.file("extdata/p1dens.tif", package="focusmapr")
r2 <- system.file("extdata/p2dens.tif", package="focusmapr")
layers<-LoadRasterLayers(c(r1,r2))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ raster }
