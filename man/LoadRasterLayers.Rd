\name{LoadRasterLayers}
\alias{LoadRasterLayers}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
LoadRasterLayers(layers_paths, repro = FALSE, resamp = FALSE, flip = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{layers_paths}{
%%     ~~Describe \code{layers_paths} here~~
}
  \item{repro}{
%%     ~~Describe \code{repro} here~~
}
  \item{resamp}{
%%     ~~Describe \code{resamp} here~~
}
  \item{flip}{
%%     ~~Describe \code{flip} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (layers_paths, repro = FALSE, resamp = FALSE, flip = FALSE) 
{
    input.ras <- lapply(layers_paths, raster)
    if (flip) 
        input.ras <- lapply(input.ras, FUN = function(x) {
            flip(x, "y")
        })
    if (repro) {
        f <- function(x) {
            if (!isLonLat(x)) {
                crs <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
                print(paste("reprojecting layer from ", projection(x), 
                  " to lat-lon (WGS84)", sep = ""))
                return(projectRaster(x, crs = crs))
            }
            else return(x)
        }
        out <- lapply(input.ras, FUN = f)
        if (resamp) 
            return(ResampleLayers(out))
        else return(out)
    }
    else if (resamp) 
        return(ResampleLayers(input.ras))
    else return(input.ras)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
