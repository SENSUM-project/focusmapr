\name{ResampleLayers}
\alias{ResampleLayers}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ResampleLayers(rasters, ref_index = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rasters}{
%%     ~~Describe \code{rasters} here~~
}
  \item{ref_index}{
%%     ~~Describe \code{ref_index} here~~
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
function (rasters, ref_index = NULL) 
{
    ref <- 1
    f <- function(x) {
        print(paste("resampling ", x@data@names, sep = ""))
        resample(x, rasters[[ref]])
    }
    c(rasters[ref], lapply(rasters[-ref], FUN = f))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
