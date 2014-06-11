\name{SamplingPoints}
\alias{SamplingPoints}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SamplingPoints(rast, coef)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rast}{
%%     ~~Describe \code{rast} here~~
}
  \item{coef}{
%%     ~~Describe \code{coef} here~~
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
function (rast, coef) 
{
    mbox <- bbox(rast)
    mat <- as.matrix(flip(rast, "y"))
    ndens <- as.im(mat * coef)
    Z <- rpoispp(ndens)
    x <- Z$x * ((mbox[1, 2] - mbox[1, 1])/ndens$xrange[2]) + 
        mbox[1, 1]
    y <- (Z$y) * ((mbox[2, 2] - mbox[2, 1])/ndens$yrange[2]) + 
        mbox[2, 1]
    return(SpatialPoints(cbind(x, y)))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
