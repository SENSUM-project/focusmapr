\name{NormalizeLayer}
\alias{NormalizeLayer}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
NormalizeLayer(rast, norm_type = "linear", rej = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rast}{
%%     ~~Describe \code{rast} here~~
}
  \item{norm_type}{
%%     ~~Describe \code{norm_type} here~~
}
  \item{rej}{
%%     ~~Describe \code{rej} here~~
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
function (rast, norm_type = "linear", rej = NULL) 
{
    if (norm_type == "linear") {
        if (!is.null(rej)) {
            if (rej[1] < 0 | rej[2] > 100 | rej[1] >= rej[2]) 
                stop("error in the rejection bound")
            qq <- quantile(rast, prob = rej)
            min <- qq[1]
            max <- qq[2]
        }
        else {
            qq <- quantile(rast, prob = c(0, 1))
            min <- qq[1]
            max <- qq[2]
        }
        if (max < 1e-08) 
            stop("upper normalization value < 1e-8 (almost zero)")
        rast[rast < min] <- min
        rast[rast > max] <- max
        out <- calc(rast, fun = function(x) {
            (x - min)/(max - min)
        })
        return(out)
    }
    else stop(paste("unknown normalization type: ", norm_type, 
        sep = ""))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
