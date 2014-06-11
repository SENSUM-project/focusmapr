\name{FocusMap}
\alias{FocusMap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
FocusMap(layers, pooling = "loglinear", weigths)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{layers}{
%%     ~~Describe \code{layers} here~~
}
  \item{pooling}{
%%     ~~Describe \code{pooling} here~~
}
  \item{weigths}{
%%     ~~Describe \code{weigths} here~~
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
function (layers, pooling = "loglinear", weigths) 
{
    if (pooling == "loglinear") {
        out <- log(layers[[1]] + 1e-05) * weigths[1]
        for (i in 2:length(layers)) {
            out <- out + log(layers[[i]] + 1e-05) * weigths[i]
        }
        return(exp(out))
    }
    else if (pooling == "linear") {
        out <- layers[[1]] * weigths[1]
        for (i in 2:length(layers)) {
            out <- out + layers[[i]] * weigths[i]
        }
        return(out)
    }
    else stop(paste("unknown pooling type: ", pooling, sep = ""))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
