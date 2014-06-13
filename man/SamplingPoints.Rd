\name{SamplingPoints}
\alias{SamplingPoints}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generates a set of sampling points according to a density distribution
%%  ~~function to do ... ~~
}
\description{
A set of spatial points is generated using the input raster as spatial density distribution. The function created an inhomogeneous Poisson Point Process to generate the points. A scaling coefficient is used to control the final amount of sampling points.
}
\usage{
SamplingPoints(rast, coef)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rast}{
%%     ~~Describe \code{rast} here~~
input raster defining the spatial density of probability of sampling. A \link{FocusMap}{focus map} is typically used 
}
  \item{coef}{
%%     ~~Describe \code{coef} here~~
scaling coefficient to tune the number of generated points
}
}

\value{
A set of sampling points.
}

\author{Massimiliano Pittore}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ sampling }
