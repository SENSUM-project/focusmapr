\name{NormalizeLayer_log}
\alias{NormalizeLayer_log}
%- Also NEED an '\alias' for EACH other topic documented here.
		\title{
			Performs a normalization of input layer through logarithmic transformation 
			%%  ~~function to do ... ~~
		}
\description{
	Normalizes a single input raster. Normalization maps the raster values using a logarithmic mapping to the interval [0,1]. If present, it uses a rejection bounds specified by the argument \code{rej}. If rejection bounds are used, normalization is applied only within the percentiles defined by the rejection bounds. Rejection bounds are useful to avoid bias from outliers. 
	The logarithmic transformation uses two parameters according to the formula \code{y=beta0+beta1*log(x)}. If not specified, these parameters are computed automatically. 
}
\usage{
	NormalizeLayer_log(rast, rej = NULL, beta0=NULL, beta1=NULL)
}
%- maybe also 'usage' for other objects documented here.
		\arguments{
			\item{rast}{
				Input \code{raster} layer
			}
			\item{norm_type}{
				Type of normalization. Currently only \code{norm_type="linear"} is implemented.}
			\item{rej}{
				Rejection bounds. Specify two percentiles outside which normalization is not applied. if \code{rej=c(0,1)} is applied, this is equivalent to simple normalization.
			}
			\item{beta0}{
				Intercept of the linear model \code{y=beta0+beta1*log(x)} specifying the logarithmic mapping.
			}
			\item{beta1}{
				Coefficient of the linear model \code{y=beta0+beta1*log(x)} specifying the logarithmic mapping.
			}
		}

\value{
	Normalized \code{raster} layer
}

\author{Massimiliano Pittore}

%% ~Make other sections like Warning with \section{Warning }{....} 

\examples{
# loading two raster layers from disk
	r1 <- system.file("extdata/p1dens.tif", package="focusmapr")
	r2 <- system.file("extdata/p2dens.tif", package="focusmapr")
	layers <- LoadRasterLayers(c(r1,r2),resamp=TRUE)
# normalize the layers with a 0.01 rejection bound
	norm_layers<-lapply(layers,FUN=function(x) NormalizeLayer_log(x,rej=c(0.01,0.99)))
	
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
		% R documentation directory.
		\keyword{ ~normalization }




