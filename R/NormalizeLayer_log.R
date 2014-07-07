# TODO: Add comment
# 
# Author: max
###############################################################################

NormalizeLayer_log <-
		function(rast,rej=NULL,beta0=NULL,beta1=NULL)
{
	#default calibration bounds
	min=minValue(rast)
	if (min<0) stop('to apply this normalization, indicator must be positive')
	max=maxValue(rast)
	
	#if provided, use quantiles to calibrate
	if (! is.null(rej))
	{
		if (rej[1]<0 | rej[2]>100 | rej[1]>= rej[2]) stop('error in the rejection bound')
		qq<-quantile(rast,prob=rej)
		min<-qq[1]
		max<-qq[2]
	}
	
	#if coefficients are not provided, calibrate mapping
	if (is.null(beta0) | is.null(beta1))
	{		
		#compute coefficients. minimum value clipped to 1e-5 to avoid problem with log
		calib=data.frame(ind=c(min+1e-5,max),prob=c(0,1))
		#compute calibration coefficients
		mlog<-lm(prob~log(ind),calib)
		beta0<-mlog$coefficients[1]
		beta1<-mlog$coefficients[2]
	}

	#perform mapping
	out<-calc(rast,fun=function(x) {beta0+log(x)*beta1})
	
	#clip output
	out[out<0]<-0
	out[out>1]<-1
	
	return(out)
}

