NormalizeLayer <-
function(rast,norm_type="linear",rej=NULL)
{
	if (norm_type=="linear")
	{
		if (! is.null(rej))
		{
			if (rej[1]<0 | rej[2]>100 | rej[1]>= rej[2]) stop('error in the rejection bound')
			qq<-quantile(rast,prob=rej)
			min<-qq[1]
			max<-qq[2]
		}
		else
		{
			qq<-quantile(rast,prob=c(0,1))
			min<-qq[1]
			max<-qq[2]
		}
		if (max<1e-8) stop('upper normalization value < 1e-8 (almost zero)')
		
		out<-calc(rast,fun=function(x) {(x-min)/(max-min)})
		#clip output
		out[out<0]<-0
		out[out>1]<-1
		
		return(out)
	}
	else stop (paste("unknown normalization type: ",norm_type,sep=''))
}
