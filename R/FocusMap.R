FocusMap <-
function(layers,pooling="loglinear",weigths)
{
	if (pooling=="loglinear")
	{
		out<-log(layers[[1]]+1e-5)*weigths[1]
		for (i in 2:length(layers))
		{
			out<-out+log(layers[[i]]+1e-5)*weigths[i]
		}
		return(exp(out))
	}
	else if (pooling=="linear")
	{
		out<-layers[[1]]*weigths[1]
		for (i in 2:length(layers))
		{
			out<-out+layers[[i]]*weigths[i]
		}
		return(out)
	}
	else stop(paste("unknown pooling type: ",pooling,sep=''))
}
