ResampleLayers <-
function(rasters,ref_index=NULL)
{
	ref<-1
	f<-function(x)
	{
		print(paste("resampling ",x@data@names,sep=''))
		resample(x,rasters[[ref]])
	}
	c(rasters[ref],lapply(rasters[-ref],FUN=f))
}
