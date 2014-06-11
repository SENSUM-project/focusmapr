MosaicRasters <-
function(paths)
{
	input.ras <- lapply(paths,raster)
	#TODO: check that they have same resolution and projection
	input.arg<-input.ras
	names(input.arg)<-NULL
	input.arg$fun<-max
	output.ras<-do.call(mosaic,input.arg)
	return(output.ras)
}
