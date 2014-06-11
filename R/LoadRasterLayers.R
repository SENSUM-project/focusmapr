LoadRasterLayers <-
function(layers_paths,repro=FALSE,resamp=FALSE,flip=FALSE)
{
	#bulk load all layers in a list
	input.ras <- lapply(layers_paths,raster)
	#flip vertically to compensate a problem of the loader
	if (flip) input.ras<-lapply(input.ras,FUN=function(x){flip(x,'y')})
	
	if (repro)
	{
		# reproject them into a lon-lat 
		f<-function(x)
		{
			if (! isLonLat(x))
			{
			crs<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
			print(paste("reprojecting layer from ",projection(x)," to lat-lon (WGS84)",sep=''))
			return(projectRaster(x,crs=crs))
			}
			else return(x)
		}
		out<-lapply(input.ras,FUN=f)
		if (resamp) return(ResampleLayers(out)) 
		else return(out)
	}
	else if (resamp) return(ResampleLayers(input.ras))
	else return(input.ras)
}
