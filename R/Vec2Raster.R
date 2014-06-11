Vec2Raster <-
function(vec,ras_attr,res)
{
	#generate a raster with the right extent
	r <- raster(extent(vec))
	#set resolution
	res(r)<-res
	#rasterize
    ras<-rasterize(vec,r,field=ras_attr)
	return(ras)
}
