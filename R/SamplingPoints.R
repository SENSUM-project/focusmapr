SamplingPoints <-
function(rast,coef)
{
	mbox<-bbox(rast)
	mat<-as.matrix(flip(rast,'y'))
	ndens<-as.im(mat*coef)
	Z<-rpoispp(ndens)
	#rescale the points on the raster extent
	x<-Z$x*((mbox[1,2]-mbox[1,1])/ndens$xrange[2])+mbox[1,1]
	y<-(Z$y)*((mbox[2,2]-mbox[2,1])/ndens$yrange[2])+mbox[2,1]
	return(SpatialPoints(cbind(x,y)))
}
