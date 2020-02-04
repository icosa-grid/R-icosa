# Resampling
# define generic function depending on whether a package namespace is present
if(requireNamespace("raster", quietly = TRUE)){
	setGeneric("resample", def=raster::resample)
}else{
	setGeneric(
		name="resample",
		def=function(x,y,...){
			standardGeneric("resample")
		}
	)
}


#' Resampling of data to a trigrid or a hexagrid object.
#'
#' The function is used to resolve and resample data stored in RasterLayers and facelayers so they can be fitted to and can be plotted by using trigrid or hexagrid objects.
#'
#' This method is necessary to utilize rasterized data in the icosa package. The only method currently implemented upscales the raster data and then resolve the values to the trigrid or hexagrid values, using averages. In the case of resampling rasterlayers, the method argument will be passed to the raster::resample() function. 
#' @param na.rm logical value. If a face contains a missing value, should its value be NA as well (FALSE) or calculate the mean anyway (TRUE).
#' @aliases Raster-trigrid-resample-method
#' @rdname resample-methods
#' @exportMethod resample
setMethod(
	"resample",
	signature=c("Raster", "trigrid"),
	definition=function(x,y, method="ngb", na.rm=TRUE){
		
		if(!requireNamespace("raster", quietly = TRUE)) stop("Install the 'raster' package to run this function.")
		
		# copy the raster
		x2<-x
		
		#determine up
		if(y@edgeLength[2]<=max(raster::res(x))*4){
			up<-round(max(raster::res(x))/y@edgeLength[2]*4)
			#set the upscaling
			raster::res(x2)<-raster::res(x)/up
			#resample the original raster
			x3<-raster::resample(x, x2, method)
		}else{
			x3<-x2
		}
		
		
		# calculate the coordinates
		# resolution
		resX<-(x3@extent@xmax-x3@extent@xmin)/x3@ncols
		resY<-(x3@extent@ymax-x3@extent@ymin)/x3@nrows
		
		# coordinates of columns and rows
		xCoords <- seq(x3@extent@xmin+(resX/2), x3@extent@xmax-resX/2, resX)
		yCoords <- rev(seq(x3@extent@ymin+(resY/2), x3@extent@ymax-resY/2, resY))
		
		#table format
		xVals<-rep(xCoords, length(yCoords))
		yVals<-rep(yCoords, each=length(xCoords))
		coords<-cbind(xVals,yVals)
		
		#look up where the coordinates are in the new grid
		cells<-locate(y, coords)
		
		# the new values in the triangular grid
		mVal<-tapply(INDEX=cells, X=values(x3), mean, na.rm=na.rm)
		
		return(mVal)
	}
	
)

