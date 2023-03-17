# Resampling
# define generic function depending on whether a package namespace is present
if(requireNamespace("terra", quietly = TRUE)){
	setGeneric("resample", def=terra::resample)
}else{
	setGeneric(
		name="resample",
		def=function(x,y,...){
			standardGeneric("resample")
		}
	)
}


#' Resampling of data involving a \code{\link{trigrid}} or a \code{\link{hexagrid}} object.
#'
#' The function is used to resolve and resample data stored in \code{SpatRaster}s and \code{\link{facelayer}}s so they can be fitted to and can be plotted by using \code{\link{trigrid}} or \code{\link{hexagrid}} objects.
#'
#' This method is necessary to utilize rasterized data in the \code{\link{icosa}} package. The only method currently implemented upscales the raster data and then resolves the values to the \code{\link{trigrid}} or \code{\link{hexagrid}} values, using averages. In the case of resampling \code{\link[terra:rast]{SpatRaster}}s, the \code{method} argument will be passed to the \code{\link[terra]{resample}} function. 
#' @rdname resample
"resample"

# Resample method of trigrid
#' @param na.rm (\code{logical}) If a face contains a missing value, should its value be \code{NA} as well (\code{FALSE}) or calculate the mean anyway (\code{TRUE}).
#' @rdname resample
#' @exportMethod resample
setMethod(
	"resample",
	signature=c("SpatRaster", "trigrid"),
	definition=function(x,y, method="near", na.rm=TRUE){
		
		if(!requireNamespace("terra", quietly = TRUE)) stop("Install the 'terra' package to run this function.")
		
		# copy the raster
		x2<-x
		
		#determine up
		if(y@edgeLength[2]<=max(terra::res(x))*4){
			up<-round(max(terra::res(x))/y@edgeLength[2]*4)
			#set the upscaling
			terra::res(x2)<-terra::res(x)/up
			#resample the original raster
			x3<-terra::resample(x, x2, method=method)
		}else{
			x3<-x2
		}
		
		
		# calculate the coordinates
		# resolution
		resX<-(terra::ext(x3)[2]-terra::ext(x3)[1])/dim(x3)[2]
		resY<-(terra::ext(x3)[4]-terra::ext(x3)[3])/dim(x3)[1]
		
		# coordinates of columns and rows
		xCoords <- seq(terra::ext(x3)[1]+(resX/2), terra::ext(x3)[2]-resX/2, resX)
		yCoords <- rev(seq(terra::ext(x3)[3]+(resY/2), terra::ext(x3)[4]-resY/2, resY))
		
		#table format
		xVals<-rep(xCoords, length(yCoords))
		yVals<-rep(yCoords, each=length(xCoords))
		coords<-cbind(xVals,yVals)
		
		#look up where the coordinates are in the new grid
		cells<-locate(y, coords)
		
		# the new values in the triangular grid
		mVal<-tapply(INDEX=cells, X=terra::values(x3), mean, na.rm=na.rm)
		
		return(mVal)
	}
	
)

