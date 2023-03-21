#' Plot method for the \code{\link{trigrid}}, \code{\link{hexagrid}} or \code{\link{facelayer}} classes
#' 
#'	This function will invoke the \code{plot} method of the \code{\link[sf:sf]{sf}} or the  \code{\link[sp]{SpatialPolygons}} class.
#' @param x (\code{\link{trigrid}}, \code{\link{hexagrid}} or \code{\link{facelayer}}) The object to be plotted.
#' @param crs (\code{character} or \code{\link[sf:st_crs]{crs}}) A coordinate system for the transformation of coordinates.
#' @param ... Arguments passed to the \code{plot} function.
#' @rdname plot
#' @return The function has no return value.
#' @exportMethod plot
"plot"

#' Plot method for the trigrid object class
#' @rdname plot
setMethod(
	"plot",
	signature="trigrid",
	definition=function(x,crs=NULL,...){
		
		# detect the what is available 
		sfplot <- FALSE
		spplot <- FALSE
		# these are missing values if they are not yet done
		if(inherits(x@sf, "sf")) sfplot <- TRUE
		if(inherits(x@sp, "Spatial")) spplot <- TRUE

		# by default use the sf-method for plotting
		if(sfplot){
			if(!is.null(crs)){
				x@sf <-sf::st_transform(x@sf, crs)
			}
			# use the plot method
			plot(x@sf$geometry, ...)
		}
		
		# if the sf slot is not available, use sp
		if(spplot & (!sfplot)){

			# if the crs is given, use sf to do the transformation
			if(!is.null(crs)){
				x@sp <- methods::as(sf::st_transform(sf::st_as_sf(x@sp), crs), "Spatial")
			}
			# use the plot method

			sp::plot(x@sp, ...)
		}

		if(!sfplot & !spplot){
			stop("Both the @sp and @sf is empty. Use newsf() to add a 2d respresentation. ")
		}
	}
)

#' Lines method for the \code{trigrid} and \code{hexagrid} classes
#' 
#' This function will invoke the method of the \code{\link[sp]{SpatialPolygons}} class.
#'	This function will invoke the \code{lines} method of the \code{\link[sf:sf]{sf}} or the  \code{\link[sp]{SpatialPolygons}} class.
#' @param x (\code{\link{trigrid}}, \code{\link{hexagrid}}) Object.
#' @param crs (\code{character} or \code{\link[sf:st_crs]{crs}}) A coordinate system for the transformation of coordinates.
#' @param col Line colors - as in \code{\link[graphics:par]{par}}
#' @param lwd Line thickness - as in \code{\link[graphics:par]{par}}
#' @param lty Line type - as in \code{\link[graphics:par]{par}}
#' @param ... Arguments passed to the \code{\link[sp:panel]{sp.lines}} method.
#' @rdname lines-methods
#' @return The function has no return value.
#' @exportMethod lines
setMethod(
	"lines",
	signature="trigrid",
	definition=function(x,crs=NULL,col=1, lwd=1, lty=1, ...){

		plot(x, crs=crs, add=TRUE, border=col, lwd=lwd, lty=lty,...)
	
	}
)

#' Labels of grid vertices, faces and edges.
#' 
#' This function will show where the grid elements are located.
#' @param gridObj (\code{\link{trigrid}}, \code{\link{hexagrid}}) An icosahedral grid.
#' @param type (\code{character}) The type of element to be plotted: either \code{"f"} (faces), \code{"v"} (vertices) or  \code{"e"} (edges).
#' @param crs (\code{character} or \code{\link[sf:st_crs]{crs}}) A coordinate system for the transformation of coordinates.
#' @param ... Arguments passed to the \code{\link[graphics]{text}} function.
#' @return The function has no return value.
#' @export
#' @examples
#' gr <- hexagrid(sp=TRUE)
#' plot(gr)
#' gridlabs(gr)
gridlabs<-function(gridObj,type="f",crs=NULL,...){
	# center back to origin if not there already
		if(gridObj@center[1]!=0 | gridObj@center[2]!=0 | gridObj@center[3]!=0){
			gridObj<-translate(gridObj,-gridObj@center)
		}
		
	if(type=="f"){
		texts<-rownames(gridObj@faceCenters)
		coords<-CarToPol(gridObj@faceCenters, norad=TRUE, origin=gridObj@center)
	}
	
	if(type=="v"){
		texts<-rownames(gridObj@vertices)
		coords<-CarToPol(gridObj@vertices, norad=TRUE, origin=gridObj@center)
	}
	if(type=="e"){
		texts<-rownames(gridObj@edges)
		coord3d<-t(apply(gridObj@edges, 1, function(x){
			apply(gridObj@vertices[x,],2,mean)
		}))
		coords<-CarToPol(coord3d, norad=TRUE, origin=gridObj@center)
		
	}
	sfPoints <- sf::st_as_sf(as.data.frame(coords), crs=gridObj@crs, coords=c("long", "lat"))
	
	#transformation is necessary
	if(!is.null(crs)){
		sfPoints <- sf::st_transform(sfPoints, crs=crs)
	}

	# detach coordinates
	newCoords <- sf::st_coordinates(sfPoints)
	
	graphics::text(labels=texts, x=newCoords[,1], y=newCoords[,2],...)
	
}

			


#' Locate grid faces based on their positions on a map
#' 
#' The function returns which grid faces contain the points clicked in a plot.
#' 
#' @param gridObj (\code{\link{trigrid}} or \code{\link{hexagrid}}) The grid object.
#' @param n (\code{integer}) The number of points to be looked up.
#' @param output (\code{character}) Type of output: \code{"faces"} returns only the face names of the points, \code{"full"} returns the coordinates as well.
#' @param ... Arguments passed to the \code{\link[graphics]{locator}} function.
#' 
#' @export
#' @return A vector of \code{character} values, each corresponding to a face identifier.
cellocator <- function(gridObj,n, output="faces",...){
	pointset<- locator(n=n, ...)
	pointset <-cbind(pointset$x, pointset$y)
	cells <- locate(gridObj, pointset)

	if(output=="full"){
		retVal<- data.frame(pointset, cells, stringsAsFactors=FALSE)
	}
	if(output=="faces"){
		retVal<-cells
	}
	return(retVal)
}

