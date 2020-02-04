# Conditional generic function for rgl::plot3d()
if(requireNamespace("rgl", quietly = TRUE)){
	setGeneric(
		"plot3d", 
		def=function(x,...) standardGeneric("plot3d"),
		package="rgl",
		useAsDefault=rgl::plot3d)
}else{
	setGeneric(
		name="plot3d",
		def=function(x,...){
			if(!requireNamespace("rgl", quietly = TRUE)) stop("Install the 'rgl' package and reload 'icosa' to use this function.")
			standardGeneric("plot3d")
		}
	)
}



#' 3d plotting of an icosahedral grid or its subset
#' 
#' This is a generic function used to plot either a \code{trigrid} or a \code{hexagrid} object or their \code{facelayer} in 3d space. 
#' 
#' The function is built on the openGL renderer of the R package \code{rgl}.
#'  
#' @param x The \code{trigrid}, \code{hexagrid} or \code{facelayer} object to be plotted.
#' 
#' @param type A character value specifying the part of the grid to be plotted by the call of the function. 
#' \code{"v"} plots the grid vertex points. 
#' \code{"e"} draws the grid edges.
#' \code{"f"} draws the grid faces.
#' \code{"c"} draws the face centers of the grid.
#' 
#' @param sphere Defaults to NULL, adding a central white sphere to the plot. Assigning a numeric value will draw a new sphere with the given radius,
#'		\code{FALSE} does not plot the sphere. 
#' @param guides Logical value indicating whether the guidelines of the polar coordinate system shall be plotted.
#' @param add Logical value indicating whether a new plot shall be drawn, or the currently plotted information should be added to the active rgl device.
#' 
#' @param ... Further graphical parameters passed to (see \code{\link[rgl]{plot3d}}).
#' 
#' @return The function does not return any value.
#'
#' @rdname plot3d-method
#'
#' @examples
#' # create a hexagonal grid
#'     g <- hexagrid(c(2,2))
#' # plot the grid in 3d space
#'     plot3d(g, col="blue")
#' # make a subset to select faces
#'    subG <- subset(g, c("F5", "F2"))
#' # plot the subset defined above
#'     plot3d(subG, type="f", col=c("orange"), add=TRUE, lwd=1)
#' @rdname plot3d-methods
#' @aliases plot3d, plot3d-trigrid-method
#' @exportMethod plot3d
setMethod(
	"plot3d",
	signature="trigrid",
	definition=function(x, type=c("l"),sphere=NULL,  add=FALSE, guides=TRUE, ...){
		
		#create new plot?
		if(add==FALSE)
		{
			#checking plotting
			rgl::plot3d(x@vertices, type="n", box=FALSE, axes=FALSE, xlab="", ylab="", zlab="")
			
			#default sphere plotting
			if(is.null(sphere)){
				#get the radius
				fc<-apply(x@vertices[x@faces[1,],],2,mean)-x@center
				rad<-sqrt(fc[1]^2+fc[2]^2+fc[3]^2)-15
				blankSphere(x@center[1],x@center[2], x@center[3], radius = rad, color ="white", ng=200, box=FALSE, axes=FALSE)
			}else{
				if(sphere){
					blankSphere(x@center[1],x@center[2], x@center[3], radius = sphere, color ="white", ng=200, box=FALSE, axes=FALSE)
				}
			}
		}
		
		
		if(type=="p")
		{
			#single point
			if(length(x@vertices)==3)
			{
				rgl::points3d(x=x@vertices[1],y=x@vertices[2],z=x@vertices[3],...)
			}else{
				rgl::points3d(x@vertices, ...)
			}
		}
			
		if(type=="l")
		{
			icosa::lines3d(x, ...)
		}
		
		if(type=="f")
		{
			faces3d(x,...)
		}
		
		if(type=="n"){
		
		}
		# guides
		if(guides){
			guides3d(col="green", origin=x@center, radius=x@r, lwd=2)
		}
		
		
	}
)
#' 3d plotting of an icosahedral grid or its subset
#' @rdname plot3d-method
#' @param color Only for the hexagrid plotting: character value/values, passed to the faces3d() function instead of col.
#' @aliases plot3d, plot3d-hexagrid-method
setMethod(
	"plot3d",
	signature="hexagrid",
	definition=function(x, type=c("l"),sphere=NULL, color="gray70", add=FALSE, guides=TRUE, ...){
		
		#create new plot?
		if(add==FALSE)
		{
			#empty plotting plotting
			rgl::plot3d(x@vertices, type="n", box=FALSE, axes=FALSE, xlab="", ylab="", zlab="")
			
			#default sphere plotting
			if(is.null(sphere)){
				fVect<-x@faces[2,!is.na(x@faces[2,])]
				#get the radius
				fc<-apply(x@vertices[fVect,],2,mean)-x@center
				rad<-sqrt(fc[1]^2+fc[2]^2+fc[3]^2)-10
				blankSphere(x@center[1],x@center[2], x@center[3], radius = rad, color ="white", ng=200, box=FALSE, axes=FALSE)
			}else{
				if(sphere){
					blankSphere(x@center[1],x@center[2], x@center[3], radius = sphere, color ="white", ng=200, box=FALSE, axes=FALSE)
				}
			}
		}
		
		
		if(type=="p")
		{
			#single point
			if(length(x@vertices)==3)
			{
				rgl::points3d(x=x@vertices[1],y=x@vertices[2],z=x@vertices[3], col=color,...)
			}else{
				rgl::points3d(x@vertices, col=color, ...)
			}
		}
			
		if(type=="l")
		{
			icosa::lines3d(x, ...)
		}
		
		if(type=="f")
		{
			faces3d(x,...)
		}
		
		if(type=="c")
		{
			#single point
			if(length(x@faceCenters)==3)
			{
				rgl::points3d(x=x@faceCenters[1],y=x@faceCenters[2],z=x@faceCenters[3], col=color, ...)
			}else{
				rgl::points3d(x@faceCenters, col=color, ...)
			}
		}
		
		if(type=="t")
		{
			rgl::text3d(x@faceCenters, texts=rownames(x@faceCenters),col=color, ...)
		}
		
		if(type=="n"){
		
		}
		if(guides){
			guides3d(col="green", origin=x@center, radius=x@r, lwd=2)
		}
		
		
	}
)

# Conditional generic function for rgl::lines3d()
if(requireNamespace("rgl", quietly = TRUE)){
	setGeneric("lines3d", def=rgl::lines3d, package="rgl")
}else{
	setGeneric(
		name="lines3d",
		def=function(x,...){
			if(!requireNamespace("rgl", quietly = TRUE)) stop("Install the 'rgl' package and reload 'icosa' to use this function.")
			standardGeneric("lines3d")
		}
	)
}



#' Methods of 3d line plotting.
#' 
#' This is a generic function used to plot the edge lines of either a \code{trigrid} or a \code{hexagrid} object in 3d space. The method is also implemented for 
#' the object classes defined by the package 'sp'.
#' 
#' The function is built on the openGL renderer of the R package \code{rgl}.
#'  
#' @param x The \code{trigrid}, \code{hexagrid}, \code{facelayer} or \code{sp} object to be plotted.
#' @param arcs Logical value setting whether great circle arcs or segments shall be drawn betwenn the points of the grid.
#' 
#' @param ... Further graphical parameters passed to (see \code{\link[rgl]{plot3d}}).
#' 
#' @return The function does not return any value.
#'
#' @examples
#' # create a hexagonal grid
#'     g <- hexagrid(c(2,2))
#' # plot the grid in 3d space
#'     plot3d(g, col="blue")
#' # make a subset to select faces
#'    subG <- subset(g, c("F5", "F2"))
#' # plot the subset defined above
#'     plot3d(subG, type="f", col=c("orange"), add=TRUE, lwd=1)
#' @rdname lines3d-method
#' @aliases lines3d-trigrid-method
#' @exportMethod lines3d
setMethod(
	"lines3d",
	signature="trigrid",
	definition=function(x, arcs=FALSE, ...){
		v<-x@skeleton$v
		e<-x@skeleton$e[x@skeleton$aE,]
		
		#create edgeMat with a simple Rccp function
		edgeMat<-.Call(Cpp_icosa_edgeMatTri_, v=v, e=e)
		
		# get the list of additional arguments
		newArgs<-list(...)
		
		
		if(prod(x@tessellation)<16 & arcs){
			res<-10
			edgeMat<-.Call(Cpp_icosa_expandEdges_, edgeMat, x@center, res)
		}
		edgeMatExp<-edgeMat
		rgl::segments3d(x=edgeMatExp[,1],y=edgeMatExp[,2],z=edgeMatExp[,3], ...)
	}
)


#' Methods of 3d face plotting.
#' 
#' This is a generic function used to plot the faces of either a \code{trigrid} or a \code{hexagrid} object in 3d space. 
#' 
#' The function is built on the openGL renderer of the R package \code{rgl}.
#'  
#' @name faces3d
#' @param x The \code{trigrid}, \code{hexagrid} or \code{facelayer} object to be plotted.
#' 
#' @param ... Further graphical parameters passed to (see \code{\link[rgl]{plot3d}}) and the heatMapLegend() function.
#' 
#' @return The function does not return any value.
#'
#' @examples
#' # create a hexagonal grid
#'     g <- hexagrid(c(2,2))
#' # plot the grid in 3d space
#'     plot3d(g)
#' # make a subset to select faces
#'    subG <- subset(g, c("F5", "F2"))
#' # plot the subset defined above
#'     faces3d(subG, col="orange")
#' @exportMethod faces3d
#' @rdname faces3d-methods
setGeneric(
	name="faces3d",
	package="icosa",
	def=function(x,...){
		if(!requireNamespace("rgl", quietly = TRUE)) stop("Install the 'rgl' package and reload 'icosa' to use this function.")
		standardGeneric("faces3d")
		
	}
)

#' Methods of 3d face plotting.
#' 
#' This is a generic function used to plot the faces of either a \code{trigrid} or a \code{hexagrid} object in 3d space. 
#' 
#' The function is built on the openGL renderer of the R package \code{rgl}.
#'  
#' @return The function does not return any value.
#'
#' @rdname faces3d-methods
#' @aliases faces3d, faces3d-trigrid-method	
setMethod(
	"faces3d",
	signature="trigrid",
	definition=function(x, ...){
		
		v<-x@skeleton$v
		f<-x@skeleton$f[as.logical(x@skeleton$aF),1:3]
		
		#create edgeMat with a simple Rccp function
		triMat<- .Call(Cpp_icosa_triMatTri_, v, f)
		
		rgl::triangles3d(x=triMat[,1],y=triMat[,2],z=triMat[,3],...)
			
	}
)

#' Methods of 3d face plotting.
#' 
#' This is a generic function used to plot the faces of either a \code{trigrid} or a \code{hexagrid} object in 3d space. 
#' 
#' The function is built on the openGL renderer of the R package \code{rgl}.
#'  
#' @return The function does not return any value.
#'
#' @rdname faces3d-methods
#' @aliases faces3d, faces3d-hexagrid-method	
setMethod(
	"faces3d",
	signature="hexagrid",
	definition=function(x,...){
		v<-x@skeleton$plotV
		f<-x@skeleton$f[as.logical(x@skeleton$aSF),1:3]
		
	#	f2<-f[order(f[,1]),]
	#	
	#	f2<-unique(f2)
		
		#create edgeMat with a simple Rccp function
		triMat<- .Call(Cpp_icosa_triMatTri_, v, f)
		
		rgl::triangles3d(x=triMat[,1],y=triMat[,2],z=triMat[,3],...)
	
	
	}
)


#' Display the names of the grid elements in 3d plots.
#' 
#' This function will display the names of vertices, faces and edges on 3d plots.
#' 
#' @name gridlabs3d
#'  
#' @param gridObj A \code{trigrid} or \code{hexagrid} object to be plotted.
#' 
#' @param type A character vector containing either "f", "e" or "v", rendering the names
#' of either the faces, edges or vertives respectively.
#'
#' @param ... further arguments passed to \code{text3d} of the rgl package.
#' 
#' @return The function does not return any value.
#'
#' @examples
#' # create a hexagonal grid
#'     g <- hexagrid(c(2,2))
#' # plot the grid in 3d space
#'	   plot3d(g, guides=FALSE)
#' # plot the names of the faces
#' 		gridlabs3d(g, type="f", col="red")
#' # plot the names of the vertices
#'     gridlabs3d(g, type="v", col="blue", cex=0.6)
#' @exportMethod gridlabs3d
#' @rdname gridlabs3d-methods
setGeneric(
	name="gridlabs3d",
	package="icosagrid",
	def=function(gridObj,...){
		if(!requireNamespace("rgl", quietly = TRUE)) stop("Install the 'rgl' package and reload 'icosa' to use this function.")
		standardGeneric("gridlabs3d")
		
	}
)

#' @rdname gridlabs3d-methods
#' @aliases gridlabs3d, gridlabs3d-trigrid-method
setMethod(
	"gridlabs3d",
	signature="trigrid",
	definition=function(gridObj,type="f",...){
		
		# vertex names
		if("v"%in%type){
			rgl::text3d(texts=rownames(gridObj@vertices), gridObj@vertices*1.005,...)
		}
		
		
		if("f"%in%type){
			rgl::text3d(texts=rownames(gridObj@faceCenters), gridObj@faceCenters*1.005,...)
		}
		
		if("e"%in%type){
			#the coordinates
			coords<-apply(gridObj@edges,1,function(x){
				apply(gridObj@vertices[x,], 2, mean)
			})
			
			rgl::text3d(t(coords)*1.005, texts=rownames(gridObj@edges),...)
		}
	
	
	}
)
	
#' @rdname gridlabs3d-methods
#' @aliases gridlabs3d, gridlabs3d-hexagrid-method
setMethod(
	"gridlabs3d",
	signature="hexagrid",
	definition=function(gridObj,type="f",...){
		
		# vertex names
		if("v"%in%type){
			rgl::text3d(texts=rownames(gridObj@vertices), gridObj@vertices*1.005,...)
		}
		
		
		if("f"%in%type){
			rgl::text3d(texts=rownames(gridObj@faceCenters), gridObj@faceCenters*1.005,...)
		}
		
		if("e"%in%type){
			#the coordinates
			coords<-apply(gridObj@edges,1,function(x){
				apply(gridObj@vertices[x,], 2, mean)
			})
			
			rgl::text3d(t(coords)*1.005, texts=rownames(gridObj@edges),...)
		}
	
	
	}
)

