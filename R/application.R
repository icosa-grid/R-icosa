# This file contains all methods that are used in the application of the grid in analyses. 


#' Rotation method of trigrid and hexagrid objects
#'
#' @param x (\code{trigrid} or \code{hexagrid}): Input grid. 
#' @param angles (\code{numeric}): The \code{vector} of rotation in radians (three values in each dimension). If set to \code{"random"}, the rotation will be random (default). 
#' @param pivot (\code{numeric}): The pivot point of the rotation, \code{vector} of xyz coordinates. Defaults to \code{NA} indicating that the rotation will be around the center of the grid.
#' @rdname rotate
#' @return Another \code{trigrid} or \code{hexagrid} class object.
#' @aliases rotate, trigrid-rotate-method		
#' @exportMethod rotate
setMethod(	
	f="rotate",
	signature="trigrid",
	definition= function(x, angles="random", pivot=NA){
		#origin<-F
		#obj<-grid
		#angles<-c(0.15,0.15,0.15)
	
		if(sum(angles=="random"))
		{
			angles<-c(stats::runif(3,0,2*pi))
		
		}

		if(length(angles)!=3 | !is.numeric(angles)) 
			stop("Invalid rotation angle vector.")

		if(!is.na(pivot)[1]){
			if(length(pivot!=3) | !is.numeric(pivot) | sum(is.na(pivot))>0){
				stop("Invalid pivot point coordinates.")
				
			}else{
				orig<-pivot
			}
		
		}else{
			orig<-x@center	
		}
		
		if(suppressWarnings(!is.na(x@sp))){
			message("Please rerun newsp() to regenerate the 2d representation!")
		}
		
		
		# rotate the points
			vertices<-t(apply(x@vertices, 1, rotateOnePoint, angles=angles, origin=orig))
			colnames(vertices)<-c("x","y","z")
		
		#do the same for the face centers
			faceCenters<-t(apply(x@faceCenters, 1, rotateOnePoint, angles=angles, origin=orig))
			colnames(faceCenters)<-c("x","y","z")
		
		# skeleton
			verticesSkel<-t(apply(x@skeleton$v, 1, rotateOnePoint, angles=angles, origin=orig))
			colnames(verticesSkel)<-c("x","y","z")
		
		# add the new, rotated tables to the original object's copy
			x@vertices<-vertices
			x@faceCenters<-faceCenters
			x@skeleton$v<-verticesSkel
		
		# in case the trigrid is a hexagrid too
		if(class(x)=="hexagrid"){
			plotV<-t(apply(x@skeleton$plotV, 1, rotateOnePoint, angles=angles, origin=orig))
			colnames(plotV)<-c("x","y","z")
			x@skeleton$plotV <- plotV
		
		}
		
		# update the orientation
		x@orientation <- x@orientation+angles

		return(x)
		
	}
)

#' Extracting the grid orientation
#' 
#' @name orientation
#' 
#' @param x (\code{trigrid} or \code{hexagrid}): Input grid. 
#' @param display The output unit. In case it is set to \code{"deg"} the output will be in degrees, in case it is \code{"rad"}, then radians.
#' @exportMethod orientation
#' 
#' @aliases orientation-method
#' @rdname orientation-methods
setGeneric(
	name="orientation",
	package="icosa",
	def=function(x,...){
		standardGeneric("orientation")
	}
)

#' Extracting the grid orientation
#' 
#' @rdname orientation-methods
#' @aliases orientation, trigrid-orientation-method	
setMethod(
	"orientation",
	signature="trigrid",
	definition=function(x,display="deg",...){
		if(display=="rad"){
			names(x@orientation)<-c("x (rad)", "y (rad)", "z (rad)")
			return(x@orientation)
		}
		if(display=="deg"){
			names(x@orientation)<-c("x (deg)", "y (deg)", "z (deg)")
			return(x@orientation/pi*180)
			
		}
	
	}
	
)

#' Setting the orientation of a trigrid or hexagrid object
#' 
#' @name orientation<-
#' 
#' @param value the vector of rotation. Passed as the \code{angles} argument of \code{\link[icosa]{rotate}}.
#' @param ... values passed on to the rotate() function.
#' 
#' @exportMethod orientation<-
#' 
#' @rdname orientation-methods
setGeneric(
	name="orientation<-",
	def=function(x,value){
		standardGeneric("orientation<-")
	}
	
)

#' Setting the orientation of a trigrid or hexagrid object
#' 
#' 
#' 
#' @aliases trigrid-setorientation-method
#' @rdname orientation-methods
setReplaceMethod(
	"orientation",
	signature="trigrid",
	definition=function(x, value){
		x<-rotate(x, angles=value)
		return(x)
	
	}
)




#' The neighbouring faces of faces in an icosahedral grid
#' 
#' This function will return neighbouring faces of the input faces. 
#' @name vicinity
#' 
#' @param gridObj A \code{trigrid} or \code{hexagrid} class object. 
#' 
#' @param faces A character vector specifying names of faces. 
#'
#' @param order Passed to the igraph::ego() function, an integer value specifying the size of the neighborhood around a face.
#'	
#' @param output Characater value, the type of the output. The default \code{"vector"} 
#' 	will give back the names of the faces that adjacent to the faces specified, 
#' 	including themselves. \code{"list"} will return a list.
#'
#' @param self logical value indicating whether the input faces should be in the output. For the \code{"list"} output option, the input face names will be
#' omitted only from those character vectors that contain face names that are related to the face in question.
#'
#' @param namedorder (\code{logical}) Should the orders of the neighbouring cells be reported (\code{TRUE}) or just the names of the cells (default, \code{FALSE}).
#' @param ... arguments passed to the igraph::ego() function
#' @examples
#' 	g <- trigrid(3)
#' 	ne <- vicinity(g, c("F4", "F10"))
#' 	ne
#' 
#' @return A character vector or a list of character vectors.
#' 	
#' @exportMethod vicinity
#' @rdname vicinity-methods
setGeneric(
	name="vicinity",
	def=function(gridObj,faces,...){
		standardGeneric("vicinity")
	}
)

#' @rdname vicinity-methods
setMethod(
	"vicinity",
	signature=c("trigrid","character"),
	definition=function(gridObj,faces, order=1, output="vector",self=TRUE,namedorder=FALSE,  ...){
		#if no @graph found
		if(suppressWarnings(is.na(gridObj@graph)[1])){
			stop("Slot @graph is empty. Use newgraph() to add an igraph respresentation. ")
		}
		
		# get rid of NA entries
		faces<-faces[!is.na(faces)]
		
		# check whether the facein put is actually in the grid
		if(sum(!faces%in%rownames(gridObj@faces))!=0)
			stop("Invalid face name.")
		
		# get the actual vertices
		nList<-igraph::ego(gridObj@graph, order=order, faces,...)
		
		# get only the names of the faces
		newList<-lapply(nList, function(x){x$name})
		
		# should the orders of the neighbours be reported? if yes..
		if(namedorder){
			newStructure <- lapply(newList, function(x){
				temp <- rep(order, length(x))
				names(temp) <- x
				temp
			})

			# rerun the process for all the lower orders iterativels
			for(i in order:1){
				nList<-igraph::ego(gridObj@graph, order=i, faces,...)
		
				# get only the names of the faces
				nL<-lapply(nList, function(x){x$name})

				newStructure <- mapply(FUN=function(x,y){
					y[x] <- i
					return(y)
				},nL, newStructure)

			}

			newList <- lapply(newStructure, function(x){x[1] <- 0; return(x)})

		}


		if(!self & !namedorder){
			newList<-lapply(newList, function(x){x[-1]})
		}

		
		if(output=="list"){
			return(newList)
		}
		
		if(output=="vector"){
			temp <- unlist(newList)
			if(self){
				return(sort(unique(c(temp, faces))))
			}else{
				return(sort(temp[!temp%in%faces]))
			
			}
		}
	}
)


#' Lengths of grid edges
#' 
#' This function will return the length of all edges in the specified grid object.
#' 
#' @name edgelength
#' @param gridObj A \code{trigrid} or \code{hexagrid} class object. 
#' 
#' @param ... arguments passed to the class specific methods.
#' 
#' @examples
#' 	g <- trigrid(3)
#' 	edges <- edgelength(g, output="deg")
#' 	edges
#' 
#' @return A named numeric vector.
#' 	
#' @exportMethod edgelength
#' @rdname edgelength-methods
setGeneric(
	name="edgelength",
	def=function(gridObj,...){
		standardGeneric("edgelength")
	}

)

#' Lengths of grid edges
#' 
#' This function will return the length of all edges in the specified grid object.
#' 
#' @param gridObj A \code{trigrid} or \code{hexagrid} class object. 
#' 
#' @param output Character value, the type of the output. \code{"distance"} will give back the distance
#'	in the metric that was fed to the function in the coordinates or the radius.
#'	\code{"deg"} will output the the distance in degrees, \code{"rad"} will do
#'	so in radians.
#' 
#' @rdname edgelength-trigrid-methods
setMethod(
	"edgelength", 
	signature="trigrid",
	definition=function(gridObj, output="distance"){
		if(!output%in%c("distance", "rad", "deg")) stop("Invalid distance method.")
		v<-gridObj@skeleton$v
		e<-gridObj@skeleton$e
		
		if(output=="distance"){
			met<-1
		}else{
			met<-0
		}
		sizes<-  .Call(Cpp_icosa_edges_, v, e, origin=as.integer(gridObj@center), method=as.logical(met))
		
		names(sizes)<-rownames(gridObj@edges)
		
		if(output=="deg") sizes<-sizes/pi*180
		
		return(sizes)
		
	}
)


#' Areas of grid cell surfaces
#' 
#' This function will return the areas of all cells in the specified grid object.
#' 
#' @name surfacearea
#' @param gridObj A \code{trigrid} or \code{hexagrid} object. 
#' 
#'	in the metric that was fed to the function in the coordinates or the radius.
#'	\code{"deg"} will output the the distance in degrees, \code{"rad"} will do
#'	so in radians.
#' 
#' @examples
#' 	g <- trigrid(3)
#' 	surfaces <- surfacearea(g)
#' 	surfaces
#' 
#' @return A named numeric vector.
#' 	
#' @rdname surfacearea-methods
#' @exportMethod surfacearea
setGeneric(
	name="surfacearea",
	package="icosa",
	def=function(gridObj){
		standardGeneric("surfacearea")
		
	}
)

#' @rdname surfacearea-methods
# ' @aliases surfacearea, trigrid-method	
setMethod(
	"surfacearea", 
	signature="trigrid", 
	def=function(gridObj){
		# get the highest resolution faces
		newF <- gridObj@skeleton$f[as.logical(gridObj@skeleton$aF),1:3]
		v <- gridObj@skeleton$v
		
		# call the surface calculation function
		surfInner <-  .Call(Cpp_icosa_spherTriSurfs,
			v=v, 
			f=newF, 
			origin=gridObj@center, 
			pi=pi
		)
		
		# reorganize the faces: outer representation
		ord<-gridObj@skeleton$aF[as.logical(gridObj@skeleton$aF)]
		
		surfOuter<-surfInner
		surfOuter[ord]<- surfInner
		
		names(surfOuter) <- rownames(gridObj@faces)
		
		return(surfOuter)
	}
)

#' @rdname surfacearea-methods
# ' @aliases surfacearea, hexagrid-method
setMethod(
	"surfacearea", 
	signature="hexagrid", 
	def=function(gridObj){
		# get the highest resolution faces
		newF <- gridObj@skeleton$f[as.logical(gridObj@skeleton$aSF),1:3]
		v <- gridObj@skeleton$v
		
		# call the surface calculation function
		surfInner <-  .Call(Cpp_icosa_spherTriSurfs, 
			v=v, 
			f=newF, 
			origin=gridObj@center, 
			pi=pi
		)
		
		# the subfaces belong to these face IDs in the outer representation
		aS<-gridObj@skeleton$aSF[as.logical(gridObj@skeleton$aSF)]
		
		# calculate the sums of all subface areas in a face, and order them
		doubleSurf<-tapply(INDEX=aS, X=surfInner, sum)
		
		# each subface occurs two times in the f matrix, divide area by 2
		singleSurf <- doubleSurf/2
		
		# augment the names attributes
		names(singleSurf)<- paste("F", names(singleSurf), sep="")
		
		return(singleSurf)
	}
)


#' Shape distortions of the triangular faces and subfaces
#' 
#' This function will return a value that is proportional to the irregularity of a triangonal face or subface.
#' 
#' The value is exactly 1 for an equilateral triangle, and becomes 0 as one of the edges approach 0.
#'
#' @name trishape
#' @param gridObj A \code{trigrid} or \code{hexagrid} object. 
#' 
#' @examples
#' 	g <- trigrid(3)
#' 	shape <- trishape(g)
#' 	trishape
#' 
#' @return A named numeric vector.
#' 	
#' @rdname trishape-methods
#' @exportMethod trishape
setGeneric(
	name="trishape",
	def=function(gridObj){
		standardGeneric("trishape")
	}
)

#' @rdname trishape-methods
setMethod(
	"trishape",
	signature="trigrid",
	definition=function(gridObj){
		# center back to origin if not there already
		if(gridObj@center[1]!=0 | gridObj@center[2]!=0 | gridObj@center[3]!=0){
			gridObj<-translate(gridObj,-gridObj@center)
		}
		
		v <- gridObj@skeleton$v
		f <- gridObj@skeleton$f[as.logical(gridObj@skeleton$aF),]
		
		# the shapes in the inner order
		innerShape <-.Call(Cpp_icosa_AllShapeTri_, v, f)
		outerShape <- innerShape[gridObj@skeleton$uiF]

		return(outerShape)
	}
)

#' @rdname trishape-methods
setMethod(
	"trishape",
	signature="hexagrid",
	definition=function(gridObj){
		# center back to origin if not there already
		if(gridObj@center[1]!=0 | gridObj@center[2]!=0 | gridObj@center[3]!=0){
			gridObj<-translate(gridObj,-gridObj@center)
		}
		
		v <- gridObj@skeleton$v
		f <- gridObj@skeleton$f[as.logical(gridObj@skeleton$aSF),]
		
		# the shapes in the inner order
		innerShape <-.Call(Cpp_icosa_AllShapeTri_, v, f)
		outerShape<- tapply(
			X=innerShape, 
			INDEX=gridObj@skeleton$aSF[as.logical(gridObj@skeleton$aSF)],
			FUN=mean)	
		outerShape<-as.numeric(outerShape)
		names(outerShape)<-paste("F", names(outerShape), sep="")
		
		return(outerShape)
		
	
	}
)

		
# Sampling of random points from grid faces
# 
# This function generates a set of random points from the surface of a face on an icosahedral grid.
# 
# The algorithm uses a method similar to tessellation: it breaks one of great circles that form the edges of the face, then selects one point from the great circle between the vertex opposite of the original edge and the previously created point.
# 
# @param n Integer vector containing the number of points to be sampled from the specified surfaces. If it is a single value than it specifies the total number of points.
# If its length equals the number of specified faces in the \code{facenames} argument, it will return the respective number of samples from each face.
# 
# @param facenames A character vector containing the face names that will be sampled. If set to \code{NULL} the entire grid will be sampled.
# 
# @param gridObj The grid that will be sampled, either a \code{trigrid} or \code{hexagrid} object.
# 
# @param res The resolution at which the randomly selected great circles are divided.
# 
# @examples
# # Create a triangular grid
# triangular<-CreateGrid(c(2,2), "tri")
# 
# #get a set of random points
# random<-RandPoints( gridObj=triangular, c(150,40),facenames=c("F12", "F10"),  res=100)
# 
# #plot these in Cartesian space
# PlotGrid3d(triangular, "edges", col="blue")
# points3d(x=random[,1],y=random[,2],z=random[,3])
# 
RandPoints<-function(gridObj, n, facenames=NULL, res=100){
	UseMethod("RandPoints",gridObj)
}




## wrapper function around the OccupiedFaces generic, to get the occupied grid cells. return out a facelayer object
#export!
#' Faces occupied by the specified object
#'
#' This function will return a \code{facelayer} class object showing which faces are occupied by the input object.
#'
#' This is a wrapper function on the \code{OccupiedFaces} methods that are specific to grid class and input data. The function creates a link between the facelayer and the relevant grid.
#'
#' @param gridObj a \code{trigrid} or \code{hexagrid} object.
#' 
#' @param data the queried data.
#'
#' @param ... arguments passed to the class specific methods
#'
#' @return returns a facelayer object
#'
#' @examples
#'	# create a grid
#'	g <- trigrid(8, sp=TRUE)
#'	# create random points
#'	randPoints <- rpsphere(100,output="polar")
#'	# the facelayer occupied by these points
#'	randomLayer <- occupied(g, randPoints)
#'	plot(randomLayer)
#'	points(randPoints, col="blue", pch="+")
#'	
#'
#' @export	
occupied  <- function(gridObj, data,...){
	
	# do spatial transformation if a CRS is present
	if(methods::.hasSlot(data, "proj4string")){
		# and only if it is not NA
		if(!is.na(data@proj4string)){
			# need rgdal
			if(requireNamespace("rgdal", quietly = TRUE)){
				data<-sp::spTransform(data, gridObj@proj4string)
			} else{
				stop("The rgdal package is required to appropriately project this object. ")
			}
		}
	}
	
	#exectue the appropriate searching procedure
	boolVec<-OccupiedFaces(gridObj, data,...)
	
	# construct a facelayer
	endObj<-facelayer(gridObj)
	
	#outer ordering for the hexagrid
	translNum<-which(boolVec)
		
	# replace
	endObj@values<-rep(FALSE, length(endObj))
	endObj@values[translNum]<-TRUE

	# get the name of the grid - 
	endObj@grid<-deparse(substitute(gridObj))
	return(endObj)

}



#OccupiedFaces
#returns a boolean for all the faces (ordered as the internal representation in skeleton$f)
#use this when you do not need to know which face contains which point
setGeneric(
		name="OccupiedFaces",
		def=function(gridObj,data,...){
			standardGeneric("OccupiedFaces")
		}
	
	)
	

# occupied method for the trigrid v6.0
setMethod(
	"OccupiedFaces", 
	signature=c("trigrid", "matrix"),
	definition=function(gridObj, data){
		#locate the cells
		occCells<-locate(gridObj, data, randomborder=FALSE, output="ui")
		
		# the logical vector indicating the face
		boolVec<-rep(FALSE, nrow(gridObj@faces))
		
		boolVec[rownames(gridObj@faces)%in%occCells] <- TRUE
		
		return(boolVec)
	}
)
	
# for spatial points
setMethod(
	"OccupiedFaces",
	signature=c("trigrid", "SpatialPoints"),
	definition=function(gridObj, data){
		borders<-NA
		# basic method for matrices
		OccupiedFaces(gridObj, data@coords)
	}
)
	
# for polygon occupation development
# v2.0 - using igraph
# 2017.02.22.
setMethod(
	"OccupiedFaces", 
	signature=c("trigrid", "Polygon"),
	definition=function(gridObj, data){
		#if no @graph found
		if(suppressWarnings(is.na(gridObj@graph)[1])){
			stop("Slot @graph is empty. Use newgraph() to add an igraph respresentation. ")
		}
		#get the number of faces occupied by the line
			lin<-PolToCar(data@coords, origin=gridObj@center, radius=gridObj@r)
			lin2<- .Call(Cpp_icosa_EvenInterpolation_, lin, gridObj@center, gridObj@edgeLength[2]/180*pi/15)
			
			lineCells<-unique(locate(gridObj,lin2))
		
		# get all the faces
			allFaces<-rownames(gridObj@faces)
			subFaces<-allFaces[!allFaces%in%lineCells]
			
			subGraph<-igraph::induced_subgraph(gridObj@graph, subFaces)
			clusters <- igraph::membership(igraph::clusters(subGraph))
		
		
		#sample the middle part
		middleSample<-sp::spsample(data, type="regular", n=25)
		middleCells<-unique(locate(gridObj, middleSample@coords))
			
		# the group ID of this unit
			clusterIDs<-clusters[names(clusters)%in%middleCells]
			
		# the inner part of the faces
		innerFaces <- names(clusters)[clusters%in%clusterIDs]
		fLayer <- rep(F, nrow(gridObj@faces))
		fLayer[rownames(gridObj@faces)%in%c(lineCells, innerFaces)]<-TRUE
		
		return(fLayer)
	}
)

#for Polygons 
setMethod(
	"OccupiedFaces",
	signature=c("trigrid", "Polygons"),
	definition=function(gridObj, data, n=10000,...){
		borders<-NA
		#faces on the line
		coordLine<-lines3d(data, plot=FALSE)
		coordLine<-coordLine[!is.na(coordLine[,1]),]
		#look these up
		lineFaces<-OccupiedFaces(gridObj, coordLine)
		#get all the sampling points
		all<-sp::spsample(data, type="regular", n=n)

		inFaces<-OccupiedFaces(gridObj, all@coords)
		fl <- inFaces | lineFaces
		return(fl)
	}
)

#for SpatialLines 
setMethod(
	"OccupiedFaces",
	signature=c("trigrid", "SpatialLines"),
	definition=function(gridObj, data, f=5){
		borders<-NA
		# increase resolution
		data<-linIntCoords(data, res=f)
		
		# get the coordinates
		coords<-lines3d(data, plot=TRUE)
		
		#get rid of NAs
		coords<-coords[!is.na(coords[,1]),]
		
		# the faces occupied by the line
		occupiedByLine<-OccupiedFaces(gridObj, coords)
		
		return(occupiedByLine)
	}
)
		
#for SpatialPolygons 
# v. 3.0
setMethod(
	"OccupiedFaces",
	signature=c("trigrid", "SpatialPolygons"),
	definition=function(gridObj, data){
		borders<-NA
		
		#faces on the line
		coordLine<-lines3d(data, plot=FALSE)
		coordLine<-coordLine[!is.na(coordLine[,1]),]
		#look these up
		lineFaces<-OccupiedFaces(gridObj, coordLine)
		
		# create a raster from the SpatialPolygons
		r <-raster::raster()
		
		# set the resolution to that of the grid
		raster::res(r)<-min(edgelength(gridObj, output="deg"))/4
		
		#rasterize it
		data<-raster::rasterize(data,r)
				
		# use the OccupiedFaces method of the raster
		inFaces<-OccupiedFaces(gridObj, data)
		fl <- inFaces | lineFaces
		return(fl)
	}
)

#for SpatialPolygonsDataFrame
setMethod(
	"OccupiedFaces",
	signature=c("trigrid", "SpatialPolygonsDataFrame"),
	definition=function(gridObj, data){
		borders<-NA
		temp<-methods::as(data,"SpatialPolygons")
		fl <- OccupiedFaces(gridObj, temp)
		return(fl)
	}
)


# for spatial points
setMethod(
	"OccupiedFaces",
	signature=c("trigrid", "RasterLayer"),
	definition=function(gridObj, data){
		borders<-NA
		resGrid<-mean(edgelength(gridObj,"deg"))
		# if the default resolution of the raster is too coarse for the trigrid
		if(resGrid<(4*raster::res(data)[1]) | resGrid<(4*raster::res(data)[2])){
			#upscale
			r<-data
			raster::res(r)<-resGrid/4
			data<-raster::resample(data, r, "ngb")
		}
		
		xmin<-data@extent@xmin
		xmax<-data@extent@xmax
		ymin<-data@extent@ymin
		ymax<-data@extent@ymax
		
		xres<-raster::res(data)[1]
		yres<-raster::res(data)[2]
		
		xs<-seq(xmin+xres/2, xmax-xres/2,xres)
		ys<-seq(ymax-yres/2, ymin+yres/2,-yres)
		
		x<-rep(xs, length(ys))
		y<-rep(ys, each=length(xs))
		mat<-cbind(x,y)
		
		cells<-locate(gridObj, mat)
		
		occup<-tapply(X=values(data), INDEX=cells, function(x){sum(!is.na(x))})
		occupiedCells<-names(occup)[occup>0]
		
		fl<-rep(FALSE, length(gridObj))
		fl[rownames(gridObj@faces)%in%occupiedCells]<-T
		
		return(fl)
	}
)



#' Basic lookup function of coordinates on an icosahedral grid
#'
#' @name locate
#' @return The function returns the cell names where the input coordinates fall.
#'
#' @param gridObj a trigrid or hexagrid class object.
#' @param data Coordinates of individual points. Can be either a two-dimensional 
#' matrix of long-lat coordinates, a three-dimensional matrix of XYZ coordinates, 
#' or a set of points with class 'SpatialPoints'.
#'
#' @param randomborder Logical value. Defaults to FALSE. If TRUE, then the points
#' falling on vertices and edges will be randomly assigned, otherwise they will be kept as NAs.
#'
#' @param output Character value either "ui" or "skeleton". ui returns the face 
#' 	names used in the user interface, while "skeleton" returns their 
#' 	indices used in back-end procedures.
#' @param ... arguments passed to class specific methods.
#' @examples
#'	# create a grid 
#'	g <- trigrid(4)
#'	# some random points
#'	randomPoints<-rpsphere(4, output="polar")
#'	# cells
#'	locate(g, randomPoints)
#' @rdname locate-methods
#' @exportMethod locate
setGeneric(
		name="locate",
		def=function(gridObj,...){
			standardGeneric("locate")
		}
	
	)


# locate method for the trigrid v6.0
# this version uses my own c++ function for point in tetrahedron testing
#' @rdname locate-methods
#' @exportMethod locate
setMethod(
	"locate", 
	signature="trigrid",
	definition=function(gridObj, data, randomborder=FALSE, output="ui"){
	
	#the tetrahedron algorithm does not find vertices
	if(!is.logical(randomborder)){
		stop("Invalid randomborder argument.")
	}
	
	if(!output%in%c("ui", "skeleton")){
		stop("Invalid value for output argument.")
	}
	
	# for the SpatialPoints
	if(class(data)=="SpatialPoints"){
		# if it has a proj4
		if(methods::.hasSlot(data, "proj4string")){
			# and it's not NA
			if(!is.na(data@proj4string)){
				# need rgdal
				if(requireNamespace("rgdal", quietly = TRUE)){
					data<-sp::spTransform(data, gridObj@proj4string)@coords
				} else{
					stop("The rgdal package is required to appropriately project this object. ")
				}
			}
		}
	}
	
	#data argument
	# which formatting?
	if(ncol(data)==2){
	
		# transform the two columns
		data<-PolToCar(data, origin=gridObj@center, radius=gridObj@r)
	}
	
	# does the data include NAs?
	boolResultNoNA<-!is.na(data[,1]) & !is.na(data[,2]) & !is.na(data[,3]) 
	data<-data[boolResultNoNA,, drop=FALSE]
	
	
	#project the coordinates out from the origin
	#access the skeleton of the grid
		v<-gridObj@skeleton$v*1.5
		f<-gridObj@skeleton$f[,1:3]
		origin<-gridObj@center
	
		d<-gridObj@div
		
	#organize vertices to the linear coordinate + add 1s for the determinants
		#written with C++ for speed
		vtsBig<- .Call(Cpp_icosa_xyz1xyz1xyz1xyz1_, v, f)
		
		
	#check whether the point is one of the vertices!!!!! here
		
	#the queried data in a similar linear format x*n,y*n, z*n
		qrs<-.Call(Cpp_icosa_xyz1, data)
		
		nQrs<-as.integer(nrow(data))
				
			
		# allocate some memory to the results vector 
		queryIndex<-rep(-9, nrow(data))
		faceIndex<-rep(-9, nrow(data))
		foundMiddle<-rep(0, nrow(data)*12)
		faceContainer<-rep(0, max(d)+1)
		offset<-rep(0,length(d)+1)
		tempF<-rep(0, max(d)+1)
			
	#invoke the C function
	#written with pass by reference object manipulation to evade speed loss with copying
	Output = .C(Cpp_locateTriangle_,
		allVertices=as.double(vtsBig),
		divs=as.integer(d),
		nDivs=as.integer(length(d)),
		queries=as.double(qrs), 
		nQrs=as.integer(nQrs),
		queryIndex=as.integer(queryIndex),
		faceIndex=as.integer(faceIndex),
		offset=as.integer(offset),
		faceContainer=as.integer(faceContainer),
		foundMiddle=as.integer(foundMiddle),
		tempF=as.integer(tempF)
	)
	# and 1 to the 0 indexing
	fi<-Output$faceIndex+1
	qi<-Output$queryIndex+1
	
	#1. in case no values are passed to the function
	if(nQrs==0){
		return(NULL)	
	}else{
		# clean up results: indicate the points the program was unable to assign
		# delete empty entries
		fi<-fi[qi>0]
		qi<-qi[qi>0]
		
		# in case of a duplicate - just get rid of the first
		fi<-fi[!duplicated(qi)]
		qi<-qi[!duplicated(qi)]
		
		# create new container for the face indices
		newFi<-rep(NA, nQrs)
		newFi[qi] <- fi
		fi <- newFi
		qi <- 1:nQrs
	}
	
	# 2. do different stuff depending on the borders argument
	if(sum(is.na(fi))>0 & randomborder){
		# this is the more difficult case
		dubiousIndex<-which(is.na(fi))
		
		# the coordinates of these points
		weirdPoints<-data[dubiousIndex,, drop=FALSE]
		
		# repeat locate on randomly generated close points
		addFi<-apply(weirdPoints, 1, approximateFace, n=20, d=2e-8, gridObj=gridObj, onlyOne=FALSE, output="skeleton")
		
		# add these points to the rest
		fi[dubiousIndex]<-addFi
		
	}
	
	
	if(output=="ui"){
		# translate the inner C representation to the UI
		fiUI<-gridObj@skeleton$aF[gridObj@skeleton$offsetF+fi]
		
		options(scipen=999)
		fiUI[!is.na(fiUI)]<-paste("F", fiUI[!is.na(fiUI)], sep="")
		options(scipen=0)
		resVec<-rep(NA, length(boolResultNoNA))
		resVec[boolResultNoNA]<-fiUI
		
		return(resVec)
	}
	if(output=="skeleton"){
		resVec<-rep(NA, length(boolResultNoNA))
		resVec[boolResultNoNA]<-fi
		
		return(resVec)
	}
}
)


# locate method for the hexagrid v6.0
# this version uses my own c++ function for point in tetrahedron testing
#' @param forceNA logical value, suppressing the recursive lookup of points falling on subface boundaries.
#' @rdname locate-methods
# ' @aliases locate, hexagrid-method
#' @exportMethod locate
setMethod(
	"locate", 
	signature="hexagrid",
	definition=function(gridObj, data, output="ui", randomborder=FALSE, forceNA=FALSE){

	#the tetrahedron algorithm does not find vertices
	if(!is.logical(randomborder)){
		stop("Invalid randomborder argument.")
	}
	
	if(!output%in%c("ui", "skeleton")){
		stop("Invalid value for output argument.")
	}
	
	if(is.data.frame(data)){
		data <- as.matrix(data)
	}
	
	# for the SpatialPoints
	if(class(data)=="SpatialPoints"){
		# if it has a proj4
		if(methods::.hasSlot(data, "proj4string")){
			# and it's not NA
			if(!is.na(data@proj4string)){
				# need rgdal
				if(requireNamespace("rgdal", quietly = TRUE)){
					data<-sp::spTransform(data, gridObj@proj4string)@coords
				} else{
					stop("The rgdal package is required to appropriately project this object. ")
				}
			}
		}
	}
	
	#data argument
	# which formatting?
	if(ncol(data)==2){
		# transform the two columns
		data<-PolToCar(data, origin=gridObj@center, radius=gridObj@r)
	}
	
	# does the data include NAs?
	boolResultNoNA<-!is.na(data[,1]) & !is.na(data[,2]) & !is.na(data[,3]) 
	data<-data[boolResultNoNA,, drop=FALSE]
	
	#project the coordinates out from the origin
	#access the skeleton of the grid
		v<-gridObj@skeleton$v*1.5
		f<-gridObj@skeleton$f[,1:3]
		origin<-gridObj@center
	
		d<-gridObj@div
		d<-c(d,6)
		
	#organize vertices to the linear coordinate + add 1s for the determinants
		#written with C++ for speed
		vtsBig<- .Call(Cpp_icosa_xyz1xyz1xyz1xyz1_, v, f)
		
		
	#check whether the point is one of the vertices!!!!! here
		
	#the queried data in a similar linear format x*n,y*n, z*n
		qrs<-.Call(Cpp_icosa_xyz1, data)
		
		nQrs<-as.integer(nrow(data))
				
			
		# allocate some memory to the results vector 
		queryIndex<-rep(-9, nrow(data)*6)
		faceIndex<-rep(0, nrow(data)*6)
			
	#invoke the C function
	#written with direct C object manipulation to evade speed loss with copying
	Output = .C(Cpp_locateTriangle_,
		allVertices=as.double(vtsBig),
		divs=as.integer(d),
		nDivs=as.integer(length(d)),
		queries=as.double(qrs), 
		nQrs=as.integer(nQrs),
		queryIndex=as.integer(queryIndex),
		faceIndex=as.integer(faceIndex),
		offset=as.integer(rep(0,length(d)+1)),
		faceContainer=as.integer(rep(0, max(d)+1)),
		foundMiddle=as.integer(rep(0,12)),
		tempF=as.integer(rep(0, max(d)+1))
	)
	# and 1 to the 0 indexing
	fi<-Output$faceIndex+1
	qi<-Output$queryIndex+1
	
	#1. in case no values are passed to the function
	if(nQrs==0){
		return(NULL)	
	}else{
		# clean up results: indicate the points the program was unable to assign
		# delete empty entries
		fi<-fi[qi>0]
		qi<-qi[qi>0]
		
		# in case of a duplicate:
		if(sum(duplicated(qi))>0){
			# delete both and reinvestigate
			tqi<-table(qi)
			duplicateBullshit<-as.numeric(names(tqi[tqi>1]))
			fi<-fi[!qi%in%duplicateBullshit]
			qi<-qi[!qi%in%duplicateBullshit]
		}
	
		# create new container for the face indices
		newFi<-rep(NA, nQrs)
		newFi[qi] <- fi
		fi <- newFi
		qi <- 1:nQrs
	}
	
	if(output=="ui"){

		# translate the inner C representation to the UI
		fiUI<-gridObj@skeleton$aSF[Output$offset[length(d)]+fi]
		
		# add the labels
		#temporarily supress scientific notation
		options(scipen=999)
		fiUI[!is.na(fiUI)]<-paste("F", fiUI[!is.na(fiUI)], sep="")
		options(scipen=0)
		
		# this section needs to be here, otherwise it won't recognize the same faces separated to different subfaces
		# 2. do different stuff depending on the borders argument
		# stop it for base case of recursion
		if(!forceNA){
			if(sum(is.na(fiUI))>0){
				# this is the more difficult case
				dubiousIndex<-which(is.na(fiUI))
				
				# the coordinates of these points
				weirdPoints<-data[dubiousIndex,, drop=FALSE]
				
				# repeat locate on randomly generated close points
				addFiUI<-apply(weirdPoints, 1, approximateFace, n=20, gridObj=gridObj, d=2e-10, onlyOne=!randomborder, output="ui")
				addFiUI
				
				# add these points to the rest
				fiUI[dubiousIndex]<-addFiUI
				
			}
		}
		resVec<-rep(NA, length(boolResultNoNA))
		resVec[boolResultNoNA]<-fiUI
		
		return(resVec)
	}
	if(output=="skeleton"){
		fiInner<-gridObj@skeleton$f[Output$offset[length(d)]+fi,1]
		
		# stop it for base case of recursion
		if(!forceNA){
			if(sum(is.na(fiInner))>0){
				# this is the more difficult case
				dubiousIndex<-which(is.na(fiInner))
				
				# the coordinates of these points
				weirdPoints<-data[dubiousIndex,, drop=FALSE]
			
				# repeat locate on randomly generated close points
				addFiInner<-apply(weirdPoints, 1, approximateFace, n=20, gridObj=gridObj, d=2e-10, onlyOne=!randomborder, output="skeleton")
				
				# add these points to the rest
				fiInner[dubiousIndex]<-addFiInner
				
			}
		}
	
		resVec<-rep(NA, length(boolResultNoNA))
		resVec[boolResultNoNA]<-fiInner
		
		return(resVec)
	}
	}
)




#' Position of face centers and vertices on a grid
#' 
#' This function will retrieve the position of a vertex or a face on a \code{hexagrid} or \code{trigrid} object.
#' 
#' Vertex and face names can be mixed in a single \code{names} argument.
#' 
#' @param gridObj a \code{hexagrid} or \code{trigrid} object.
#' 
#' @param names A (character) vector of the names that are to be looked up.
#' 
#' @param output The coordinate system in which the names are to be shown: use \code{"polar"} for longitude-latitude and \code{"cartesian"} for XYZ output.
#' 
#' @return A numerical matrix.
#' 
#' @examples
#' 	g <- trigrid(c(4,4))
#' 	pos(g, c("F2", "P6", "dummyname"))
#' 
#' 
#' @export
pos<-function(gridObj, names, output="polar"){
	
	if(!class(gridObj)%in%c("trigrid", "hexagrid")) stop("Invalid gridObj argument.")
	if(!output%in%c("polar", "cartesian")) stop("Invalid output argument.")
	
	names<-as.character(names)
	
	#facecenters
	fBool<-names%in%rownames(gridObj@faces)
	fcs<-gridObj@faceCenters[names[fBool],]

	#vertices
	vBool<-names%in%rownames(gridObj@vertices)
	vs<-gridObj@vertices[names[vBool],]
	
	#result
	res<-matrix(NA, nrow=length(names), ncol=3)
	
	res[fBool,] <- fcs
	res[vBool,] <- vs
	
	if(output=="cartesian"){
		rownames(res)<-names
		colnames(res)<-c("x","y", "z")
	}
	
	if(output=="polar"){
		res<-CarToPol(res, norad=TRUE, origin=gridObj@center)
		rownames(res)<-names
		colnames(res)<-c("long", "lat")
	}
	return(res)
}

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



#	if(requireNamespace("chronosphere", quietly = TRUE)){
#		setGeneric("mapplot", def=chronosphere::mapplot)
#	}else{
#		setGeneric(
#			name="mapplot",
#			def=function(x,...){
#				standardGeneric("mapplot")
#			}
#		)
#	}
#	
#	setMethod(
#		"mapplot",
#		signature="facelayer",
#		function(x){
#			plot(x)
#		}
#	)

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


#' Create or instantiate an 'igraph' class graph from the faces of an icosahedral grid
#'
#' The function can be applied to both grids and facelayers.
#' @name gridgraph
#' @return The function returns an undirected igraph graph.
#' @param x the icosahedral grid or facelayer.
#' @param ... arguments passed to the class specific methods.
#' @rdname gridgraph-methods
#' @exportMethod gridgraph
setGeneric(
		name="gridgraph",
		def=function(x,...){
			standardGeneric("gridgraph")
		}
	
	)
	
#' Create or instantiate an 'igraph' class graph from the faces of a triangular grid
#'
#' The function can be applied to a trigrid class object.
#' @return The function returns an 'igraph' graph.
#' @param directed logical value, defaults to FALSE creating an undirected graph. If TRUE than the graph will be directed.
#' @param distances logical values, defaults to FALSE. If TRUE than the distances between the linked faces will be calculated and will be rendered to the edges as [["dist"]].
#' @rdname gridgraph-methods
#' @aliases gridgraph-trigrid-method
setMethod(
	f="gridgraph",
	signature="trigrid",
	definition= function(x, directed=FALSE,distances=FALSE){
		# if a graph object already exists in the grid
		if(!suppressWarnings(is.na(x@graph))[1]){
			gridGraph<-x@graph
		}else{
				
			# calculate the outer ordering
				# same format
				boolActFace <- x@skeleton$f[,4]==max(x@skeleton$f[,4])
				replaceSource<-x@skeleton$aF[boolActFace]
			
				# new order
				nOutOldIndex <- x@skeleton$n[x@skeleton$uiF,]
				nOutOldIndex <- nOutOldIndex +1
			
			
			nOut<-x@skeleton$n
			nOut[,1]<-replaceSource[nOutOldIndex[,1]]
			nOut[,2]<-replaceSource[nOutOldIndex[,2]]
			nOut[,3]<-replaceSource[nOutOldIndex[,3]]
			nOut[,4]<-replaceSource[nOutOldIndex[,4]]
			
			# c++ function to create an edgelist 
			edgeList <- .Call(Cpp_icosa_edgeListFromNeighbours_, nOut)
			
			# supress scientific notation!
			options(scipen=999)
			edgeListChar <- matrix(paste("F", edgeList, sep=""), ncol=2)
			options(scipen=0)
			
			# the arguments for the igraph function
		
			# get rid of the double edges
			edgeListChar <- unique(edgeListChar)
			
			
			# order the edges so they are not that messy
			edgeListChar<-edgeListChar[order(edgeListChar[,1]), ]
			
			
			# make a graph from that 
				graphArgs <- c(list(d=edgeListChar), list(directed=FALSE))
				
				gridGraph <- do.call(igraph::graph_from_data_frame, graphArgs)
		}	
		
		# depending on whether the directed argument was specified or not
		if(!is.null(directed)){
			if(directed){
				gridGraph<-igraph::as.directed(gridGraph)
				
			}
		
		}
		
		
		if(distances){
			edgeListChar<-igraph::get.edgelist(gridGraph)
			p0 <- x@faceCenters[edgeListChar[,1],]
			p1 <- x@faceCenters[edgeListChar[,2],]
			weights<- .Call(Cpp_icosa_ArcDistMany_, p0, p1, x@center, x@r)
			igraph::E(gridGraph)$dist <- weights
		}
		
		
		# subset, if necessary
			graph <- igraph::induced_subgraph(gridGraph, v=rownames(x@faces))
		
		return(graph)
})



#' Create or instantiate an 'igraph' class graph from the faces of a penta-hexagonal grid
#'
#' The function can be applied to a hexagrid class object.
#' @return The function returns an 'igraph' graph.

#' @rdname gridgraph-methods
#' @aliases gridgraph-hexagrid-method
#' @exportMethod gridgraph
setMethod(
	f="gridgraph",
	signature="hexagrid",
	definition= function(x, directed=FALSE,distances=FALSE){
	
	# get the edges of the original trigrid
		edgeListChar<-gsub("P", "F",x@skeleton$edgeTri)
		rownames(edgeListChar)<-NULL
	
		# depending on whether the directed argument was specified or not
		if(!is.null(directed)){
			if(directed==TRUE){
				# get rid of the double edges
				edgeList2<-cbind(edgeListChar[,2],edgeListChar[,1])
				edgeListChar<-rbind(edgeListChar,edgeList2)
			}
			
		
		}
		
		# order the edges so they are not that messy
		edgeListChar<-edgeListChar[order(edgeListChar[,1]), ]
		
		# make a graph from that 
			graphArgs <- c(list(d=edgeListChar), list(directed=directed))
			
			gridGraph <- do.call(igraph::graph_from_data_frame, graphArgs)
		
		
			if(distances){
				edgeListChar<-igraph::get.edgelist(gridGraph)
				p0 <- x@faceCenters[edgeListChar[,1],]
				p1 <- x@faceCenters[edgeListChar[,2],]
				weights<- .Call(Cpp_icosa_ArcDistMany_, p0, p1, x@center, x@r)
				igraph::E(gridGraph)$dist <- weights
			}
		
		
		# subset, if necessary
			graph <- igraph::induced_subgraph(gridGraph, v=rownames(x@faces))
		
		return(graph)

})



#' Translating the grid object in 3d Cartesian space
#' 
#' The function translates the coordinates of a grid object with the specified 3d vector.
#' @name translate
#' 
#' @param gridObj A \code{trigrid} or \code{hexagrid} class object. 
#' 
#' @param vec A numeric vector of length 3. This is the translation vector.
#'
#' @examples
#'  # create a grid and plot it
#'	g <- trigrid(3)
#'	lines3d(g)
#'	# translate the grid to (15000,15000,15000)
#'  g2 <- translate(g, c(15000,15000,15000))
#' 	 lines3d(g2)
#' 
#' @return The same grid structure as the input, but with translated coordinates.
#' 	
#' @exportMethod translate
#' @rdname translate-methods
setGeneric(
	name="translate",
	package="icosa",
	def=function(gridObj,vec){
		standardGeneric("translate")
	}
)

#' @rdname translate-methods
setMethod(
	"translate",
	signature=c("trigrid","numeric"),
	def=function(gridObj, vec){
		if(!is.numeric(vec)| 0<sum(is.na(vec))) stop("Invalid transformation vector.")
		for(vc in 1:3){
			gridObj@vertices[,vc] <- gridObj@vertices[,vc]+vec[vc]
			gridObj@faceCenters[,vc] <- gridObj@faceCenters[,vc]+vec[vc]
			gridObj@skeleton$v[,vc] <- gridObj@skeleton$v[,vc]+vec[vc]
			gridObj@center[vc] <- gridObj@center[vc] +vec[vc]
		}
		return(gridObj)
	}
)

#' @rdname translate-methods
#' @exportMethod translate
setMethod(
	"translate",
	signature=c("hexagrid","numeric"),
	def=function(gridObj, vec){
		if(!is.numeric(vec) | 0<sum(is.na(vec))) stop("Invalid transformation vector.")
		for(vc in 1:3){
			gridObj@vertices[,vc] <- gridObj@vertices[,vc]+vec[vc]
			gridObj@faceCenters[,vc] <- gridObj@faceCenters[,vc]+vec[vc]
			gridObj@skeleton$v[,vc] <- gridObj@skeleton$v[,vc]+vec[vc]
			gridObj@skeleton$plotV[,vc] <- gridObj@skeleton$plotV[,vc]+vec[vc]
			gridObj@center[vc] <- gridObj@center[vc] +vec[vc]
		}
		return(gridObj)
	}
)


#' Locate grid faces based on their positions on a map
#' 
#' The function returns which grid faces contain the points clicked in a plot.
#' 
#' @param gridObj (\code{trigrid} or \code{hexagrid}) The grid object.
#' @param n (\code{integer}) The number of points to be looked up.
#' @param output (\code{character}) Type of output: \code{"faces"} returns only the face names of the points, \code{"full"} returns the coordinates as well.
#' @param ... arguments passed to the \code{\link[graphics]{locator}} function.
#' 
#' @export
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

