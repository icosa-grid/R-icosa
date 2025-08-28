
#' Rotation of 3d objects
#'
#' Multiple implementations of rotations for coordinate transformation.
#'
#' The function implements 3D rotations of various class of objects, that are ultimately reduced to individual points. Internally, point rotation is implemented with 3-axis rotations (Method 1),
#' that are implemented in the X-Y-Z order (note that 3d rotations are not commutative!). For this reason it is not recommended to re-rotate an already rotated object, unless the purpose is to achieve random orientation.
#'
#' Method 2 parametrizes rotation with three arguments (\code{long}, \code{lat}, \code{reflong}), that can be easier to control. Longitudinal rotations are invariant to the position of the object, but latitudinal
#' rotation is not. An axis in the equatorial plane will be set perpendicular to the reference longitude (\code{reflong})for latitude-based rotation
#' (i.e. the latitude difference will equal the latitudinal rotation value only at the reference longitude). If this is not given, then the reference longitude will be that of the centroid of the point cloud.
#' Note that latitudinal rotation is executed first and only then are the points rotated longitudinally.
#'
#' @param x (\code{matrix}, \code{\link{trigrid}}, \code{\link{hexagrid}}) Input coordinates or grid.
#' @param angles (\code{numeric}): The \code{vector} of rotation in radians (three values in each dimension). If set to \code{"random"}, the rotation will be random (default). Rotations are executed in X-Y-Z order.
#' @param pivot (\code{numeric}): The pivot point of the rotation, \code{vector} of xyz coordinates. For \code{trigrid}-methods it defaults to \code{NA} indicating that the rotation will be around the center of the grid.
#' @param projnote (\code{logical}): Should messages be shown to remind users to regenerate grid projections with 'sf' and 'sp'?
#' @param ... Arguments passed to class-specific methods.
#' @rdname rotate
#' @return Same class object as \code{x}.
#' @exportMethod rotate
"rotate"


# Rotation method of trigrid class
#' @rdname rotate
setMethod(	
	f="rotate",
	signature="trigrid",
	definition= function(x, angles="random", pivot=NA, projnote=TRUE){
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

		if(projnote){
			if(suppressWarnings(!is.na(x@sp))){
				message("Please rerun newsp() to regenerate the 2d representation with 'sp'!")
			}

			if(suppressWarnings(sum(is.na(x@sf))==0)){
				message("Please rerun newsf() to regenerate the 2d representation with 'sf'!")
			}
		}
		
		
		# rotate the points
			vertices <- rotateMultiplePoints(x@vertices, angles=angles, origin=orig)
			# vertices<-t(apply(x@vertices, 1, rotateOnePoint, angles=angles, origin=orig))
			# colnames(vertices)<-c("x","y","z")

		#do the same for the face centers
			faceCenters <- rotateMultiplePoints(x@faceCenters, angles=angles, origin=orig)
			# faceCenters<-t(apply(x@faceCenters, 1, rotateOnePoint, angles=angles, origin=orig))
			# colnames(faceCenters)<-c("x","y","z")
		
		# skeleton
			verticesSkel <- rotateMultiplePoints(x@skeleton$v, angles=angles, origin=orig)
			# verticesSkel<-t(apply(x@skeleton$v, 1, rotateOnePoint, angles=angles, origin=orig))
			# colnames(verticesSkel)<-c("x","y","z")
		
		# add the new, rotated tables to the original object's copy
			x@vertices <- vertices
			x@faceCenters <- faceCenters
			x@skeleton$v <- verticesSkel
		
		# in case the trigrid is a hexagrid too
		if(inherits(x,"hexagrid")){
			plotV <- rotateMultiplePoints(x@skeleton$plotV, angles=angles, origin=orig)
			# plotV<-t(apply(x@skeleton$plotV, 1, rotateOnePoint, angles=angles, origin=orig))
			# colnames(plotV)<-c("x","y","z")
			x@skeleton$plotV <- plotV
		
		}
		
		# update the orientation
		x@orientation <- x@orientation+angles

		return(x)
		
	}
)



#' Translating an icosahedral grid object in 3d Cartesian space
#' 
#' The function translates the coordinates of a grid object with the specified 3d vector.
#' @name translate
#' 
#' @param gridObj (\code{\link{trigrid}} or \code{\link{hexagrid}}) Icosahedral grid object. 
#' 
#' @param vec (\code{numeric}) A vector of length 3. This is the translation vector.
#'
#' @examples
#' # create a grid and plot it
#' g <- trigrid(3)
#' # lines3d(g)
#' # translate the grid to (15000,15000,15000)
#' g2 <- translate(g, c(15000,15000,15000))
#' # lines3d(g2)
#' 
#' @return The same grid structure as the input, but with translated coordinates.
#' 	
#' @exportMethod translate
#' @rdname translate
setGeneric(
	name="translate",
	package="icosa",
	def=function(gridObj,vec){
		standardGeneric("translate")
	}
)

#' @rdname translate
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

#' @rdname translate
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
