#' Resampling a facelayer to a different resolution
#' 
#' The function applies different resampling algorithms. Currently there are only two implemented methods, one for upscaling and one for downscaling. The downscaling method "average" will tabluate all face centers from the high resolution grid that fall on a coarse resolution cell and average them. The upscaling method "ebaa" (edge breakpoint area approximation) will estimate the areas covered by the high resolution cells using the number of edge breakpoints.
#' 
#' @param x (\code{\link[terra:rast]{SpatRaster}}, \code{\link{facelayer}}) Object to resample.  
#' 
#' @param y  (\code{\link{hexagrid}} or \code{\link{trigrid}}) Object describing the target structure.
#' @param res (\code{numeric}) Value indicating the precision of area estimation during the upscaling (\code{facelayer}-method). In case the \code{"ebaa"} method is chosen, the variable indicate the number of breaking points on an edge.
#' 
#' @param method (\code{character}) The name of the algorithm used for resampling. 
#' 
#' @return A named \code{numeric} vector.
#' 
#' @examples
#' # create a grid
#' g <- trigrid(c(4,4))
#' # create a data layer
#' fl <- facelayer(g)
#' fl@values<-rnorm(length(fl))
#' # target structure
#' h <- trigrid(4)
#' # resampling
#' res <- resample(fl, h)
#' fl2<-facelayer(h)
#' fl2@values[] <- res
#'
#' @exportMethod resample
#' @rdname resample
setMethod(
	"resample",
	signature=c("facelayer", "trigrid"),
	definition=function(x, y,method=NULL,res=5){
	#x:layer, y new grid
		oldGrid<-get(x@grid)
		values<-x@values
		#only if values is a numeric vector!
		if(!is.numeric(x@values)){
			
		}
		
		# do some checking immediately: upscaling or downscaling
		# downscaling
		if(length(oldGrid)>length(y)){
			downscale <- TRUE
			if(is.null(method)){
				method <- "average"
			}else{
				# perform a check of the potential resampling methods
				if(!method%in%c("average")){
					stop("Invalid downscaling method. ")
				}
			}
			
		} else {
			downscale <- FALSE
			if(is.null(method)){
				method <- "ebaa"
			}else{
				# perform a check of the potential resampling methods
				if(!method%in%c("ebaa")){
					stop("Invalid upscaling method.")
				}
			}
			
		}
		
		# do some checking
		#the two grids should have the same centers and radius
		
#		# reorder the values to the internal order of oldGrid
#		intValues<-values
#		if(class(oldGrid)=="trigrid"){
#			intValues[oldGrid@skeleton$uiF]<-values
#		}
#		
#		if(class(oldGrid)=="hexagrid"){
#			intValues<-values[oldGrid@skeleton$aF]
#		}
		
		# easier case:
		# given that downsampling occurs
		if(downscale){
			if(method=="average"){
				#oldgrids facecenters
				loc<-locate(y, oldGrid@faceCenters)
				newVals<-tapply(INDEX=loc, X=values, function(x){mean(x, na.rm=TRUE)})
				newVals<-newVals[rownames(y@faces)]
				nVNames<-names(newVals)
				newVals<-as.numeric(newVals)
				names(newVals)<-nVNames
			}
		
		}
		
		#given that upsampling occurs
		# edge breakpoint area approximation 
		if(!downscale){
			if(method=="ebaa"){
				
				# basic algorithm
				# 1. break down every single edge of the finer, new grid using SplitArc() and produce a table with xyz coordinates
				if(inherits(y,"trigrid") & !inherits(y, "hexagrid")){
					newGridF<-y@skeleton$f[y@skeleton$f[,4]==(length(y@tessellation)-1),]
					
					# c++ function will look up produce these points
					newPoints<-.Call(Cpp_icosa_ExpandEdgesByFacesTri_, y@skeleton$v, newGridF, y@center, res)
					newPoints[,4] <- newPoints[,4]+1
					# outer order
					fNames<-names(sort(y@skeleton$uiF))
					newNames <- fNames[newPoints[,4]]
				}
				if(inherits(y, "hexagrid")){
					newGridF<-y@skeleton$f[y@skeleton$f[,4]==-6,]
				
					# c++ function will look up produce these points
					newPoints<-.Call(Cpp_icosa_ExpandEdgesByFacesTri_, y@skeleton$v, newGridF, y@center, res)
					newPoints[,4] <- newPoints[,4]+1
					
					# create a proper UIF table
					tempUIF <- as.numeric(t(y@skeleton$uiF))
					names(tempUIF)<-rep(rownames(y@skeleton$uiF), each=12)
					
					# get rid of NAs
					tempUIF<-tempUIF[!is.na(tempUIF)]
					
					# outer order
					fNames<-names(sort(tempUIF))
					newNames <- fNames[newPoints[,4]]
				
				}
				
				
				# 2. look up these new coordinates in the old grid, it is not a problem if they are not found (locate produces NAs)
					oldCells<-locate(oldGrid, newPoints[,1:3])
					
					# 3. get the values from the facelayer that corresponds to these face designations
					names(x@values) <- x@names
					oldVals<-x@values[oldCells]
					
					# 4. average them out with tapply()
					newVals<-tapply(INDEX=newNames, X=oldVals, mean, na.rm=TRUE)
				
					# 5. you have an estimation based on the approximate coverages
					nVNames<-names(newVals)
					newVals<-as.numeric(newVals)
					names(newVals)<-nVNames
				
			}
		}
		
		return(newVals)
	}
	
)


#' @exportMethod resample
#' @rdname resample
setMethod(
	"resample",
	signature=c("facelayer", "SpatRaster"),
	definition=function(x, y,method=NULL,res=5){
		# make a copy of the original entity
		newRast <- y
		
		# get raster coordinates
		allCoords <- as.data.frame(terra::xyFromCell(y, 1:terra::ncell(y)))

		# make the sf with crs
		sfPoints <- sf::st_as_sf(
			as.data.frame(allCoords), coords=c("x", "y"), crs=terra::crs(y))
		# locate the points
		tiedGrid <- dynGet(x@grid)

		# look up the cells
		cells <- locate(tiedGrid, sfPoints)	

		# get the appropriate values
		terra::values(newRast) <- values(x)[cells]

		# return the thing
		return(newRast)

})


#' Icosahedral grid-based density estimation
#'
#' Spatial density estimation algorithm based on rotation of icosahedral grids.
#'
#' Any points set can be binned to an icosahedral grid (i.e. number of incidences can be counted), which will be dependent on the exact positions of grid cells. Rotating the grid in 3d space will result in a different distribution of counts. This distribution can be resampled to a standard orientation structure. The size of the icosahedral grid cells act as a bandwidth parameter.
#'
#' The implemented algorithm 1) takes a point cloud (\code{x})) and an icosahedral grid \code{y} 2) randomly rotates the icosahedral grid, 3) looks up the points falling on grid cells, 4) resamples the grid to a constant orientation object (either \code{\link{trigrid}}, \code{\link{hexagrid}} or \code{\link[terra:rast]{SpatRaster}}). Steps 2-4 are repeated \code{trial} times, and then \code{FUN} is applied to every vector of values that have same spatial position.
#' 
#' @param x Matrix of longitude, latitude data, \code{\link[sf:sf]{sf}} class, or \code{\link[sp:SpatialPoints]{SpatialPoints}} Point cloud.
#' @param y \code{\link{trigrid}} or \code{\link{hexagrid}} An icosahedral grid.
#' @param out \code{\link{trigrid}}, \code{\link{hexagrid}} or \code{\link[terra:rast]{SpatRaster}}output structure.
#' @param trials \code{numeric} value, the number of iterations.  
#' @param FUN \code{function} The function to be applied on the iteration results.
#' @return Either named numeric vector, or a \code{\link[terra:rast]{SpatRaster}} object. If FUN is set to \code{NULL}, the output will be either a \code{matrix} or \code{\link[terra:rast]{SpatRaster}}.
#' @examples
#' # example to be run if terra is present
#' if(requireNamespace("terra", quietly=TRUE)){
#' 
#'  # randomly generated points
#'  x <- rpsphere(100, output="polar")
#' 
#'  # bandwidth grid
#'  y <- hexagrid(deg=13)
#' 
#'  # output structure
#'  out <- terra::rast(res=5)
#'
#'  # the function
#'  o <- gridensity(x, y, out, trials=7)
#' 
#'  # visualize results
#'  terra::plot(o)
#'  points(x, pch=3)
#' }
#' 
#' @export
gridensity <- function(x, y, out, trials=100, FUN=mean){
	# find function
	FUN <- match.fun(mean)

	if(inherits(out, "SpatRaster")){
		if(!requireNamespace("terra", quietly=T)) stop("This module requires the 'terra' package.")

		for(i in 1:trials){
			# execute a random rotation
			newY <- rotate(y)

			# locate the points on the rotated grid 
			cells <- locate(newY, x)

			# tabulate their counts
			cellTab <- table(cells)

			# put them on a facelayer
			fl<- facelayer(newY)
			fl[] <- cellTab

			# resample to output
			newZ <- resample(fl, out)
			terra::values(newZ)[is.na(terra::values(newZ))] <- 0

			if(i == 1){
				stack <- newZ
			}else{
				stack <- c(stack, newZ)
			}
			cat(i, "\r")
			flush.console()
		}

		if(!is.null(FUN)){
			res <- terra::app(stack, fun=FUN)
		}else{
			res <- stack
		}
	}

	if(inherits(out, "trigrid")){
		stack <- matrix(NA, ncol=nrow(out@faces), nrow=trials)
		colnames(stack)  <- rownames(out@faces)

		for(i in 1:trials){
			# execute a random rotation
			newY <- rotate(y)

			cells <- locate(newY, x)
			cellTab <- table(cells)
			fl<- facelayer(newY)
			fl[] <- cellTab

			newZ <- resample(fl, out)
			newZ[is.na(newZ)] <- 0
			
			stack[i,names(newZ)] <- newZ
			cat(i, "\r")
			flush.console()
		}

		# calculate the thing for all of them
		if(!is.null(FUN)){
			res <- apply(stack, 2, FUN=FUN)
		}else{
			res <- stack
		}
	}
	return(res)

}
