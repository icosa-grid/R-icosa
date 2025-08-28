#' Icosahedral grid-based density estimation
#
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
#' @rdname grapply
#' @export
gridensity <- function(x, y, out, trials=100, FUN=mean){
	warning("This function is deprecated. Please use\n  'grapply(x=x, out=out,y=y, iter=trials, APP=mean, miss=0)' instead.")

	# outsource this to grapply
	res <- grapply(x=x, out=out,y=y, iter=trials, miss=0)

	# return
	return(res)

}


#' Grid Rotation Apply
#'
#' Discretization with iteratively rotated icosahedral grids
#'
#' Simple discretization of spatial data is subject to error due to the random assignment to grid cells. \code{grapply} offers a framework for the Monte Carlo estimation of the expectation of a function that normally can be applied to
#' a discretized set of points. This function \code{FUN} takes the input the \code{data.frame} (or \code{matrix}, which will be coerced into one) as argument, with the addition of a new variable \code{cells}, which includes
#' face identifiers for the point set given the random grid rotation (the output of \code{locate}). See examples for a step-by-step description.
#' @param x Matrix of longitude, latitude data, \code{\link[sf:sf]{sf}} class, or \code{\link[sp:SpatialPoints]{SpatialPoints}} Point cloud.
#' @param y \code{\link{trigrid}} or \code{\link{hexagrid}} An icosahedral grid.
#' @param out \code{\link{trigrid}}, \code{\link{hexagrid}} or \code{\link[terra:rast]{SpatRaster}}output structure. If not given, then the input argument
#' @param trials \code{numeric} value, the number of iterations.
#' @param FUN \code{function} The function to be applied on the iteration results. It defaults to the number of points, but it can be used to
#' @param FUN.args \code{list} Additional arguments passed to \code{FUN}.
#' @param APP \code{function} The function to be applied on the iteration results. If set to \code{NULL}, it will return the stack of results for subsequent processing.
#' @param APP.args \code{list} Additional arguments passed to \code{APP}.
#' @param counter \code{logical} Should a loop counter be shown?
#' @param iter \code{numeric} Value, the number of iterations.
#' @param miss \code{numeric} A single default value used in positions in \code{out} where no value is produced by \code{FUN} in an iteration trial.
#' @param coords \code{character} Column names to find longitude and latiude variables in \code{x}.
#' @param ... Arguments passed to class-specific methods.
#' @examples
#' # example to be run if terra is present
#' if(requireNamespace("terra", quietly=TRUE)){
#'
#'  # randomly generated points
#'  x <- rpsphere(100, output="polar")
#'
#'  # bandwidth grid
#'  gr <- hexagrid(deg=13)
#'
#'  # output structure
#'  out <- terra::rast(res=5)
#'
#'  # Manual example - for understanding what FUN is doing
#'  cell<- locate(gr, x)
#'	yNew <- cbind(as.data.frame(x), cell=cell)
#'  # this is the default function (here named CellCount)
#'  CellCount <- function(x) table(x$cell)
#'  counts <- CellCount(yNew)
#'  # create facelayer
#'  fl <- facelayer(gr, counts)
#'
#'  # and resample
#'  oneOut <- icosa::resample(fl, out)
#'  terra::plot(oneOut, main="Density with default grid orientation")
#'  points(x, pch=3, col="red")
#'
#'  # for density estimation
#'  o <- grapply(x=x, out=out,y=gr,  iter=7, FUN=CellCount, miss=0)
#'
#'  # visualize results
#'  terra::plot(o)
#'  points(x, pch=3, col="red")
#' }
#'
#' @export
#' @rdname grapply
setGeneric(
	name="grapply",
	def=function(x,out, ...){
		standardGeneric("grapply")
	}
)

#' @rdname grapply
setMethod(
	"grapply",
	signature=c(x="data.frame", out="SpatRaster"),
	definition=function(x, out, y, coords=c("long", "lat"), iter=100, FUN=function(x) table(x$cell), APP=mean, miss=NA, APP.args=NULL, counter=TRUE, FUN.args=NULL){

		if(length(miss)>1) stop("The 'miss' argument must be a single numeric or NA.")

		# function to iterate
		ITER <- match.fun(FUN)

		# the averaging function
		if(!is.null(APP)) APP <- match.fun(APP)

		# protype run-> enforce single argument return
		if(!requireNamespace("terra", quietly=T)) stop("This function requires the 'terra' package.")

		# the coordinates
		coordDat <- x[, coords]

		# iterate
		for(i in 1:iter){
			# execute a random rotation
			newY <- rotate(y)

			# locate the points on the rotated grid
			cells <- locate(newY, coordDat)

			# redefine object
			thisX <- cbind(as.data.frame(x), cell=cells)

			# the function
			config <- c(list(x=thisX), FUN.args)
			current <- do.call(ITER, config)

			# put them on a facelayer
			fl<- facelayer(newY)
			fl[] <-current

			# resample to output
			newZ <- resample(fl, out)
			if(!is.na(miss)){
				terra::values(newZ)[is.na(terra::values(newZ))] <- miss
			}

			if(i == 1){
				stack <- newZ
			}else{
				stack <- c(stack, newZ)
			}
			if(counter){
				cat(i, "\r")
				flush.console()
			}
		}

		if(!is.null(APP)){
			# the configures
			conf <- list(x=stack, fun=APP)

			# the provided arguments
			conf <- c(conf, APP.args)

			# do a call
			res <- do.call(terra::app, conf)

		}else{
			res <- stack
		}

		return(res)
})

#' @rdname grapply
setMethod(
	"grapply",
	signature=c(x="data.frame", out="trigrid"),
	definition=function(x, out, y=out, coords=c("long", "lat"), iter=100, FUN=function(x) table(x$cell), APP=mean, miss=NA, APP.args=NULL, counter=TRUE, FUN.args=NULL){

		# function to iterate
		ITER <- match.fun(FUN)

		# the averaging function
		if(!is.null(APP)) APP <- match.fun(APP)

		# the stack of results
		stack <- matrix(NA, ncol=nrow(out@faces), nrow=iter)
		colnames(stack)  <- rownames(out@faces)

		# the coordinates
		coordDat <- x[, coords]

		for(i in 1:iter){
			# execute a random rotation
			newY <- rotate(y)

			# locate the points on the rotated grid
			cells <- locate(newY, coordDat)

			# redefine object
			thisX <- cbind(as.data.frame(x), cell=cells)

			# the function
			config <- c(list(x=thisX), FUN.args)
			current <- do.call(ITER, config)

			# put them on a facelayer
			fl<- facelayer(newY)
			fl[] <-current

			# resample to output
			newZ <- resample(fl, out)
			if(!is.na(miss)){
				newZ[is.na(newZ)] <- miss
			}

			stack[i,names(newZ)] <- newZ

			if(counter){
				cat(i, "\r")
				flush.console()
			}
		}

		if(!is.null(APP)){
			# the configures
			conf <- list(X=stack, FUN=APP, MARGIN=2)

			# the provided arguments
			conf <- c(conf, APP.args)

			# do a call
			res <- do.call(apply, conf)

		}else{
			res <- stack
		}

})

#' @rdname grapply
setMethod(
	"grapply",
	signature=c(x="matrix", out="SpatRaster"),
	definition=function(x, out, ...){
		grapply(x=as.data.frame(x), out=out, ... )
})



#' @rdname grapply
setMethod(
	"grapply",
	signature=c(x="matrix", out="trigrid"),
	definition=function(x, out, ...){
		grapply(x=as.data.frame(x), out=out, ... )

})

#' @rdname grapply
setMethod(
	"grapply",
	signature=c(x="data.frame", out="missing"),
	definition=function(x, out, y, ...){
		grapply(x, out=y, y=y, ... )
})

#' @rdname grapply
setMethod(
	"grapply",
	signature=c(x="matrix", out="missing"),
	definition=function(x, out, y, ...){
		grapply(x=as.data.frame(x), out=y, y=y, ... )
})
