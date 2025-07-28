#' A \code{facelayer} linked to a \code{trigrid} or \code{hexagrid} object
#' 
#' The grids themselves are scaffolds for the assigned data. The data are stored in containers which are linked to the grids.
#' 
#' @param gridObj (\code{\link{hexagrid}} or \code{\link{trigrid}}) The linked grid object.
#' 
#' @param value (\code{logical},\code{numeric} or \code{character}) The \code{facelayer} will be initialized with these values/this value
#' @examples
#' g <- trigrid(c(4,4))
#' fl <- facelayer(g, 1:length(g))
#' # faces3d(fl)
#' @exportClass facelayer
#' @return A \code{facelayer} class object.
facelayer <- setClass(
	#name
	"facelayer",
	contains="gridlayer"
) 


#' @export facelayer
setMethod("initialize", signature = "facelayer",
	definition = function(.Object, gridObj, value=NA){
		.Object@grid <- deparse(substitute(gridObj))
		.Object@tessellation <- gridObj@tessellation
		nam<-class(gridObj)
		names(nam)<-NULL
		.Object@gridclass <- nam
		.Object@names <- rownames(gridObj@faces)
		.Object@length <- length(.Object@names)
		if(length(value)==1){
			.Object@values <- rep(value, .Object@length)
		}else{
			# ensure that this is a one dimensonal object
			valDim <- dim(value)
			if(!is.null(valDim)) if(length(valDim)>1) stop("Only one-dimensional values are supported.")

			# if there are names
			if(!is.null(names(value))){
				# all of them have to be a valid face name
				if(all(names(value)%in%.Object@names)){
					# and then replace based on the names
					almost <- value[.Object@names]

					# omit one dimensionality
					if(!is.null(valDim)) dim(almost) <- NULL

					# simplify tables
					if(inherits(almost,  "table")){
						class(almost) <- "integer"
					}
					.Object@values <- almost
				}
			# unnamed value
			}else{
				if(length(value)==.Object@length){
					.Object@values <- value
				}else{
					stop("Length of input values does not equal facelayer length.")
				}
			}

		}
	
		return(.Object)
	}
	
)

# utility function to test whether the grid linked to a layer can host it or not
checkLinkedGrid <- function(gridObj, fl){
	if(!prod(gridObj@tessellation)==prod(fl@tessellation) |
		fl@gridclass!=class(gridObj) |
		sum(fl@names%in%rownames(gridObj@faces))!=length(fl@names))
		stop("The linked grid doesn't match the facelayer's grid structure.")
}



