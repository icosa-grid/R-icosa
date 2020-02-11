#' This function will create \code{facelayer} linked to a \code{trigrid} or \code{hexagrid} object
#' 
#' The grids themselves are scaffolds for the assigned data. The data themselves are stored in containers which are linked to the the grids.
#' 
#' @param gridObj A \code{hexagrid} or \code{trigrid} object.
#' 
#' @param value The \code{facelayer} will be initialized with these values/this value
#' @examples
#' 	g <- trigrid(c(4,4))
#' 	fl <- facelayer(g, 1:length(g))
#' 	faces3d(fl)
#' @exportClass facelayer
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
			if(length(value)==.Object@length){
				.Object@values <- value
			}else{
				stop("Length of input values does not equal facelayer length.")
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



