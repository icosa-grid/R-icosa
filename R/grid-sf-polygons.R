#' Add a \code{\link[sf:sf]{sf}} object to a predefined slot in a \code{\link{trigrid}} or \code{\link{hexagrid}} object
#'
#' @name newsf
#		
#' @rdname newsf
#' @param x (\code{\link{trigrid}} or \code{\link{hexagrid}}) An icosahedral grid.
#' @param res (\code{numeric}) The number of points inserted between two vertices, passed to \code{\link{SpPolygons}}.
#' 
#' @return A \code{\link{trigrid}} or \code{\link{hexagrid}} object with the new \code{@sf} slot.
#' @examples
#'	a<-trigrid(4)
#'	a<-newsf(a)
#'	plot(a)
#' @exportMethod newsf
setGeneric(
	name="newsf",
	package="icosa",
	def=function(x,res=NULL){
		standardGeneric("newsf")
	}

)

#' @rdname newsf
setMethod(
	"newsf",
	signature="trigrid",
	definition=function(x, res=NULL){
		x@sf<-sf::st_as_sf(SpPolygons(x,res=res))
		x@sf$faces <- rownames(x@faces)
		rownames(x@sf) <- rownames(x@faces)
		return(x)
	}
)

