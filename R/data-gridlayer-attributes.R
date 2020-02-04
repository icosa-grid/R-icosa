
if(requireNamespace("raster", quietly = TRUE)){
	setGeneric("values", def=raster::values)
}else{
	setGeneric(
		name="values",
		def=function(x,...){
			standardGeneric("values")
		}
	)
}

#' Extract values from a gridlayer
#'
#' The function will get the \code{@values} slot of a \code{gridlayer} object.
#'
#' @param x a gridlayer derived object.
#' @aliases gridlayer-values-method
#' @rdname values-methods
#' @exportMethod values
setMethod(	
	f="values",
	signature="gridlayer",
	definition= function(x){
		return(x@values)
	}
)
#' The length of a \code{gridlayer} class object.
#' 
#' This function returns the number of values present in the \code{gridlayer}.
#' @rdname length-methods
#' @aliases gridlayer-length-method
#' @exportMethod length
setMethod(	
	f="length",
	signature="gridlayer",
	definition= function(x){
		x@length
	}
)

#' The face names in a \code{gridlayer} class object
#'
#' Function to extract the registered face names to which the \code{gridlayer} renders information.
#'
#' @param x A \code{gridlayer} class object.
#' @return \code{character} vector, the names of the faces.
#' @rdname names-methods
#' @aliases gridlayer-names-method
#' @exportMethod names
setMethod(	
	f="names",
	signature="gridlayer",
	definition= function(x){
		x@names
	}
)
		

if(requireNamespace("raster", quietly = TRUE)){
	setGeneric("values<-", def=raster::`values<-`)
}else{
	setGeneric(
		name="values<-",
		def=function(x,value){
			standardGeneric("values<-")
		}
	)
}
	

#' Replace values from a \code{gridlayer}
#'
#' Shorthand function to replace all values of a \code{gridlayer} object.
#'
#' @param value replacement values.
#' @aliases gridlayer-set-values-method
#' @rdname values-methods
#' @exportMethod values<-
setReplaceMethod(	
	f="values",
	signature="gridlayer",
	definition= function(x, value){
		x@values<-value
		if(length(x@values)!=x@length){
			stop("Wrong replacement length.")
		}else{
			return(x)
		}
	}
)


