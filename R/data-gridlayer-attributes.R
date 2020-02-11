#' @rdname values
#' @param ... Arguments passed to class-specific methods.
#' @usage values(x,...)
"values" 

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

#' Extract and replace values from a gridlayer
#'
#' The function will get the \code{@values} slot of a \code{gridlayer} object.
#'
#' @param x a gridlayer derived object.
#' @rdname values
#' @exportMethod values
setMethod(	
	f="values",
	signature="gridlayer",
	definition= function(x){
		return(x@values)
	}
)


#' @usage values(x) <- value
#' @rdname values
"values<-"

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
	

#' @param value replacement values.
#' @rdname values
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


#' The length of a \code{gridlayer} class object.
#' 
#' This function returns the number of values present in the \code{gridlayer}.
#' @rdname length
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
#' @rdname names
#' @exportMethod names
setMethod(	
	f="names",
	signature="gridlayer",
	definition= function(x){
		x@names
	}
)


