# This chunk of the code will use the created grid structures as as hosts
# and link data to them using layers similar to the raster package

# the most important thing with this is that it can be useful for 
# both the trigrid and the hexagrid objects!


# S4 version of the layering

#gridlayer basic class
	#class definition
	gridlayer <- setClass(
		"gridlayer",
		
		slots= c(
			grid= "character",
			tessellation ="numeric",
			gridclass = "character",
			names = "character",
			values= "vector",
			length= "integer"
		)
	
	)

# methods for the gridlayer	
# show	
	setMethod("show", signature ="gridlayer",
		definition = function(object){
		#	cat(paste(class(object), "of", object@grid ,"with", object@length, class(object@values), "values\n", sep=" "))
		#	cat(object@values, fill=TRUE)
			
			actGrid<-get(object@grid)
			
			
			cat(paste("class        : ", class(object),"\n", sep=""))
			cat(paste("linked grid  : \'", object@grid,"\' (name), ", class(actGrid)," (class), ",
				paste(as.character(object@tessellation), collapse=",")
				, " (tessellation)", "\n", sep=""))
			cat(paste("dimensions   : ", length(object), " (values)", " @ ", "mean edge length: ",round(actGrid@edgeLength[1],2), " km, " ,round(actGrid@edgeLength[2],2), " degrees",  "\n" ,sep=""))
			if(sum(is.na(object@values))==length(object)){
				mx<-NA
				mn<-NA
			}else{
				mx<-max(object@values, na.rm=TRUE)
				mn<-min(object@values, na.rm=TRUE)
			}
			
			cat(paste("values       : ", class(object@values)), "\n")
			cat(paste("max value    : ", mx), "\n")
			cat(paste("min value    : ", mn), "\n")
			cat(paste("missing      : ", sum(is.na(object@values))), "\n")
			
		} 
	)

	
#basic operators for the layers	
{

	# stats
	setMethod("Math", signature="gridlayer", 
	definition=function(x){
		methods::callGeneric(x@values)
	}
	)
	
	setMethod("Summary", signature="gridlayer", 
	definition=function(x){
		methods::callGeneric(x@values)
	}
	)
	
	
	setMethod("Ops", signature=c("gridlayer", "numeric"),
	definition=function(e1,e2){
		methods::callGeneric(e1@values,e2)
	}
	)
	
	
	setMethod("Ops", signature=c("gridlayer", "gridlayer"),
	definition=function(e1,e2){
		methods::callGeneric(e1@values,e2@values)
	}
	)
	
	# basic statistics
	setMethod("mean", signature="gridlayer", 
	definition=function(x,...){
		methods::callGeneric(x@values,...)
	}
	)
	
	# basic statistics
	setMethod("sd", signature="gridlayer", 
	definition=function(x,na.rm=FALSE){
		methods::callGeneric(x@values,na.rm=na.rm)
	}
	)
	
	# basic statistics
	setMethod("summary", signature="gridlayer", 
	definition=function(object,...){
		methods::callGeneric(object@values,...)
	}
	)
	
	# basic statistics
	setMethod("quantile", signature="gridlayer", 
	definition=function(x,...){
		methods::callGeneric(x@values,...)
	}
	)
	
	# basic statistics
	setMethod("median", signature="gridlayer", 
	definition=function(x,na.rm=FALSE){
		methods::callGeneric(x@values,na.rm=na.rm)
	}
	)
	
	
}


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


#' Subsetting a gridlayer object.
#'
#' The function extracts subsets of the \code{gridlayer} depending on different criteria.
#'
#' The following methods are incorporated into the function: If the subsetVector argument is a vector of integers, they will be interpreted as indices. If the numeric subsetVector contains either the lamin, lamax, lomin or lomax names, the subsetting will be done using the latitude-longitude coordinates outlined by these 4 values. Logical subsetting and subsetting by face names are also possible.
#'
#' @param x The gridlayer object to be subsetted.
#' @param subsetVector Vector object indicating the faces to be subsetted.
#'
#' @rdname subset-methods
#' @aliases subset-gridlayer-method
#' @exportMethod subset
setMethod(
	"subset",
	signature="gridlayer",
	definition=function(x, subsetVector){
		if(is.numeric(subsetVector)){
			#add checking for lat/long subsetting
			# lat-long mode of subsetting
			potConds<-c("lamin", "lamax", "lomin", "lomax")
			if(sum(names(subsetVector)%in%potConds)>0){
				#if it contains an unitelligible names
				if(sum(!names(subsetVector)%in%potConds)>0) 
					warning("Some subscript condition names were not recognized.")
				
				
				#in case you want something at the dateline
				normal <- T
				if(sum(c("lomax", "lomin")%in%names(subsetVector))==2){
					if(subsetVector["lomin"]>subsetVector["lomax"]){
						normal<- F
					}
				}
				
				#get the facecenters
				actGrid<-get(x@grid)
				pol <- CarToPol(actGrid@faceCenters, norad=TRUE, origin=actGrid@center)
				
				boolSelect<-rep(T, nrow(pol))
				
				#longitude
				if(normal){
					#minimum longitude condition
					if("lomin"%in%names(subsetVector)){
						boolSelect <- boolSelect & pol[,1]>=subsetVector["lomin"]
					}
					
					#maximum longitude condition
					if("lomax"%in%names(subsetVector)){
						boolSelect <- boolSelect & pol[,1]<=subsetVector["lomax"]
					}
				}else{
					#minimum longitude condition
					if("lomin"%in%names(subsetVector)){
						boolSelect <- boolSelect & pol[,1]>=subsetVector["lomin"]
					}
					
					#maximum longitude condition
					if("lomax"%in%names(subsetVector)){
						boolSelect <- boolSelect | pol[,1]<=subsetVector["lomax"]
					}
				
				}
				
				#minimum latitude condition
				if("lamin"%in%names(subsetVector)){
					boolSelect <- boolSelect & pol[,2]>=subsetVector["lamin"]
				}
				
				#minimum latitude condition
				if("lamax"%in%names(subsetVector)){
					boolSelect <- boolSelect & pol[,2]<=subsetVector["lamax"]
				}
				
				subsetVector<-rownames(actGrid@faceCenters)[boolSelect]
				# control will pass over to the subsetting by facenames

			}else{
			
			# index subsetting
				y<-x
				y@names<-y@names[subsetVector]
				y@values<-y@values[subsetVector]
				y@length<-length(y@values)
			}
		}
		if(is.logical(subsetVector)){
			if(length(subsetVector)==(length(x@names))){
				subsetVector<-x@names[subsetVector]
			}else{
				stop("Length of logical subscript does not match the facelayer.")
			}
		
		}
		
		if(is.character(subsetVector)){
			if(sum(subsetVector%in%x@names)==length(subsetVector)){
				y<-x
				y@names<-subsetVector
				y@values<-y@values[x@names%in%subsetVector]
				y@length<-length(y@values)
			}
		
		}
		
		return(y)
		
	}
)	


#' @rdname faces-methods
#' @exportMethod faces
setMethod(	
	f="faces",
	signature="gridlayer",
	definition= function(x){
		actGrid<-get(x@grid)
		return(actGrid@faces)
	}
)

#subsetting for layers
#' Extraction from a gridlayer using indices
#' 
#' Shorthand to the subset() function.
#' 
#' @rdname subset-methods
#'
#' @aliases [-gridlayer-index-method
#' @exportMethod "["
setMethod(
	"[",
	signature=c("gridlayer","ANY", "missing"),
	definition=function(x,i){
		subset(x, i)
	
	}
)

#' Extraction from a gridlayer using 'Extent' class object 
#' 
#' Shorthand to the subset() function.
#' 
#' @rdname subset-methods
#'
#' @aliases [-gridlayer-Extent-method
#' @exportMethod "["
setMethod(
	"[",
	signature=c("gridlayer","Extent", "missing"),
	definition=function(x,i){
		#check the extent object
		
		actGrid <- get(x@grid)
		pol <- CarToPol(actGrid@faceCenters, origin=actGrid@center)
		
		boolLong<-pol[,1]>=i@xmin & pol[,1]<=i@xmax
		boolLat<-pol[,2]>=i@ymin & pol[,2]<=i@ymax
		
		nm<-rownames(pol)[boolLong & boolLat]
	
	
		subset(x, nm)
	
	}
)

#' Replacement of elements in a gridlayer object.
#' 
#' Function to replace specific elements in a gridlayer object 
#' 
#' All these methods are implementing direct replacement in the values slot of a layer, depending on criteria used for subsetting. 
#'
#' @param x the gridlayer.
#' @param i the subsetting vector, as in subset(). 
#' @param value the replacement values.
#' @rdname replace-methods
#'
#' @aliases replace-gridlayer-method
#' @exportMethod "[<-"
setReplaceMethod(
	"[",
	signature="gridlayer",
#	definition=function(x,i,j,..., value){
	definition=function(x,i,value){
		y<-x
		#named vector replacement
		if(length(names(value))>0 & missing(i)){
			if(sum(names(value)%in%y@names)==length(value)){
				u<-y@values
				names(u)<-y@names
				u[names(value)]<-value
				y@values<-u
			}
		}else{
		#numeric
			
			if(is.numeric(i)){
				#add checking for lat/long subsetting
				# lat-long mode of subsetting
				potConds<-c("lamin", "lamax", "lomin", "lomax")
				if(sum(names(i)%in%potConds)>0){
					#if it contains an unitelligible names
					if(sum(!names(i)%in%potConds)>0) 
						warning("Some subscript condition names were not recognized.")
					
					
					#in case you want something at the dateline
					normal <- T
					if(sum(c("lomax", "lomin")%in%names(i))==2){
						if(i["lomin"]>i["lomax"]){
							normal<- F
						}
					}
					
					#get the facecenters
					actGrid<-get(x@grid)
					pol <- CarToPol(actGrid@faceCenters, norad=TRUE, origin=actGrid@center)
					
					boolSelect<-rep(T, nrow(pol))
					
					#longitude
					if(normal){
						#minimum longitude condition
						if("lomin"%in%names(i)){
							boolSelect <- boolSelect & pol[,1]>=i["lomin"]
						}
						
						#maximum longitude condition
						if("lomax"%in%names(i)){
							boolSelect <- boolSelect & pol[,1]<=i["lomax"]
						}
					}else{
						#minimum longitude condition
						if("lomin"%in%names(i)){
							boolSelect <- boolSelect & pol[,1]>=i["lomin"]
						}
						
						#maximum longitude condition
						if("lomax"%in%names(i)){
							boolSelect <- boolSelect | pol[,1]<=i["lomax"]
						}
					
					}
					
					#minimum latitude condition
					if("lamin"%in%names(i)){
						boolSelect <- boolSelect & pol[,2]>=i["lamin"]
					}
					
					#minimum latitude condition
					if("lamax"%in%names(i)){
						boolSelect <- boolSelect & pol[,2]<=i["lamax"]
					}
					
					i<-rownames(actGrid@faceCenters)[boolSelect]
					# control will pass over to the subsetting by facenames
	
				}else{
					# index subsetting
					actGrid<-get(x@grid)
	
					subGrid<-subset(actGrid,i)
					i<-rownames(subGrid@faces)
				}
			}
		
			# pass on from the numeric too!
			if(is.character(i)){
				if(sum(i%in%y@names)==length(i)){
					u<-y@values
					names(u)<-y@names
					u[i]<-value
					y@values<-u
				}else{
					stop("Invalid character subscript.")
				}
			}
			if(is.logical(i)){
				y@values[i]<-value
			}
		}
		
		return(y)
	
	}
)



#2. inheriting classes


#facelayer
#'Container for data storage using the faces on icosahedral grid
#'
#'@name facelayer
#'
#'@rdname facelayer-class
#'
#'@export facelayer
facelayer <- setClass(
	#name
	"facelayer",
	contains="gridlayer"
) 

#' Constructor of a \code{facelayer} object
#' 
#' This function will create \code{facelayer} linked to a \code{trigrid} or \code{hexagrid} object
#' 
#' The grids themselves are scaffolds for the assigned data. The data themselves are stored in containers which are linked to the the grids.
#' 
#' @param gridObj A \code{hexagrid} or \code{trigrid} object.
#' 
#' @param value The \code{facelayer} will be initialized with these values/this value
#' @name facelayer  
#' @examples
#' 	g <- trigrid(c(4,4))
#' 	fl <- facelayer(g, 1:length(g))
#' 	faces3d(fl)
#' @aliases facelayer-class
#' @exportClass facelayer
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

# defining an edgelayer is superflous!!!!!	
##edgelayer
#	#class definition
#	edgelayer <- setClass(
#		#name
#		"edgelayer",
#		contains="gridlayer"
#	) 
#	
#	setMethod("initialize", signature = "edgelayer",
#		definition = function(.Object, gridObj){
#			.Object@grid <- deparse(substitute(gridObj))
#			.Object@names <- rownames(gridObj@edges)
#			.Object@length <- length(.Object@names)
#			.Object@values <- rep(NA, .Object@length)
#			return(.Object)
#		}
#		
#	)

#pointlayer
	#class definition
	pointlayer <- setClass(
		#name
		"pointlayer",
		contains="gridlayer"
	) 
	
	setMethod("initialize", signature = "pointlayer",
		definition = function(.Object, gridObj){
			.Object@grid <- deparse(substitute(gridObj))
			.Object@names <- rownames(gridObj@vertices)
			.Object@length <- length(.Object@names)
			.Object@values <- rep(NA, .Object@length)
			return(.Object)
		}
		
	)



#' @rdname vertices-methods
#' @exportMethod vertices
setMethod(	
	f="vertices",
	signature=c("facelayer","character"),
	definition= function(x, output="polar"){
		actGrid <- get(x@grid)
		if(output=="polar"){
			return(CarToPol(actGrid@vertices, origin=actGrid@center, norad=TRUE))
		
		}else{
			return(actGrid@vertices)
		}
	}
)

#' The face centers of a trigrid or hexagrid class object that is linked to a facelayer
#'
#' Shorthand function to return the faceCenters slot of the linked icosahedral grid . 
#
#' @rdname centers-methods
#' @aliases centers-facelayer-method
#' @exportMethod centers
setMethod(	
	f="centers",
	signature="facelayer",
	definition= function(x, output="polar"){
		actGrid <- get(x@grid)
		if(output=="polar"){
			return(CarToPol(actGrid@faceCenters, origin=actGrid@center, norad=TRUE))
		
		}else{
			return(actGrid@faceCenters)
		}
	}
)

#' @rdname edges-methods
#' @aliases facelayer-edges-method
#' @exportMethod edges
setMethod(	
	f="edges",
	signature="facelayer",
	definition= function(x){
		actGrid<-get(x@grid)
		return(actGrid@edges)
	}
)

#' @rdname faces-methods
#' @aliases facelayer-faces-method
#' @exportMethod faces
setMethod(	
	f="faces",
	signature="facelayer",
	definition= function(x){
		actGrid<-get(x@grid)
		return(actGrid@faces)
	}
)



#' 3d plotting of a facelayer of an icosahedral grid or its subset
#'
#' The function is built on the openGL renderer of the R package \code{rgl}. The default plotting window size is 800x800 pixels. In case you want to override this, please
#' use the function with 'defaultPar3d=FALSE' after running 'rgl::par3d(windowRect=<>)'. 
#'  
#' @param x The \code{facelayer} object to be plotted.
#' 
#' @param type A character value specifying the part of the grid to be plotted by the call of the function. 
#' \code{"l"} plots the grid lines (only when frame=FALSE). 
#' \code{"f"} draws the grid faces.
#' \code{FALSE} does not plot the sphere. 
#' @param defaultPar3d Logical value, whether the default settings for par3d() are to be used (windowRect = c(50, 60, 800, 800), zoom=0.8).
#' @param guides If set to TRUE the guides3d() function will be run with col="green" and default settings.
#' @param frame If set to TRUE the grid line structure will be plotted.
#' 
#' @param ... Further graphical parameters passed to (see \code{\link[rgl]{plot3d}}).
#' 
#' @exportMethod plot3d
#' @rdname plot3d-methods
# ' @aliases plot3d, plot3d-facelayer-method
setMethod(
	"plot3d",
	signature="facelayer",
	definition=function(x,type="f",frame=TRUE, guides=TRUE, defaultPar3d=TRUE, ...){
	
		# default par3d options
		if(defaultPar3d){
			rgl::par3d(windowRect = c(50, 60, 800, 800), zoom=0.8)
		}
			
		actGrid  <- get(x@grid)
		checkLinkedGrid(actGrid, x)
			
		#do not allow arguments to pass through!
		if(frame==TRUE){
			plot3d(actGrid, guides=guides, col="gray50")
		}else{
			plot3d(actGrid, type="n", guides=guides)
			
			#boundaries
			if(type=="l"){
				lines3d(x,...)
		
			}
		}
		if(type=="f"){
			faces3d(x,specular="black",...)
		}
		
		#add additional types of plotting to this method
		#no plotting
		if(type=="n"){
		}
		
	
	}
)
#' Methods of 3d face plotting.
#' 
#' This is a generic function used to plot the faces of a facelayer type object. 
#' 
#' The function is built on the openGL renderer of the R package \code{rgl}.
#'  
#' @param col graphical parameter indicating the colours of the faces. A single value is accepted for logical values. Multiple colors will be passed to grDevices::colorRampPalette(), to create palettes for heat maps in case of numeric values. The defaul plotting method in this case is the reversed grDevices::heat.colors (). In case of categorical data, random colors will be chosen.
#' @param breaks Numeric vector stating the breakpoints between the plotted levels. The argument is passed to the \code{\link[base]{cut}} function. 
#' @param inclusve (\code{logical}): If there are values beyond the limits of breaks, should these be represented in the plot (\code{TRUE}) or left out completely \code{FALSE}?
#' @param discrete Description.
#' @return The function does not return any value.
#'
#' @exportMethod faces3d
#' @rdname faces3d-methods
# ' @aliases faces3d, facelayer-method
setMethod(	
	f="faces3d",
	signature="facelayer",
	definition= function(x,col="heat",breaks=NULL, inclusive=TRUE, discrete=FALSE, ...){
		# extract the grid that needs to be plotted:
		actGrid  <- get(x@grid)
	#	checkLinkedGrid(actGrid, x)
		
		#check whether the  grid is actually updated
		if(sum(x@names%in%rownames(actGrid@faces))!=length(x)) 
		stop("The facenames in thelinked grid does not match the facelayer object.")
		
		#when the valuues are logical
		#FALSEs do not plot; NAs do not plot, TRUEs plot
		
		# defend 'breaks'
		if(!is.null(breaks)){
			if(!is.numeric(breaks)) stop("The 'breaks' argument has to be numeric.")
			if(length(breaks)<3) stop("You need to provide at least three values to the 'breaks' argument.")
		}


		# if the grid is numerical and it has only one value, make it logical
		if(class(x@values)%in%c("integer","double", "numeric")){
			if(length(unique(x@values[!is.na(x@values)]))==1){
				x@values<-as.logical(x@values)
			}
			
		}
		if(is.logical(x@values)){
			#just add NAs where the values are 0
			x@values[x@values==FALSE]<-NA
		}
		
		#if the number of values does not match the grid face no
		boolPresent1<-rep(T,nrow(actGrid@faces))
		if(length(x)!=nrow(actGrid@faces)){
			boolPresent1<-rownames(actGrid@faces)%in%x@names
			actGrid<-subset(actGrid, rownames(actGrid@faces)[boolPresent1])
		}
		
		# in case there are NAs, do a subsetting before going on
		# rgl does not understand col=NA as omission of plotting
		if(sum(is.na(x@values))>0){
			# select only the faces that are available
			boolPresent<-!is.na(x@values)
			#1. the values
			x@values<-x@values[boolPresent]
			#2. the names too
			x@names<- x@names[boolPresent]
			#3. number
			x@length <- sum(boolPresent)
			
		#	#do a pseudo subsetting!
		#	tempFacesLog<-rep(F, length(actGrid@skeleton$aF))
		#	#what should not be removed
		#	tempFacesLog[actGrid@skeleton$offsetF+actGrid@skeleton$uiF[which(names(actGrid@skeleton$uiF)%in%x@names)]] <- TRUE
		#	#remove everthing but that
		#	actGrid@skeleton$aF[!tempFacesLog]<- FALSE
		
		
			actGrid<-subset(actGrid, x@names) # the real subsetting
			
		}
		#when the values are logical
		if(class(x@values)=="logical"){
			#set default color value
			faces3d(actGrid,col=col,...)
		}
		
		# when  numerical values are added to the facelayer object, do a heatmap!
		if(class(x@values)%in%c("integer","double", "numeric")){
			
			
			# calculate the breaking vector
			if(is.null(breaks)){
				minimum <- min(x@values)
				maximum <- max(x@values)
				steps <- length(x)+1
				
				# the vector used to cut the plottted variable
				useBreaks <- seq(minimum, maximum,length.out=steps)
			}else{
				minimum <- min(breaks)
				maximum <- max(breaks)
				useBreaks <- breaks
			}

			# still need to include limitations
			bMax <- FALSE
			bMin <- FALSE
			if(inclusive){
				# values that are beyond the minimum boundary set by breaks
				beyondMax <- which(x@values>maximum)
				if(length(beyondMax)>1){
					x@values[beyondMax] <- maximum
					bMax <- TRUE
				}
				# values that are beyond the minimum boundary set by breaks
				beyondMin <- which(x@values<minimum)
				if(length(beyondMin)>1){
					x@values[beyondMin] <- minimum
					bMin <- TRUE
				}
			}


			#do a heatmap!
			#create a ramp, with a given number of colours
			#the color vector will control the heatmap
			if(length(col)==1){
				# predefined
				if(col=="heat"){
#					col<-c("red","orange","yellow", "white")
					cols <- rev(grDevices::heat.colors(length(useBreaks)-1))
				
					cols<-substring(cols, 1,7)
				}else{
					
					if(length(col)==1){
						stop("You specified only one color.")
					}
				
				}
			} else{
			#do a heatmap!
				ramp<-grDevices::colorRampPalette(col, bias=2, space="Lab")
				# produce as many colours as there are values
				cols <- ramp(length(useBreaks)-1)
			}

			# do the cutting
			alreadyCut <- base::cut(x@values, breaks=useBreaks, include.lowest=TRUE)

			# transfer the factor to indices
			trans2 <- as.numeric(alreadyCut)

			# this is the ui sequence	
			faceColors<-cols[trans2]
			
			if(class(actGrid)=="trigrid"){
				
				#in the inner sequence
				#create a source vector as if it was complete
					faceColors2<-rep(NA, length(actGrid@skeleton$uiF))
					names(faceColors2)<-paste("F", 1:length(faceColors2), sep="")
					faceColors2[names(x)]<-faceColors
				
				#order them
					faceColors3<-rep(NA, length(faceColors2))
					faceColors3[actGrid@skeleton$uiF]<-faceColors2
				
				#and get rid of the NAs
				faceColors3<-faceColors3[!is.na(faceColors3)]
			
				
			}
			if(class(actGrid)=="hexagrid"){
				
				tu <- as.numeric(t(actGrid@skeleton$uiF[names(x),]))
				
				empty<-rep(NA, nrow(actGrid@skeleton$f))
				
				fc<-rep(faceColors, each=12)
				
				empty[tu[!is.na(tu)]] <-fc[!is.na(tu)]
				
				noNA<-empty[as.logical(actGrid@skeleton$aSF)]
			
			###	
				# get the subfaces where there is information
			#	f<-as.data.frame(actGrid@skeleton$f[as.logical(actGrid@skeleton$aSF),1:3])
				
				#which outer faces do the subfaces belong?
				aas<- actGrid@skeleton$aSF[as.logical(actGrid@skeleton$aSF)]
				
				#create a vector fro all the total colors (as if the grid was full)
				totCol<-rep(NA,nrow(actGrid@skeleton$uiF))
				names(totCol) <- paste("F", 1:length(totCol), sep="")
				
				# insert the information
				totCol[names(x)]<-faceColors
				
				#reorder the colors to the subfaces
				faceColors3<-totCol[aas]
				
				
			#	
			#	temp<-cbind(f, newCol, stringsAsFactors=FALSE)
			#
			#	temp2<-temp[order(temp[,1]),]
			#	temp2<-unique(temp2)
				
				
				
			}
			faces3d(actGrid,col=rep(faceColors3, each=3),...)
			
			
		# numeric heatmap!			
			# increase the resolution when you plot the legend
			currentset<-rgl::par3d("windowRect")
			currentset2<-currentset
			currentset2[3]<-currentset[3]*1.5
			currentset2[4]<-currentset[4]*1.5
			
			
			# what should be passed to the heatmaplegend
			if(!discrete){
				tickLabs <- useBreaks
			}else{
				tickLabs <-  (useBreaks+useBreaks[2:(length(useBreaks)+1)])/2
				tickLabs <- tickLabs[!is.na(tickLabs)]
			}
				

			# double the resolution
			rgl::par3d(windowRect=currentset2)
			# plot the background
			rgl::bgplot3d(
				# turn off the graphical parameters warning bullshit
				suppressWarnings(
					heatMapLegend(cols,vals=tickLabs,...)
				)
			)
				
			rgl::par3d(windowRect=currentset)

	
		}
		
		#when all the values are colors
		#plot faces as 
		if(class(x@values)=="character" & sum(x@values%in%grDevices::colors())==x@length){
			faces3d(actGrid, col=x@values, plot="faces",...)
			
		}
		
		# when the values are text | they are not colors
		if(class(x@values)=="character" & !sum(x@values%in%grDevices::colors())==x@length){
			# state the labels in 3d on the face (using the centers of the faces)
			colorAll <- grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = TRUE)]
			active<-factor(x@values)
			if(length(levels(active))>length(colorAll)){
				cols<-sample(colorAll, length(levels(active)), replace=TRUE)
			}else{
				cols<-sample(colorAll, length(levels(active)), replace=FALSE)
			}
			
			faceColors<-cols[as.numeric(active)]
			
			if(class(actGrid)=="trigrid"){
				
				#in the inner sequence
				#create a source vector as if it was complete
					faceColors2<-rep(NA, length(actGrid@skeleton$uiF))
					names(faceColors2)<-paste("F", 1:length(faceColors2), sep="")
					faceColors2[names(x)]<-faceColors
				
				#order them
					faceColors3<-rep(NA, length(faceColors2))
					faceColors3[actGrid@skeleton$uiF]<-faceColors2
				
				#and get rid of the NAs
				faceColors3<-faceColors3[!is.na(faceColors3)]
			
				
			}
			if(class(actGrid)=="hexagrid"){
				
				tu <- as.numeric(t(actGrid@skeleton$uiF[names(x),]))
				
				empty<-rep(NA, nrow(actGrid@skeleton$f))
				
				fc<-rep(faceColors, each=12)
				
				empty[tu[!is.na(tu)]] <-fc[!is.na(tu)]
				
				noNA<-empty[as.logical(actGrid@skeleton$aSF)]
			
			###	
				# get the subfaces where there is information
			#	f<-as.data.frame(actGrid@skeleton$f[as.logical(actGrid@skeleton$aSF),1:3])
				
				#which outer faces do the subfaces belong?
				aas<- actGrid@skeleton$aSF[as.logical(actGrid@skeleton$aSF)]
				
				#create a vector fro all the total colors (as if the grid was full)
				totCol<-rep(NA,nrow(actGrid@skeleton$uiF))
				names(totCol) <- paste("F", 1:length(totCol), sep="")
				
				# insert the information
				totCol[names(x)]<-faceColors
				
				#reorder the colors to the subfaces
				faceColors3<-totCol[aas]
				
				
			#	
			#	temp<-cbind(f, newCol, stringsAsFactors=FALSE)
			#
			#	temp2<-temp[order(temp[,1]),]
			#	temp2<-unique(temp2)
				
				
				
			}
			faces3d(actGrid,col=rep(faceColors3, each=3),...)
			
			
		}
		
		# when the values are factors!
		if(class(x@values)=="factor"){
			# depending on the number of levels, more color palettes might be useful
			if(length(levels(factor(x@values))) <= 7){
				faces3d(actGrid, col=rep(as.numeric(x@values),each=3), ...)
			
			}
		}	
	#	legend3d()
	}
)


#' 2d plotting of a facelayer class object
#' This function will invoke the 2d plotting methods of a grid so data stored in a facelayer object can be displayed.
#'
#' The function passes arguments to the plot method of the SpatialPolygons class (€ref). In case a heatmap is plotted and the windows plotting device gets resized,
#' some misalignments can happen. If you want to use a differently sized window, use windows() to set the height and width before running the function.
#' @param x The facelayer object to be plotted.
#' @param frame Logical value, if TRUE the grid boundaries will be drawn with black.
#' @param col Character vector. Colors passed to a \code{\link[grDevices]{colorRamp}} in case of the facelayer contains logical values, a single value is required (defaults to red).
#' @param border Character value specifying the color of the borders of the cells.
#' @param alpha Character value of two digits for the fill colors, in hexadecimal value between 0 and 255.
#' @param breaks Numeric vector stating the breakpoints between the plotted levels. The argument is passed to the \code{\link[base]{cut}} function. 
#' @param legend (\code{logical}): Should the legend be plotted? 
#' @param inclusve (\code{logical}): If there are values beyond the limits of breaks, should these be represented in the plot (\code{TRUE}) or left out completely \code{FALSE}?
#' @param discrete \code{logical}: Do the heatmaps symbolize a discrete or a continuous variable? This argument only affects the legend of the heatmap. 
#' @rdname plot-methods
#' @exportMethod plot
setMethod(
	"plot",
	signature="facelayer",
	definition=function(x,projargs=NULL,col="heat",border=NA, alpha=NULL, frame=FALSE,legend=TRUE, breaks=NULL,beside=TRUE, inclusive=TRUE, discrete=FALSE,  ...){
		actGrid<-get(x@grid)
		checkLinkedGrid(actGrid, x)
		
		# defend 'breaks'
		if(!is.null(breaks)){
			if(!is.numeric(breaks)) stop("The 'breaks' argument has to be numeric.")
			if(length(breaks)<3) stop("You need to provide at least three values to the 'breaks' argument.")
		}

		# defend alpha
		if(!is.null(alpha)){
			if(length(alpha)>1) stop("Only one 'alpha' value is permitted.")
		#	if(alpha<=1 & alpha>=0) alpha
		}

		#if no @sp found
		if(suppressWarnings(is.na(actGrid@sp))){
			stop(paste("Slot @sp in the linked grid \'",x@grid, "\' is empty.", sep=""))
		}
		
		#transformation is necessary
		if(!is.null(projargs)){
		#	requireNamespace("rgdal")
		# need rgdal
			if(requireNamespace("rgdal", quietly = TRUE)){
				actGrid@sp<-sp::spTransform(actGrid@sp, projargs)
			} else{
				stop("The rgdal package is required to appropriately project this object. ")
			}
		}
		#check whether the  grid is actually updated
		if(sum(x@names%in%rownames(actGrid@faces))!=length(x)) 
		stop("The facenames in the linked grid does not match the facelayer object.")
		
		# if the grid is numerical and it has only one value, make it logical
		if(class(x@values)%in%c("integer","double", "numeric")){
			if(length(unique(x@values[!is.na(x@values)]))==1){
				x@values<-as.logical(x@values)
			}
			
		}
		#when the valuues are logical
		#FALSEs do not plot; NAs do not plot, TRUEs plot
		
		if(is.logical(x@values)){
			#just add NAs where the values are 0
			x@values[x@values==FALSE]<-NA
		}
		
		#if the number of values does not match the grid face no
		boolPresent1<-rep(T,nrow(actGrid@faces))
		if(length(x)!=nrow(actGrid@faces)){
			boolPresent1<-rownames(actGrid@faces)%in%x@names
		}
		actSp<-actGrid@sp[boolPresent1]
		
	
		#get rid of the NAs
		boolPresent<-rep(T,length(x))
		# in case there are NAs, do a subsetting before going on
		# rgl does not understand col=NA as omission of plotting
		if(sum(is.na(x@values))>0){
			# select only the faces that are available
			boolPresent<-!is.na(x@values) 
			#1. the values
			x@values<-x@values[boolPresent]
			#2. the names too
			x@names<- x@names[boolPresent]
			#3. number
			x@length <- sum(boolPresent)
		}
		actSp<-actSp[boolPresent]
		
		#when the values are logical
		if(class(x@values)=="logical"){
			#set default color value
			plot(actSp,col=col,border=border,...)
		}
		
		# when  numerical values are added to the facelayer object, do a heatmap!
		if(class(x@values)%in%c("integer","double", "numeric")){
			
			# calculate the breaking vector
			if(is.null(breaks)){
				minimum <- min(x@values)
				maximum <- max(x@values)
				steps <- length(x)+1
				
				# the vector used to cut the plottted variable
				useBreaks <- seq(minimum, maximum,length.out=steps)
			}else{
				minimum <- min(breaks)
				maximum <- max(breaks)
				useBreaks <- breaks
			}

			# still need to include limitations
			bMax <- FALSE
			bMin <- FALSE
			if(inclusive){
				# values that are beyond the minimum boundary set by breaks
				beyondMax <- which(x@values>maximum)
				if(length(beyondMax)>1){
					x@values[beyondMax] <- maximum
					bMax <- TRUE
				}
				# values that are beyond the minimum boundary set by breaks
				beyondMin <- which(x@values<minimum)
				if(length(beyondMin)>1){
					x@values[beyondMin] <- minimum
					bMin <- TRUE
				}
			}


			#do a heatmap!
			#create a ramp, with a given number of colours
			#the color vector will control the heatmap
			if(length(col)==1){
				# predefined
				if(col=="heat"){
#					col<-c("red","orange","yellow", "white")
					cols <- rev(grDevices::heat.colors(length(useBreaks)-1))
				
					cols<-substring(cols, 1,7)
				}else{
					
					if(length(col)==1){
						stop("You specified only one color.")
					}
				
				}
			} else{
			#do a heatmap!
				ramp<-grDevices::colorRampPalette(col, bias=2, space="Lab")
				# produce as many colours as there are values
				cols <- ramp(length(useBreaks)-1)
			}

			# do the cutting
			alreadyCut <- base::cut(x@values, breaks=useBreaks, include.lowest=TRUE)

			# transfer the factor to indices
			trans2 <- as.numeric(alreadyCut)

			# this is the ui sequence	
			faceColors<-cols[trans2]
			if(is.character(border)){
				if(length(unique(border))==1){
					if(substring(border[1], 1,1)=="#"){
						border=paste(border, alpha,sep="")
					}
				
				}
			}
			faceColors<-paste(faceColors, alpha, sep="")
			
			# set the new margins
			if(legend){
				graphics::par(mar=c(2,2,2,8))
			}
			# plot the sp object with the given argumetns
				# get rid of some of the arguments
				addArgs<-list(...)
				
				# arguments of the heatMapLegend()
				addArgs$tick.text<-NULL
				addArgs$ticks<-NULL
				addArgs$tick.cex<-NULL
				addArgs$barWidth<-NULL
				addArgs$barHeight<-NULL
				addArgs$tickLength<-NULL
				addArgs$xBot<-NULL
	
				
				firstArgs<-list(
					x=actSp,
					col=faceColors,
					border=border
				)
					
				plotArgs<-c(firstArgs, addArgs)
				
				do.call(plot, plotArgs)
			
			#the heatmap legend
			if(legend){
				#in case a heatmap is needed
				oldRef<-graphics::par()
				oldRef$cin<-NULL
				oldRef$cra<-NULL
				oldRef$cxy<-NULL
				oldRef$din<-NULL
				oldRef$page<-NULL
				oldRef$csi<-NULL
				
				graphics::par(usr=c(0,100,0,100))
				graphics::par(xpd=NA)
				graphics::par(mar=c(2,2,2,2))
				
				# additional argumetns to the heatmap, remove something
				addArgs<-list(...)
				addArgs$axes<-NULL
				addArgs$add<-NULL
				
				# what should be passed to the heatmaplegend
				if(!discrete){
					tickLabs <- useBreaks
				}else{
					tickLabs <-  (useBreaks+useBreaks[2:(length(useBreaks)+1)])/2
					tickLabs <- tickLabs[!is.na(tickLabs)]
				}
				

				firstArgs<-list(
					cols=cols,
					vals=tickLabs,
					add=TRUE,
					xLeft=101, 
					bounds=c(bMin, bMax)
				)
				
				# all the argumetns of the heatmap
				heatArgs<-c(firstArgs, addArgs)
				
				suppressWarnings(
					do.call(heatMapLegend, heatArgs)
				
				)
				graphics::par(oldRef)
			}
				
		}
		
		# when the values are text | they are not colors
		if(class(x@values)=="character" & !sum(x@values%in%grDevices::colors())==x@length){
			# state the labels in 3d on the face (using the centers of the faces)
			colorAll <- grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = TRUE)]
			active<-factor(x@values)
			if(length(levels(active))>length(colorAll)){
				cols<-sample(colorAll, length(levels(active)), replace=TRUE)
			}else{
				cols<-sample(colorAll, length(levels(active)), replace=FALSE)
			}
			
			faceColors<-cols[as.numeric(active)]
			if(is.character(border)){
				if(length(unique(border))==1){
					if(substring(border[1], 1,1)=="#"){
						border=paste(border, alpha,sep="")
					}
				
				}
			}
			faceColors<-paste(faceColors, alpha, sep="")
		
		}
		if(class(x@values)=="character" & sum(x@values%in%grDevices::colors())==x@length){
			faceColors<-paste(x@values, alpha, sep="")
		}
		
		if(class(x@values)=="character"){
		
			# plot the sp object with the given argumetns
				# get rid of some of the arguments
				addArgs<-list(...)
				
				# arguments of the heatMapLegend()
				addArgs$tick.text<-NULL
				addArgs$ticks<-NULL
				addArgs$tick.cex<-NULL
				addArgs$barWidth<-NULL
				addArgs$barHeight<-NULL
				addArgs$tickLength<-NULL
				addArgs$xBot<-NULL
	
				
				firstArgs<-list(
					x=actSp,
					col=faceColors,
					border=border
				)
					
				plotArgs<-c(firstArgs, addArgs)
				
				do.call(plot, plotArgs)
			
		}

		# when the col argument actually contains colors
		
		if(frame){
			
			plot(actSp, border="black", add=TRUE)
			
		}
	
	
	}

)

#' Create or instantiate an 'igraph' class graph based on a facelayer
#'
#' The function can be applied to a facelayer class object of logical values. The resulting graph will have the characteristics of the original grid (directed/undirected etc.).
#' @return The function returns an 'igraph' graph.
#' @exportMethod gridgraph
#' @rdname gridgraph-methods
#' @aliases gridgraph-facelayer-method
#' @exportMethod gridgraph
setMethod(
	f="gridgraph",
	signature="facelayer",
	definition=function(x){
		# the grid object of the facelayer
		actGrid <- get(x@grid)
		
		checkLinkedGrid(actGrid, x)
		
		# only the occupied cells should be part of the grid
		# or: where the logical values indicate absence
		if(is.logical(x@values)){
			x@values[x@values==FALSE] <- NA
		}
		
		# these will be the nodes of the graph
		nodes<- x@names[!is.na(x@values)]
		
		# subset the graph to the nodes which are indicated in the facelayer
		actGraph <- igraph::induced_subgraph(actGrid@graph, nodes)
		
		# the values of the facelayer should be also present
		if(!is.logical(x@values)){
			igraph::V(actGraph)$value <- x@values[!is.na(x@values)]
		}
		
		return(actGraph)

	}
)


#' Resampling a facelayer to a different resolution
#' 
#' This function will calculate values for a different resolution icosahedral grid from a facelayer.
#' 
#' The function applies different resampling algorithms. Currently there are only two implemented methods, one for upscaling and one for downscaling. The downscaling method "average" will tabluate all face centers from the high resolution grid that fall on a coarse resolution cell and average them. The upscaling method "ebaa" (edge breakpoint area approximation) will estimate the areas covered by the high resolution cells using the number of edge breakpoints.
#' 
#' @param x a \code{facelayer} class object.
#' 
#' @param y a \code{hexagrid} or \code{trigrid} object.
#' @param res Numeric value, indicating the precision of area estimation during the upscaling. In case the "ebaa" method is chosen, the variable indicate the number of breaking points on an edge.
#' 
#' @param method Character string stating the name of the algorithm used for resampling. 
#' 
#' @return A named numeric vector.
#' 
#' @examples
#' 	g <- trigrid(c(4,4))
#' 	fl <- facelayer(g)
#'		fl@values<-rnorm(length(fl))
#' 	h <- trigrid(4)
#'		res <- resample(fl, h)
#' 	fl2<-facelayer(h)
#'		fl2@values[] <- res
#'
#' @exportMethod resample
#' @rdname resample-methods
#' @aliases facelayer-trigrid-resample-method
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
				if(class(y)=="trigrid"){
					newGridF<-y@skeleton$f[y@skeleton$f[,4]==(length(y@tessellation)-1),]
					
					# c++ function will look up produce these points
					newPoints<-.Call(Cpp_icosa_ExpandEdgesByFacesTri_, y@skeleton$v, newGridF, y@center, res)
					newPoints[,4] <- newPoints[,4]+1
					# outer order
					fNames<-names(sort(y@skeleton$uiF))
					newNames <- fNames[newPoints[,4]]
				}
				if(class(y)=="hexagrid"){
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




### junk and examples

#defGrid<-trigrid(c(2,2))

#	# additional things
#	
#	
#	a<-facelayer(defGrid)
#	#b<-edgelayer(defGrid)
#	c<-pointlayer(defGrid)
#	
#	
#	values(a)<-sample(c(TRUE,FALSE),length(a), replace=TRUE)	
#	#
#	
#	resample(layerObj, newGrid, method)



