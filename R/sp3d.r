# this file includes method and class defintions for 3d plotting and coordinate extraction of the sp-family objects

# functions for 3d SP operations


##########################################################
#class definitions for the lines			
Line3d<-setClass(
	"Line3d",
	slot=c(
		coords = "matrix"
	)
	#coords should have three columns
)

Lines3d<-setClass(
	"Lines3d",
	slot=c(
		Lines="list",
		ID = "character"
	)
	#list should only contain
	#Line3d objects
)
	
SpatialLines3d<-setClass(
	"SpatialLines3d",
	slots=c(
		lines="list"
	)
	#lists should only consist of Lines3d objects
	
)


SpatialLines3dDataFrame<-setClass(
	"SpatialLines3dDataFrame",
	slots=c(
		data="data.frame",
		lines="list"
	)
	#lists should only consist of Lines3d objects
	
)

##########################################################
#class definitions for the polygons			
Polygon3d<-setClass(
	"Polygon3d",
	slot=c(
		coords = "matrix",
		ringDir ="integer",
		hole = "logical",
		area = "numeric"
	)
	#coords should have three columns
)

Polygons3d<-setClass(
	"Polygons3d",
	slot=c(
		Polygons="list",
		plotOrder="integer",
		ID = "character",
		area= "numeric"
	)
	#list should only contain
	#Line3d objects
)
	
SpatialPolygons3d<-setClass(
	"SpatialPolygons3d",
	slots=c(
		polygons="list",
		plotOrder="integer",
		proj4string="CRS"
	)
	#lists should only consist of Lines3d objects
	
)


SpatialPolygons3dDataFrame<-setClass(
	"SpatialPolygons3dDataFrame",
	slots=c(
		data="data.frame",
		polygons="list",
		plotOrder="integer",
		proj4string="CRS"
	)
	#lists should only consist of Lines3d objects
	
)

#########
#3d conversion:
# To3d function
#generic
setGeneric(
		name="To3d",
		def=function(spObj,...){
			standardGeneric("To3d")
		}
	
	)

#lines
#Line->Line3d
setMethod(
	"To3d",
	signature="Line",
	definition=function(spObj, radius=authRadius){
		coords<-as.matrix(PolToCar(spObj@coords, radius=radius))
		Line3d(coords=coords)
	}
)


#Lines->Lines3d
setMethod(
	"To3d",
	signature="Lines",
	definition=function(spObj, radius=authRadius){
		listLook<-spObj@Lines
		list3d<-lapply(listLook, To3d)
		
		Lines3d(Lines=list3d, ID=spObj@ID)
	}
)

#SpatialLines->SpatialLines3d
setMethod(
	"To3d",
	signature="SpatialLines",
	definition=function(spObj, radius=authRadius){
		listLook<-spObj@lines
		list3d<-lapply(listLook, To3d)
		
		SpatialLines3d(lines=list3d)
	}
)

#SpatialLinesDataFrame->SpatialLines3dDataFrame
	setMethod(
		"To3d",
		signature="SpatialLinesDataFrame",
		definition=function(spObj, radius=authRadius){
			listLook<-spObj@lines
			list3d<-lapply(listLook, To3d)
			
			SpatialLines3dDataFrame(data=spObj@data, lines=list3d)
		
		}
	)

######################################
#polygons
#Polygon->Polygon3d
setMethod(
	"To3d",
	signature="Polygon",
	definition=function(spObj, radius=authRadius){
		coords<-as.matrix(PolToCar(spObj@coords, radius=radius))
		Polygon3d(coords=coords, area=spObj@area, ringDir=spObj@ringDir, hole=spObj@hole)
	}
)

#qw<-zi[[1]]	
#qw3<-To3d(qw)	

#Polygons->Polygons3d
setMethod(
	"To3d",
	signature="Polygons",
	definition=function(spObj, radius=authRadius){
		listLook<-spObj@Polygons
		list3d<-lapply(listLook, To3d)
		
		Polygons3d(Polygons=list3d, plotOrder=spObj@plotOrder, ID=spObj@ID, area=spObj@area)
	}
)

#qa<-wow@polygons[[1]]
#qa3<-To3d(qa)
	

#SpatialPolygons->SpatialPolygons3d
setMethod(
	"To3d",
	signature="SpatialPolygons",
	definition=function(spObj, radius=authRadius){
		listLook<-spObj@polygons
		list3d<-lapply(listLook, To3d)
		
		SpatialPolygons3d(polygons=list3d, proj4string=spObj@proj4string,plotOrder=spObj@plotOrder)
	}
)
	
#wow3d<-To3d(wow)	
	

#SpatialPolygonsDataFrame->SpatialPolygons3dDataFrame
setMethod(
	"To3d",
	signature="SpatialPolygonsDataFrame",
	definition=function(spObj, radius=authRadius){
		listLook<-spObj@polygons
		list3d<-lapply(listLook, To3d)
		
		SpatialPolygons3dDataFrame(polygons=list3d, proj4string=spObj@proj4string,plotOrder=spObj@plotOrder)
	}
)
		
#wo3d<-To3d(wo)
	
	
# new methods for lines3d

# internal function to plot in 3d that should not be exported to the UI
setGeneric(
		name="lines3dInt",
		def=function(x,...){
			standardGeneric("lines3dInt")
		}
	
	)

#3d Line plotting and coordinate extraction
	#1A. Line3d
	setMethod(
		"lines3dInt",
		signature="Line3d",
		definition=function(x,...){
			rgl::lines3d(x@coords,...)
		}
	)
	

#' 3d plotting method of a Line class object
#' 
#' The function draws the segments of a Line clas object in 3d.
#' @rdname lines3d-methods
#' @aliases Line-lines3d-method
#' @exportMethod lines3d
setMethod(
	"lines3d",
	signature="Line",
	definition=function(x,...){
		y<-To3d(x)
		lines3dInt(y,...)
	}
)
	
	
	
	#2A. Lines3d
	setMethod(
		"lines3dInt",
		signature="Lines3d",
		definition=function(x,plot=TRUE,...){
			listLook<-x@Lines
			
			temp<-lapply(listLook, function(lineObj){
				rbind(lineObj@coords, rep(NA, 3))
			})
			#
			finMat<-matrix(ncol=3, nrow=0)
			for(i in 1:length(temp)){
				finMat<-rbind(finMat,temp[[i]])
			}
			if(plot==TRUE){
				lines3d(finMat,...)
			}else{
				return(finMat)
			}
			
		}
	)
	
	
#' 3d  plotting method of a Lines class object
#' 
#' The function draws the segments of a Line clas object in 3d.
#' @param ... arguments passed to rgl::lines3d()
#' @rdname lines3d-methods
#' @aliases Lines-lines3d-method
#' @exportMethod lines3d
setMethod(
	"lines3d",
	signature="Lines",
	definition=function(x,...){
		y<-To3d(x)
		lines3dInt(y,...)	
	}
)
	
	#3A. SpatialLines3d
	setMethod(
		"lines3dInt",
		signature="SpatialLines3d",
		definition=function(x,plot=TRUE,...){
			listLook<-x@lines
			
			temp<-lapply(listLook, function(lineObj){
				lines3dInt(lineObj,plot=FALSE)
			})
			
			finMat<-matrix(ncol=3, nrow=0)
			
			for(i in 1:length(listLook)){
				finMat<-rbind(finMat,temp[[i]])
			}
			
			if(plot==FALSE){
				lines3d(finMat,...)
			}else{
				return(finMat)
			}
		}
	)
	
	#3B. SpatialLines
	#' lines3d method for the SpatialLines class
	#' @rdname lines3d-methods
	#' @param x the SpatialLines object
	#' @param y unused argument of the lines3d() generic.
	#' @param z unused argument of the lines3d() generic.
	#' @aliases lines3d, SpatialLines-method
	#' @exportMethod lines3d
		setMethod(
			"lines3d",
			signature="SpatialLines",
			definition=function(x,...){
				y<-To3d(x)
				lines3dInt(y,...)	
			}
		)
	
	#4A. SpatialLines3dDataFrame
		
	setMethod(
		"lines3dInt",
		signature="SpatialLines3dDataFrame",
		definition=function(x,plot=TRUE, ...){
			listLook<-x@lines
			
			temp<-lapply(listLook, function(lineObj){
				lines3dInt(lineObj,plot=FALSE)
			})
			
			finMat<-matrix(ncol=3, nrow=0)
			
			for(i in 1:length(listLook)){
				finMat<-rbind(finMat,temp[[i]])
			}
			if(plot==TRUE){
				lines3d(finMat,...)
			}else{
				return(finMat)
			}
		
			
		}
	)
	
	#4B. SpatialLinesDataFrame
	#' lines3d method for the SpatialLinesDataFrame
	#' @rdname lines3d-methods
	#' @aliases lines3d-SpatialLinesDataFrame-method
	#' @exportMethod lines3d
		setMethod(
			"lines3d",
			signature="SpatialLinesDataFrame",
			definition=function(x,...){
				y<-To3d(x)
				lines3dInt(y,...)	
			}
		)
		
	
#3d Line plotting and coordinate extraction (Polygons)	
	#1A. Polygon3d 
	setMethod(
		"lines3dInt",
		signature="Polygon3d",
		definition=function(x,...){
			lines3d(x@coords,...)
		}
	)
	
	#1B. Polygon
	#' lines3d method for the Polygon class
	#' @rdname lines3d-methods
	#' @aliases lines3d-Polygon-method
	#' @exportMethod lines3d
	setMethod(
		"lines3d",
		signature="Polygon",
		definition=function(x,...){
			y<-To3d(x)
			lines3dInt(y,...)
		}
	)
	
	
	
	#2A. Polygons3d
	setMethod(
		"lines3dInt",
		signature="Polygons3d",
		definition=function(x,plot=TRUE,...){
			listLook<-x@Polygons
			
			temp<-lapply(listLook, function(lineObj){
				rbind(lineObj@coords, rep(NA, 3))
			})
			#
			finMat<-matrix(ncol=3, nrow=0)
			for(i in 1:length(temp)){
				finMat<-rbind(finMat,temp[[i]])
			}
			if(plot==TRUE){
				lines3d(finMat,...)
			}else{
				return(finMat)
			}
			
		}
	)
	
	#2B. Polygons
	#' lines3d method for the Polygon sclass
	#' @rdname lines3d-methods
	#' @aliases lines3d-Polygons-method
	#' @exportMethod lines3d
		setMethod(
			"lines3d",
			signature="Polygons",
			definition=function(x,...){
				y<-To3d(x)
				lines3dInt(y,...)	
			}
		)
	
	#3A. SpatialPolygons3d
	setMethod(
		"lines3dInt",
		signature="SpatialPolygons3d",
		definition=function(x,plot=TRUE,...){
			listLook<-x@polygons
			
			temp<-lapply(listLook, function(lineObj){
				lines3dInt(lineObj,plot=FALSE)
			})
			
			finMat<-matrix(ncol=3, nrow=0)
			
			for(i in 1:length(listLook)){
				finMat<-rbind(finMat,temp[[i]])
			}
			
			if(plot==TRUE){
				lines3d(finMat,...)
			}else{
				return(finMat)
			}
		}
	)
	
	#3B. SpatialPolygons
	#' lines3d method for the SpatialPolygons sclass
	#' @rdname lines3d-methods
	#' @aliases lines3d-SpatialPolygons-method
	#' @exportMethod lines3d
		setMethod(
			"lines3d",
			signature="SpatialPolygons",
			definition=function(x,...){
				y<-To3d(x)
				lines3dInt(y,...)	
			}
		)
	
	#4A. SpatialPolygons3dDataFrame
	setMethod(
		"lines3dInt",
		signature="SpatialPolygons3dDataFrame",
		definition=function(x,plot=TRUE, ...){
			listLook<-x@polygons
			
			temp<-lapply(listLook, function(lineObj){
				lines3dInt(lineObj,plot=FALSE)
			})
			
			finMat<-matrix(ncol=3, nrow=0)
			
			for(i in 1:length(listLook)){
				finMat<-rbind(finMat,temp[[i]])
			}
			if(plot==TRUE){
				lines3d(finMat,...)
			}else{
				return(finMat)
			}
		
			
		}
	)
	
	#4B. SpatialPolygonsDataFrame
	#' lines3d method for the SpatialPolygonsDataFrame sclass
	#' @rdname lines3d-methods
	#' @aliases lines3d-SpatialPolygonsDataFrame-method
	#' @exportMethod lines3d
		setMethod(
			"lines3d",
			signature="SpatialPolygonsDataFrame",
			definition=function(x,...){
				y<-To3d(x)
				lines3dInt(y,...)	
			}
		)
	


#function to extract the coordinates using the lines3d function
setMethod(
	"coordinates",
	signature="SpatialLines3d",
	definition=function(obj, gaps=FALSE){
		objCoor<-lines3d(obj, plot=FALSE)
		if(gaps==TRUE){
			return(objCoor)
		}else{
			return(objCoor[!is.na(objCoor[,1]),])
		}
	}
)

setMethod(
	"coordinates",
	signature="SpatialLines3dDataFrame",
	definition=function(obj, gaps=FALSE){
		objCoor<-lines3d(obj, plot=FALSE)
		if(gaps==TRUE){
			return(objCoor)
		}else{
			return(objCoor[!is.na(objCoor[,1]),])
		}
	}
)

setMethod(
	"coordinates",
	signature="SpatialPolygons3d",
	definition=function(obj, gaps=FALSE){
		objCoor<-lines3dInt(obj, plot=FALSE)
		if(gaps==TRUE){
			return(objCoor)
		}else{
			return(objCoor[!is.na(objCoor[,1]),])
		}
	}
)

setMethod(
	"coordinates",
	signature="SpatialPolygons3dDataFrame",
	definition=function(obj, gaps=FALSE){
		objCoor<-lines3dInt(obj, plot=FALSE)
		if(gaps==TRUE){
			return(objCoor)
		}else{
			return(objCoor[!is.na(objCoor[,1]),])
		}
	}
)


# methods for linear interpolation of spherical
setGeneric(
	name="linIntCoords",
	def=function(obj,res,...){
		standardGeneric("linIntCoords")
	}
	
)

#Line
setMethod(
	"linIntCoords",
	signature="Line",
	definition=function(obj, res){
		temp<-obj@coords[nrow(obj@coords),]
		obj@coords<-.Call(Cpp_icosa_Refine2d_, obj@coords, res)
		obj@coords<-rbind(obj@coords,temp)
		rownames(obj@coords)<-NULL
		return(obj)
	}
)

#Polygon
setMethod(
	"linIntCoords",
	signature="Polygon",
	definition=function(obj, res){
		temp<-obj@coords[nrow(obj@coords),]
		obj@coords<-.Call(Cpp_icosa_Refine2d_, obj@coords, res)
		obj@coords<-rbind(obj@coords,temp)
		rownames(obj@coords)<-NULL
		
		return(obj)
	}
)

#Lines
setMethod(
	"linIntCoords",
	signature="Lines",
	definition=function(obj, res){
		lookUpList<-obj@Lines
		newList<-lapply(lookUpList,linIntCoords, res=res)
		obj@Lines <-newList
		return(obj)
	}
)

#SpatialLines
setMethod(
	"linIntCoords",
	signature="SpatialLines",
	definition=function(obj, res){
		lookUpList<-obj@lines
		newList<-lapply(lookUpList,linIntCoords, res=res)
		obj@lines <-newList
		return(obj)
	}
)
#Polygons
setMethod(
	"linIntCoords",
	signature="Polygons",
	definition=function(obj, res){
		lookUpList<-obj@Polygons
		newList<-lapply(lookUpList,linIntCoords, res=res)
		obj@Polygons <-newList
		return(obj)
	}
)
#SpatialPolygons
setMethod(
	"linIntCoords",
	signature="SpatialPolygons",
	definition=function(obj, res){
		lookUpList<-obj@polygons
		newList<-lapply(lookUpList,linIntCoords, res=res)
		obj@polygons <-newList
		return(obj)
	}
)

#SpatialPolygonsDataFrame
setMethod(
	"linIntCoords",
	signature="SpatialPolygonsDataFrame",
	definition=function(obj, res){
		lookUpList<-obj@polygons
		newList<-lapply(lookUpList,linIntCoords, res=res)
		obj@polygons <-newList
		return(obj)
	}
)

