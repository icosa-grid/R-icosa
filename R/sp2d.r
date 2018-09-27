## new script for the 2d plotting

dateLineBreak<-function(oneLine){
	#where is the break- if there is any?
	endIndex<-which(abs(diff(oneLine[,1]))>350)
	if(length(endIndex)>0){
		#one line cannot be broken more than once
		oneLineSeg1<-Line(oneLine[1:endIndex,,drop=FALSE])
		oneLineSeg2<-Line(oneLine[(endIndex+1):nrow(oneLine),,drop=FALSE])
		
		#add the broken lines to the lines of the face
		faceLines <- c(list(oneLineSeg1), list(oneLineSeg2))
	
	#if there is no break, do everything normally
	}else{
		#then to Line object
		oneLine <- sp::Line(oneLine)
		
		#add Line to faceLines list
		faceLines<-list(oneLine)
	
	}
	
}
		
			
# do something with the 90/-90latitude data - to avoid long0-lat90 every time
correct90lat<-function(faceMat){
	if(90%in%abs(faceMat[,2])){
		meaning<- which(abs(faceMat[,2])==90)
		for(u in 1:length(meaning)){
			#average out the one before or after
			#take longitudes before to avoid "subscript out of bounds!"
			longs<-faceMat[,1]
			neighbours<-longs[c(meaning[u]-1,meaning[u]+1)]
			faceMat[meaning[u],1]<- mean(neighbours, na.rm=TRUE)
			
			#in case there are no nas
			# special case of the icosahedron
			if(sum(is.na(neighbours[1:2]))==0){
				if(sign(neighbours[1])!=sign(neighbours[2])){
					faceMat[meaning[u],1]<-neighbours[1]
				}
			
			}
			
		}
	}
	
	return(faceMat)

}
	

oneFace<-function(faceMat, nadirFace, zenithFace,faceID){
	#divide the cells that cross the dateline, if there is a difference in long values bigger than 300
		boolDiv<-sign(faceMat[,1])==1 & sum(abs(diff(faceMat[,1]))>300)
		check<-NULL
	#if there is only one deviating sign in the longitude data - assign 180 with the 
		latSign<-sign(faceMat[,1])
		if(sum(latSign==-1)==1){
			#and fairly close to 180
			if(abs(abs(faceMat[latSign==-1,1])-180)<3){
				faceMat[latSign==-1,1]<--180
			}
		}
		if(sum(latSign==1)==1){
			if(abs(abs(faceMat[latSign==1,1])-180)<3){
				faceMat[latSign==1,1]<-180
			}
		}
		
	#if the face is polar| given that is not a vertex
		if(!is.na(nadirFace)){
			boolNad<-nadirFace==faceID
		}else{
			boolNad <- FALSE
		}
		
		if(!is.na(zenithFace)){
			boolZen<-zenithFace==faceID
		}else{
			boolZen <- FALSE
		}
		
		#in case the selected face is the zenith
		if(boolZen){
			cutInd<-which(abs(diff(faceMat[,1]))>320)
			signChange<-sign(diff(faceMat[,1])[abs(diff(faceMat[,1]))>320])
			
			# filter cases when there is a rounding error
			if(length(signChange)>0){
				#when positive			
				if(signChange>0){
					insertMat<-matrix(c(-180,90,180,90),ncol=2, byrow=TRUE)
				}else{
					insertMat<-matrix(c(180,90,-180,90),ncol=2, byrow=TRUE)
				}
				faceMat<-rbind(faceMat[1:cutInd,],
					insertMat,
					faceMat[(cutInd+1):nrow(faceMat),]
				)
			}else{
				boolZen <- FALSE
			}
		}
		
		if(boolNad){
			cutInd<-which(abs(diff(faceMat[,1]))>320)
			signChange<-sign(diff(faceMat[,1])[abs(diff(faceMat[,1]))>320])
			# filter cases when there is a rounding error
			if(length(signChange)>0){	
				#when positive
				if(signChange>0){
					insertMat<-matrix(c(-180,-90,180,-90),ncol=2, byrow=TRUE)
				}else{
					insertMat<-matrix(c(180,-90,-180,-90),ncol=2, byrow=TRUE)
				}
				faceMat<-rbind(faceMat[1:cutInd,],
					insertMat,
					faceMat[(cutInd+1):nrow(faceMat),]
				)
			}else{
				boolNad <- FALSE
			}
		}
	
		
		#if there is a division, and the faces are not polar
		if(sum(boolDiv)>0 & sum(boolDiv)<nrow(faceMat) & !boolZen & !boolNad){
			cellPart1<-faceMat[boolDiv,,drop=FALSE]
			cellPart2<-faceMat[!boolDiv,,drop=FALSE]
			
			#if the divided cells are at 90 deg. lat (vertex on the pole)
			if(90%in%abs(faceMat[,2])){
				#add the +-180 longitude 90latitude where it is appropriate
				#1. first part of the  cell 
				chk<-abs(cellPart1[,2])
				if(which(chk==90)==1){
					#in case the first row is present
					cellPart1<-rbind(c(sign(cellPart1[1,1])*180, cellPart1[1,2]), cellPart1)
				}else{
					#in case it is the last!
					if(which(chk==90)==nrow(cellPart1)){
						#put it afterwards
						cellPart1<-rbind(cellPart1, c(sign(cellPart1[which(abs(cellPart1[,2])==90),1])*180, cellPart1[which(abs(cellPart1[,2])==90),2]))
					}else{
						# put the new value after this
						cellPart1a<-cellPart1[1:which(chk==90),]
						cellPart1b<-rbind(
							c(sign(cellPart1[which(abs(cellPart1[,2])==90),1])*180, cellPart1[which(abs(cellPart1[,2])==90),2]),
							cellPart1[(which(chk==90)+1):nrow(cellPart1),]
							)
						cellPart1<-rbind(cellPart1a, cellPart1b)
					}
				}
				
				#2. second part of the  cell 
				chk <- abs(cellPart2[,2])
				# for the icosahedron
				if(!90%in%chk) chk<-c(90, chk)
				if(which(chk==90)==1){
					#in case the first row is present
					cellPart2<-rbind(c(sign(cellPart2[1,1])*180, cellPart2[1,2]), cellPart2)
				}else{
					#put it afterwards
					cellPart2<-rbind(cellPart2, c(sign(cellPart2[which(abs(cellPart2[,2])==90),1])*180, cellPart2[which(abs(cellPart2[,2])==90),2]))
				}
				
			}
			
			#if either one of these has only one row and it's longitude is 180
			if((nrow(cellPart1)==1 & 0.5>abs(abs(cellPart1[1,1])-180)) | (nrow(cellPart2)==1 & 0.5>abs(abs(cellPart2[1,1])-180))){
				# aparent divison, but no true divison is required
				# turn the sign, and divide not!
				faceMat[abs(faceMat[,1])==180,1] <- -faceMat[abs(faceMat[,1])==180,1]
				cell<-sp::Polygon(faceMat)
				oneFace<-list(cell)
			#	check<-c(check, i)
			
			}else{
			#divide regularly
				cellPart1<-sp::Polygon(cellPart1)
				cellPart2<-sp::Polygon(cellPart2)
				oneFace<-list(cellPart1, cellPart2)
			}
		} else{
		# if not 
			cell<-sp::Polygon(faceMat)
			oneFace<-list(cell)
		}
	return(oneFace)
}	
#' SpatialLines class object from an icosahedral grid
#'
#' @name SpLines
#'
#' @param gridObj Either a \code{trigrid} or a \code{hexagrid} class object. 
#' @param ... specific details of the new SpatialLines object. 
#' @rdname SpLines-methods
#' 
#' @exportMethod SpLines
setGeneric(
	name="SpLines",
	def=function(gridObj,...){
		standardGeneric("SpLines")
	}

)

#' SpatialLines class object from an icosahedral grid
#'
#' The function will create an approporiate 2d render of the grid in the SpatialLines object format defined in the package 'sp'. 
#'
#' @param dateLine Specifies that NAs should be introduced at the dateline to break the boundaries of the faces. 
#' Can be switched off by setting it to \code{FALSE}.	
#' 
#' @param res Integer value, specifies the number of points (resolution) to be inserted between two vertices.
#' @rdname SpLines-methods
#' @aliases SpLines-trigrid-method
#' @return an object of class SpatialLines.
#' @exportMethod SpLines
setMethod(
	"SpLines",
	signature="trigrid",
	definition=function(gridObj, dateLine="break", res=15){
		# center back to origin if not there already
		if(gridObj@center[1]!=0 | gridObj@center[2]!=0 | gridObj@center[3]!=0){
			gridObj<-translate(gridObj,-gridObj@center)
		}
		
		#extend the faces
		v<-gridObj@skeleton$v[as.logical(gridObj@skeleton$aV),]
		f<-gridObj@skeleton$f[as.logical(gridObj@skeleton$aF),1:3]
		
	#	res<-30
		
		#extend to make a matrix
		temp<- .Call(Cpp_icosa_ExpandBoundariesToCols_, f, v, res, gridObj@center,0)
		
		#reorder to the outer representation
		temp2<-temp[,gridObj@skeleton$uiF]
		
		allNames<-paste("F", gridObj@skeleton$aF[as.logical(gridObj@skeleton$aF)], sep="")
		
		#make a data frame from the matrix
		temp2<-data.frame(temp2)
		
		if(dateLine==FALSE){
			# for the non-breaking method
			finalList<-lapply(temp2, function(x){
				#3 lines
				l<-(length(x)-4)/9
				
				mat1<-cbind(x[(0*l+1):(1*l)],x[(1*l+1):(2*l)],x[(2*l+1):(3*l)])
				mat2<-cbind(x[(3*l+1):(4*l)],x[(4*l+1):(5*l)],x[(5*l+1):(6*l)])
				mat3<-cbind(x[(6*l+1):(7*l)],x[(7*l+1):(8*l)],x[(8*l+1):(9*l)])
			
				mat1<-CarToPol(mat1,norad=TRUE, origin=gridObj@center)
				mat2<-CarToPol(mat2,norad=TRUE, origin=gridObj@center)
				mat3<-CarToPol(mat3,norad=TRUE, origin=gridObj@center)
				
				line1<-sp::Line(mat1)
				line2<-sp::Line(mat2)
				line3<-sp::Line(mat3)
				sp::Lines(list(line1,line2,line3), ID=allNames[x[length(x)]+1])
			})
		}
		if(dateLine=="break"){
			# the breaking method
			finalList<-lapply(temp2, function(x){
				#3 lines
				l<-(length(x)-4)/9
				
				mat1<-cbind(x[(0*l+1):(1*l)],x[(1*l+1):(2*l)],x[(2*l+1):(3*l)])
				mat2<-cbind(x[(3*l+1):(4*l)],x[(4*l+1):(5*l)],x[(5*l+1):(6*l)])
				mat3<-cbind(x[(6*l+1):(7*l)],x[(7*l+1):(8*l)],x[(8*l+1):(9*l)])
				
				mat1<-CarToPol(mat1,norad=TRUE, origin=gridObj@center)
				faceList1<-dateLineBreak(mat1)
				mat2<-CarToPol(mat2,norad=TRUE, origin=gridObj@center)
				faceList2<-dateLineBreak(mat2)
				mat3<-CarToPol(mat3,norad=TRUE, origin=gridObj@center)
				faceList3<-dateLineBreak(mat3)
				
				sp::Lines(c(faceList1,faceList2,faceList3), ID=allNames[x[length(x)]+1])
				
			})
		}
		endObj<-sp::SpatialLines(finalList, proj4string=CRS("+proj=longlat +a=6371007 +b=6371007"))
	}
)
	

#' Spatial polygons from an icosahedral grid
#'
#' The function will create a SpatialPolygons class 2d representation of the icosahedral grid.
#'		
#' @name SpPolygons
#		
#' @rdname SpPolygons-methods
#' @param gridObj an icosahedral grid.
#' @param res Integer value, the number of points inserted between two vertices.
#' 
#' @exportMethod SpPolygons
setGeneric(
	name="SpPolygons",
	def=function(gridObj,res){
		standardGeneric("SpPolygons")
	}

)

#' Spatial polygons from a triangular grid 
#'
#' The function will create a SpatialPolygons class 2d representation of trigrid class object.
#'		
#' @rdname SpPolygons-methods
#' 
#' @aliases SpPolygons-trigrid-method
#' @exportMethod SpPolygons
setMethod(
	"SpPolygons",
	signature="trigrid",
	definition=function(gridObj, res=50){
		# center back to origin if not there already
		if(gridObj@center[1]!=0 | gridObj@center[2]!=0 | gridObj@center[3]!=0){
			gridObj<-translate(gridObj,-gridObj@center)
		}
		
		zenith <- matrix(c(0,0, gridObj@r), ncol=3, nrow=1)
		nadir <- matrix(c(0,0, -gridObj@r), ncol=3, nrow=1)
		zenithFace <- locate(gridObj,zenith)
		nadirFace <- locate(gridObj,nadir)
		
		# override these if the x86 doesn't find the vertices at the poles
	#	vertexCoord<-vertices(gridObj, output="polar")
	#	if(90%in%vertexCoord[,2]){
	#		zenithFace<-NA
	#	}
	#	
	#	if(-90%in%vertexCoord[,2]){
	#		nadirFace<-NA
	#	}
		
		
	#extend the faces
		v<-gridObj@skeleton$v
		f<-gridObj@skeleton$f[as.logical(gridObj@skeleton$aF),1:3]
		
	#	res<-30
		# there are no pentagons here :)
		pent<-0
		
		#extend to make a matrix		
		temp<- .Call(Cpp_icosa_ExpandBoundariesToCols_, f, v, res, gridObj@center, pent)
		
		#reorder to the outer representation
	#	temp2<-temp[,gridObj@skeleton$uiF]
		put<-gridObj@skeleton$aF[as.logical(gridObj@skeleton$aF)]
		
		temp2<-matrix(NA, nrow=nrow(temp), ncol=max(put))
		
		temp2[,put]<-temp
		
		subLog<-!is.na(apply(temp2,2, sum))
		temp2<-temp2[,subLog]
		
	#	allNames<-paste("F", gridObj@skeleton$aF[as.logical(gridObj@skeleton$aF)], sep="")
		allNames<-paste("F", put, sep="")
		
		
		#make a data frame from the matrix
		temp2<-data.frame(temp2)
		
		finalList<-lapply(temp2, function(x){
	#	for(i in 1:length(temp2)){
	#		x<-temp2[[i]]
			#3 lines
			faceID<-allNames[x[length(x)]+1]
			
			l<-(length(x)-4)/9
			
			mat<-cbind(
				c(x[(0*l+1):(1*l)],x[(3*l+1):(4*l)],x[(6*l+1):(7*l)], x[9*l+1]),
				c(x[(1*l+1):(2*l)],x[(4*l+1):(5*l)],x[(7*l+1):(8*l)], x[9*l+2]),
				c(x[(2*l+1):(3*l)],x[(5*l+1):(6*l)],x[(8*l+1):(9*l)], x[9*l+3])
			)

			faceMat<-CarToPol(mat,norad=TRUE, origin=gridObj@center)
			faceMat<-correct90lat(faceMat)
			
			# do the dateline corrections and some 
			ofList<-oneFace(faceMat,nadirFace, zenithFace, faceID)
			
			for(j in 1:length(ofList)){
				if(ofList[[j]]@hole){
					ofList[[j]]@hole<-FALSE
					ofList[[j]]@ringDir<-as.integer(1)
					ofList[[j]]@coords<-ofList[[j]]@coords[nrow(ofList[[j]]@coords):1,]
				}
			}
		
			of<-sp::Polygons(ofList, ID=faceID)
#			return(of)
	#	}
		
		})
#		plot(NULL, NULL, xlim=c(-180,180), ylim=c(-90,90))
#		for(i in 1:length(finalList)){
#	#	for(i in 430:458){
#			tsp<-SpatialPolygons(finalList[i])
#			plot(tsp, add=TRUE)
#			Sys.sleep(0.1)
#		}
		
		endObj<-sp::SpatialPolygons(finalList, proj4string=gridObj@proj4string)
		
		return(endObj)
	}
)

#' Spatial polygons from a penta-hexagonal grid
#'
#' The function will create a SpatialPolygons class 2d representation of the hexagrid class object
#'		
#' @rdname SpPolygons-methods
#' 
#' @aliases SpPolygons-hexagrid-method
#' @exportMethod SpPolygons
setMethod(
	"SpPolygons",
	signature="hexagrid",
	definition=function(gridObj, res=50){
		# center back to origin if not there already
		if(gridObj@center[1]!=0 | gridObj@center[2]!=0 | gridObj@center[3]!=0){
			gridObj<-translate(gridObj,-gridObj@center)
		}
		
		zenith <- PolToCar(matrix(c(0,90), ncol=2,nrow=1), radius=gridObj@r, origin=gridObj@center)
		nadir <- PolToCar(matrix(c(0,-90), ncol=2,nrow=1), radius=gridObj@r, origin=gridObj@center)
		zenithFace <- locate(gridObj,zenith)
		nadirFace <- locate(gridObj,nadir)
		
	#extend the faces
		v<-gridObj@skeleton$v
		f<-gridObj@skeleton$vF[as.logical(gridObj@skeleton$aF),]
		
		# pentagons
		pent<-sum(is.na(f[,6]))
		
	#	res<-30
		
		#extend to make a matrix
		temp<- .Call(Cpp_icosa_ExpandBoundariesToCols_, f, v, res, gridObj@center, pent)
		
		#reorder to the outer representation
		put<-gridObj@skeleton$aF[as.logical(gridObj@skeleton$aF)]
		
		temp2<-matrix(NA, nrow=nrow(temp), ncol=max(put))
		
		temp2[,put]<-temp
		
		subLog<-!is.na(apply(temp2,2, sum))
		temp2<-temp2[,subLog]
		
	#	allNames<-paste("F", gridObj@skeleton$aF[as.logical(gridObj@skeleton$aF)], sep="")
		allNames<-paste("F", put, sep="")
	
		#make a data frame from the matrix
		temp2<-data.frame(temp2)
		
		finalList<-lapply(temp2,function(x){
		
	#	finalList<-list()
	#	for(i in 1:length(temp2)){
	#		x<-temp2[[i]]
			#6 lines
			faceID<-allNames[x[length(x)]+1]
			
			l<-(length(x)-4)/(6*3)
			
			# in case of the tessellation=1 object, only pentagons are present!
			if(prod(gridObj@tessellation)==1){
				l<-(length(x)-4)/(5*3)
			}
			
			if((x[length(x)]+1)<(pent+1)){
			#pentagon case
				#parts are lines
				mat<-cbind(
					# x coordinates
					c(
						x[(0*l+1):(1*l)],
						x[(3*l+1):(4*l)],
						x[(6*l+1):(7*l)],
						x[(9*l+1):(10*l)],
						x[(12*l+1):(13*l)]
					),
					# y coordinates
					c(
						x[(1*l+1):(2*l)],
						x[(4*l+1):(5*l)],
						x[(7*l+1):(8*l)],
						x[(10*l+1):(11*l)],
						x[(13*l+1):(14*l)]
					),
					# z coordinates
					c(
						x[(2*l+1):(3*l)],
						x[(5*l+1):(6*l)],
						x[(8*l+1):(9*l)],
						x[(11*l+1):(12*l)],
						x[(14*l+1):(15*l)]
					)
				)
				
			}else{
			# hexagon case
				#parts are lines
				mat<-cbind(
					# x coordinates
					c(
						x[(0*l+1):(1*l)],
						x[(3*l+1):(4*l)],
						x[(6*l+1):(7*l)],
						x[(9*l+1):(10*l)],
						x[(12*l+1):(13*l)],
						x[(15*l+1):(16*l)]
					),
					# y coordinates
					c(
						x[(1*l+1):(2*l)],
						x[(4*l+1):(5*l)],
						x[(7*l+1):(8*l)],
						x[(10*l+1):(11*l)],
						x[(13*l+1):(14*l)],
						x[(16*l+1):(17*l)]
					),
					# z coordinates
					c(
						x[(2*l+1):(3*l)],
						x[(5*l+1):(6*l)],
						x[(8*l+1):(9*l)],
						x[(11*l+1):(12*l)],
						x[(14*l+1):(15*l)],
						x[(17*l+1):(18*l)])
				)
			}
			
					
			faceMat<-CarToPol(mat,norad=TRUE, origin=gridObj@center)
			faceMat<-correct90lat(faceMat)
			
			# do the dateline corrections and some 
			ofList<-oneFace(faceMat,nadirFace, zenithFace, faceID)
			
			for(j in 1:length(ofList)){
				if(ofList[[j]]@hole){
					ofList[[j]]@hole<-FALSE
					ofList[[j]]@ringDir<-as.integer(1)
					ofList[[j]]@coords<-ofList[[j]]@coords[nrow(ofList[[j]]@coords):1,]
				}
			}
		
			of<-sp::Polygons(ofList, ID=faceID)
	#		finalList<-c(finalList, list(of))
			return(of)
	#	}
		
		})
		
	
		
#		plot(NULL, NULL, xlim=c(-180,180), ylim=c(-90,90))
#		for(i in 1:length(finalList)){
#	#	for(i in 430:458){
#			tsp<-SpatialPolygons(finalList[i])
#			plot(tsp, add=TRUE)
#	#		Sys.sleep(0.3)
#		}
		
		endObj<-sp::SpatialPolygons(finalList, proj4string=CRS("+proj=longlat +a=6371007 +b=6371007"))
		
		return(endObj)
	}
)



#' Add SpatialPolygons object to a predefined slot in a trigrid or hexagrid object
#'
#' @name newsp
#		
#' @rdname newsp-methods
#' @param gridObj an icosahedral grid.
#' @param res integer, the number of points inserted between two vertices, passed to \code{SpPolygons()}.
#' 
#' @return A trigrid or hexagrid class object.
#' @examples
#'	a<-trigrid(4)
#'	a<-newsp(a)
#'	plot(a)
#' @exportMethod newsp
setGeneric(
	name="newsp",
	package="icosa",
	def=function(gridObj,res=50){
		standardGeneric("newsp")
	}

)

#' @rdname newsp-methods
setMethod(
	"newsp",
	signature="trigrid",
	definition=function(gridObj, res=50){
		gridObj@sp<-SpPolygons(gridObj,res=res)
		return(gridObj)
	
	}
)

#' Plot method for the trigrid and hexagrid classes
#' 
#'	This function will invoke the \code{plot()} method of the SpatialPolygons class.
#' @param projargs a projection string for the transformation fo coordinates. Accepts both a CRS class object and a character string that will be transformed to the CRS class.
#' @param ... arguments passed to the sp::plot() function.
#' @rdname plot-methods
#' @exportMethod plot
setMethod(
	"plot",
	signature="trigrid",
	definition=function(x,projargs=NULL,...){
		#if no @sp found
		if(suppressWarnings(is.na(x@sp))){
			stop("Slot @sp is empty. Use newsp() to add a 2d respresentation. ")
		}
		
		#transformation is necessary
		if(!is.null(projargs)){
			#	requireNamespace("rgdal")
			if(requireNamespace("rgdal", quietly = TRUE)){
				if(class(projargs)=="CRS"){
					x@sp<-sp::spTransform(x@sp, projargs)
				}
				if(class(projargs)=="character"){
					x@sp<-sp::spTransform(x@sp, sp::CRS(projargs))
				}
			} else{
				stop("The rgdal package is required to appropriately project this object. ")
			}
		
		}
	
		sp::plot(x@sp, ...)
	#	rect(ytop=90, ybot=-90, xleft=-180, xright=180)
	
	}
)

#' Lines method for the trigrid and hexagrid classes
#' 
#'	This function will invoke the \code{lines()} method of the SpatialPolygons class.
#' @param x the trigrid or hexagrid class object.
#' @param projargs a projection string for the transformation fo coordinates. Accepts both a CRS class object and a character string that will be transformed to the CRS class.
#' @param ... arguments passed to the sp::lines() method of the SpatialPolygons class.
#' @rdname lines-methods
#' @exportMethod lines
setMethod(
	"lines",
	signature="trigrid",
	definition=function(x,projargs=NULL,...){
		#if no @sp found
		if(suppressWarnings(is.na(x@sp))){
			stop("Slot @sp is empty. Use newsp() to add a 2d respresentation. ")
		}
		
		#transformation is necessary
		if(!is.null(projargs)){
			if(requireNamespace("rgdal", quietly = TRUE)){
				if(class(projargs)=="CRS"){
					x@sp<-sp::spTransform(x@sp, projargs)
				}
				if(class(projargs)=="character"){
					x@sp<-sp::spTransform(x@sp, sp::CRS(projargs))
				}	
			} else{
				stop("The rgdal package is required to appropriately project this object. ")
			}
				
		}
		
		raster::lines(x@sp,...)
	
	}
)

#' Labels of grid vertices, faces and edges
#' 
#' This function will show where the grid elements are located.
#' @param gridObj a trigrid or hexagrid class icosahedral grid.
#' @param type The type of element to be plotted: either "f" (faces), "v" (vertices) or "e" (edges).
#' @param projargs a projection string for the transformation fo coordinates. Accepts both a CRS class object and a character string that will be transformed to the CRS class.
#' @param ... Arguments passed to the text() function.
#' @return the function has no return value
#' @export
gridlabs<-function(gridObj,type="f",projargs=NULL,...){
	# center back to origin if not there already
		if(gridObj@center[1]!=0 | gridObj@center[2]!=0 | gridObj@center[3]!=0){
			gridObj<-translate(gridObj,-gridObj@center)
		}
		
	if(type=="f"){
		texts<-rownames(gridObj@faceCenters)
		coords<-CarToPol(gridObj@faceCenters, norad=TRUE, origin=gridObj@center)
	}
	
	if(type=="v"){
		texts<-rownames(gridObj@vertices)
		coords<-CarToPol(gridObj@vertices, norad=TRUE, origin=gridObj@center)
	}
	if(type=="e"){
		texts<-rownames(gridObj@edges)
		coord3d<-t(apply(gridObj@edges, 1, function(x){
			apply(gridObj@vertices[x,],2,mean)
		}))
		coords<-CarToPol(coord3d, norad=TRUE, origin=gridObj@center)
		
	}
	
	spPoints<-sp::SpatialPoints(coords, proj4string=CRS("+proj=longlat +a=6371007 +b=6371007"))
	
	#transformation is necessary
	if(!is.null(projargs)){
		#	requireNamespace("rgdal")
		if(requireNamespace("rgdal", quietly = TRUE)){
			if(class(projargs)=="CRS"){
				spPoints<-sp::spTransform(spPoints, projargs)
			}
			if(class(projargs)=="character"){
				spPoints<-sp::spTransform(spPoints, sp::CRS(projargs))
			}

		} else{
			stop("The rgdal package is required to appropriately project this object. ")
		}
		
	}
	
	graphics::text(labels=texts, x=spPoints@coords[,1], y=spPoints@coords[,2],...)
	
}

			
