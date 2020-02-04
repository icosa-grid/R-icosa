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
#' @param res The number of points inserted between two vertices, or \code{NULL}, if this is to be set by the package. The default method increases resolution wiht lower tessellation values, and is higher for higher absolute latitudes.
#' @rdname SpLines-methods
#' @aliases SpLines-trigrid-method
#' @return an object of class SpatialLines.
#' @exportMethod SpLines
setMethod(
	"SpLines",
	signature="trigrid",
	definition=function(gridObj, dateLine="break", res=NULL){
		# center back to origin if not there already
		if(gridObj@center[1]!=0 | gridObj@center[2]!=0 | gridObj@center[3]!=0){
			gridObj<-translate(gridObj,-gridObj@center)
		}
		
		#extend the faces
		v<-gridObj@skeleton$v[as.logical(gridObj@skeleton$aV),]
		f<-gridObj@skeleton$f[as.logical(gridObj@skeleton$aF),1:3]
		
	
		# prepare resolution vector
		if(is.null(res)){
			# the entire implementations is then
			# if(dynamic)
			minres <- ceiling(1/prod(gridObj@tessellation)^2*500)

			# maxres should be 50 more, whichever it is - doesn't matter for coarse grids, 
			# and 50 is enough for everything. 
			maxres <- minres+500

			# then the latitudinal correction needs to be added
				# the frequency of cells in latitudinal belts
				tabBelt <- table(gridObj@belts)^1.3

				# how many plus vertices are needed in each belt?
				plusBelt <- round((maxres-minres)/as.numeric(tabBelt))

				# final resolution vector
				res <- minres+plusBelt[gridObj@belts]

		}else{
			if(is.numeric(res)){
				res <- rep(res, nrow(gridObj@faces))
			}else{
				stop("The provided resolution value is not numeric and not 'NULL'. ")
			}
		}
		
		
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
	

