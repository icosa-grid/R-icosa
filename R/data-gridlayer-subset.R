#' Subsetting a gridlayer object.
#'
#' The function extracts subsets of the \code{gridlayer} depending on different criteria.
#'
#' The following methods are incorporated into the function: If the subsetVector argument is a vector of integers, they will be interpreted as indices. If the numeric subsetVector contains either the lamin, lamax, lomin or lomax names, the subsetting will be done using the latitude-longitude coordinates outlined by these 4 values. Logical subsetting and subsetting by face names are also possible.
#'
#' @param x The gridlayer object to be subsetted.
#' @param subsetVector Vector object indicating the faces to be subsetted.
#' @param ... Not in use.
#'
#' @rdname subset
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


#subsetting for layers
#' Extraction from a gridlayer using indices
#' 
#' Shorthand to the subset() function.
#' 
#' @rdname subset
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
#' @rdname subset
#'
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
#' @rdname replace
#'
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

