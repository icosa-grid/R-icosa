#' Conversion of spherical coordinates to 3d Cartesian coordinates
#' 
#' The function uses basic trigonometric relationships to transform longitude/latitude coordinates on a sphere to xyz Cartesian coordinates.
#' 
#' The authalic mean radius of Earth (6371.007 km) is used by this function as a default while the origin is c(0,0,0). The precision of these conversions is not exact (see example c(0,90) below),
#' but should be considered acceptable when applied at a reasonable scale (e.g. for global analyses using data above 10e-6 meters of resolution).
#' 
#' @param longLatMat A 2-column numerical matrix with the longitude/latitude data.
#' 
#' @param radius Single numeric value, indicating the radius of the sphere. Defaults to the R2 radius of Earth (6371.007km).
#'
#' @param origin Numeric vector of length 3, the xyz coordinates of the sphere center.
#' 
#' @return An xyz 3-column numeric matrix.
#' 
#' @export PolToCar
#'
#' @examples 
#' longLat <- rbind(
#'     c(0,0),
#'     #note the precision here!
#'     c(0, 90),
#'     c(-45,12)
#' )
#' 
#' xyz <- PolToCar(longLat)
PolToCar <- function(longLatMat, radius=authRadius, origin=c(0,0,0)){

	# allow only numeric matrices to pass through (or data frames with numeric vars)
	if(!class(longLatMat)%in%c("matrix", "data.frame")){
		stop("Invalid input.")
	}
	
	if((class(longLatMat[,1])=="numeric")+(class(longLatMat[,2])=="numeric")!=2){
		stop("Invalid input matrix or data frame.")
	}
	
	
	#ignore the NAs
	boolNA<-(is.na(longLatMat[,1]) | is.na(longLatMat[,2]))
	longLat<-longLatMat[!boolNA,, drop=FALSE]
	
	if(length(radius)!=1 | !is.numeric(radius)) stop("Invalid \'radius\' value.")
	
	#essential! check whether the long is first and lat is second
	if(max(abs(longLat[,2]))>90) stop("Latitudinal data should be in the second column of the matrix.")
	
	x<-cos(longLat[,2]/180*pi)*cos(longLat[,1]/180*pi)
	y<-cos(longLat[,2]/180*pi)*sin(longLat[,1]/180*pi)
	z<-sin(longLat[,2]/180*pi)
	
	newMat<-matrix(NA, ncol=3, nrow=nrow(longLatMat))
	newMat[!boolNA,]<-cbind(x,y,z)
	colnames(newMat)<-c("x", "y", "z")

	endRes<-newMat*radius
	
	# do the translocation if necessary
	endRes[,1]<-endRes[,1]+origin[1]
	endRes[,2]<-endRes[,2]+origin[2]
	endRes[,3]<-endRes[,3]+origin[3]
	
	return(endRes)

}


#' Conversion of 3d Cartesian coordinates to polar coordinates
#' 
#' The function uses basic trigonometric relationships to transform xyz coordinates to polar coordinates
#' 
#' @param matXYZ 3-column numeric matrix containing xyz coordinates in a Cartesian space
#' @param origin Numeric vector of length 3, defining the center of the sphere. Defaults to c(0,0,0).
#' @param norad Logical value, toggles whether the rho coordinate (distance from origin) should be omitted or not.
#' 
#' @return A 3-column or 2-column numeric matrix with longitude, latitude and, if set accordingly, radius data.
#' 
#' @examples
#' # some random points
#' xyz <- rbind(
#' 	 c(6371, 0,0),
#' 	 c(0, 6371,0),
#' 	 c(1000,1000,1000)
#' )
#' 
#' # conversions
#'   CarToPol(xyz)
#' @export CarToPol
CarToPol <- function(matXYZ, norad=FALSE, origin=c(0,0,0)) {
	#ignore the NAs
	boolNA<-(is.na(matXYZ[,1]) | is.na(matXYZ[,2]) | is.na(matXYZ[,3]))
	matXYZ<-matXYZ[!boolNA,, drop=FALSE]
	
	# center the coordinates to the center of c(0,0,0)
	matXYZ[,1]<-matXYZ[,1]-origin[1]
	matXYZ[,2]<-matXYZ[,2]-origin[2]
	matXYZ[,3]<-matXYZ[,3]-origin[3]
	
	
	#transform back to spherical coordinates
		xSign<-sign(matXYZ[,1])
	
		theta<-atan(matXYZ[,2]/matXYZ[,1])
		phi<-atan(sqrt(matXYZ[,1]^2+matXYZ[,2]^2)/matXYZ[,3])
	
	#transform spherical coordinates to long/lat
		theta<-theta/pi*180
		phi<-phi/pi*180
	
		#convert to lat-long
		long<-rep(NA, length(theta))
		lat<-rep(NA, length(phi))
		
		lat[phi>=0]<-90-phi[phi>=0]
		lat[phi<0]<--90-phi[phi<0]
		
		long[xSign<0 & theta<=0]<-180+theta[xSign<0 & theta<=0]
		long[xSign<0 & theta>0]<--180+theta[xSign<0 & theta>0]
		long[xSign>=0]<-theta[xSign>=0]
	
		rho<-sqrt(matXYZ[,1]^2+matXYZ[,2]^2+matXYZ[,3]^2)
		
		if(sum(is.nan(long))>0)
		{
			long[is.nan(long)]<-0
		
		}
		#the polar problem:
		lat[lat==90 & matXYZ[,3]<0]<- -90
		
		
	if(norad){
		matLongLat<-matrix(NA, ncol=2, nrow=length(boolNA))
		matLongLat[!boolNA,]<-cbind(long, lat)
		rownames(matLongLat)<-rownames(matXYZ)
		colnames(matLongLat)<-c("long", "lat")
		return(matLongLat)
		
	}else{	
		matLongLat<-matrix(NA, ncol=3, nrow=length(boolNA))
		matLongLat[!boolNA,]<-cbind(long, lat, rho)
		rownames(matLongLat)<-rownames(matXYZ)
		colnames(matLongLat)<-c("long", "lat", "rho")
		return(matLongLat)
		
	}
		
}

