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



#' Calculation of points along an arc
#' 
#' This function calculates points along an arc between two points and and a circle center.
#' 
#' The function always returns the smaller arc, with angle alpha < pi.
#' 
#' @param p1 Numeric vector, XYZ or longitude-latitude coordinates of the first point along the arc.
#' 
#' @param p2 Numeric vector, XYZ or longitude-latitude coordinates of the last point along the arc.
#' 
#' @param origin Numeric vector, The center of the circle in XYZ coordinates (default is 0,0,0).
#' 
#' @param breaks Single positive integer, the number of points inserted between p1 and p2.
#' 
#' @param onlyNew Logical value whether of p1 and p2 should be omitted from the result or not.
#' 
#' @param output Character value, the coordinate system of the output points. Can either be \code{"polar"} for
#' 	longitude-latitude or \code{"cartesian"} for XYZ data.
#'
#' @param radius Numeric value, the radius of the circle in case the input points have only polar coordinates.
#'	Unused when XYZ coordinates are entered. Defaults to the authalic radius of Earth ca. 6371.007km.
#' 
#' @return Either an XYZ or a long-lat numeric matrix.
#' 
#' @examples
#'	# empty plot
#'	plot(NULL, NULL, xlim=c(-180, 180), ylim=c(-90,90))
#'	# then endpoints of the arc
#'	point1<-c(-45,-70)
#'	point2<-c(130,65)
#'	points(arcpoints(point1, point2, breaks=70, output="polar"))
#'
#' @export arc
arcpoints<-function(p1,p2,breaks=2,origin=c(0,0,0), onlyNew=FALSE, output="cartesian", radius=authRadius){
	if(!output%in%c("cartesian", "polar")) stop("Invalid \'output\' argument.")
	
	if(!is.logical(onlyNew)| length(onlyNew)>1) "Invalid \'onlyNew\' argument."

	if(!is.numeric(breaks) | length(breaks)!=1 | breaks%%1!=0) stop("Invalid \'breaks\' argument")
	
	if(!is.numeric(p1) | !is.numeric(p2) | !is.numeric(origin)) stop("Invalid coordinate input.")
	
	if(!sum(is.finite(p1))%in%c(2,3) | 
		!sum(is.finite(p2))%in%c(2,3) | 
		!sum(is.finite(origin))%in%c(2,3)) stop("Invalid coordinate input.")
	
	if(length(p1)==2){
		p1<-PolToCar(matrix(p1, ncol=2, nrow=1),radius=radius, origin=origin)
		p1<-p1[1,]
	}
	
	if(length(p2)==2){
		p2<-PolToCar(matrix(p2, ncol=2, nrow=1),radius=radius, origin=origin)
		p2<-p2[1,]
	}
		
	#invoke Rcpp function
	temp<-.Call(Cpp_icosa_SplitArc_, p1, p2, origin, breaks, onlyNew)
	
	colnames(temp)<-c("x", "y", "z")
	
	if(output=="polar"){
		temp<- CarToPol(temp, norad=TRUE, origin=origin )
	}
	
	return(temp)
}

#' Calculation of distances along arcs
#' 
#' This function calculates the shortest arc distance between two points.
#' 
#' @param p1 Numeric vector, XYZ or longitude-latitude coordinates of the first point along the arc.
#' 
#' @param p2 Numeric vector, XYZ or longitude-latitude coordinates of the last point along the arc.
#' 
#' @param origin Numeric vector, the center of the circle in XYZ coordinates (default is 0,0,0).
#' 
#' @param output Character value, the type of the output value. \code{"distance"} will give the distance
#'	in the metric that was fed to the function for the coordinates or the radius.
#'	\code{"deg"} will output the the distance in degrees, \code{"rad"} will do
#'	so in radians.
#'
#' @param radius Numeric value, the radius of the circle in case the input points have polar coordinates only.
#'	Unused when XYZ coordinates are entered. Defaults to the authalic radius of Earth ca. 6371.007km.
#'
#' @return A single numeric value.
#'
#' @examples 
#'	# coordinates of two points
#'	point1<- c(0,0)
#'	point2<- c(180,0)
#'	arcdist(point1,point2,"distance")
#' @export arcdist
arcdist <- function(p1, p2, output="distance", origin=c(0,0,0), radius=authRadius) {
    if(!is.numeric(p1) | !is.numeric(p2) | !is.numeric(origin)) stop("Invalid coordinate input.")
	
	if(!output%in%c("distance", "deg", "rad")) stop("Invalid \'output\' argument.")
	
	if(!sum(is.finite(p1))%in%c(2,3) | 
		!sum(is.finite(p2))%in%c(2,3) | 
		!sum(is.finite(origin))%in%c(2,3)) stop("Invalid coordinate input.")
	
	if(length(p1)==2){
		p1<-PolToCar(matrix(p1, ncol=2, nrow=1),radius=radius, origin=origin)
		p1<-p1[1,]
	}
	
	if(length(p2)==2){
		p2<-PolToCar(matrix(p2, ncol=2, nrow=1),radius=radius, origin=origin)
		p2<-p2[1,]
	}
		
	if(output=="distance") method<-T
	if(output%in%c("deg", "rad")) method<-F
	
	result<-.Call(Cpp_icosa_ArcDist_, p1, p2, origin, method)
	
	#acos() out of domain error
		if(is.nan(result) & output=="distance"){
			v<-p1-origin
			result <- pi*sqrt(v[1]^2+v[2]^2+v[3]^2)
		} 
		
		if(is.nan(result) & output%in%c("deg","rad")){
			v<-p1-origin
			result <- pi
		} 
	
	if(output=="deg"){
		result<-result*180/pi
	}

	return(result)
	
}

#' Calculation of distance matrices along arcs
#' 
#' This function calculates the shortest arc distance matrix between two set of points.
#' 
#' This function will create all possible shortest arc distances between points in the two sets,
#' 	but not between the points within the sets. The function is useful for great circle distance calculations.
#' 	For a symmetrical distance matrix leave the \code{points2} argument empty.
#' 
#' @param points1 Numeric matrix, XYZ or longitude-latitude coordinates of the first set of points.
#' 
#' @param points2 Numeric matrix, XYZ or longitude-latitude coordinates (matrix) of the second set of points. 
#'	Leave this empty if you want all the arc distances between a set of points	
#' 
#' @param origin Numeric vector, the center of the circle in XYZ coordinates (default is 0,0,0).
#' 
#' @param output Character value, the type of the output value. \code{"distance"} will give back the distance
#' 	in the metric that was fed to the function in the coordinates or the radius.
#' 	\code{"deg"} will output the the distance in degrees, \code{"rad"} will do
#' 	so in radians.
#' 
#' @param radius Numeric value, the radius of the circle in case the input points have polar coordinates only.
#' 	Unused when XYZ coordinates are entered. Defaults to the authalic radius of Earth ca. 6371.007km.
#' 
#' @return A single numeric value.
#' 
#' @examples
#' g <- trigrid(c(4))
#' res <- arcdistmat(g@vertices)
#' 
#' rand<-rpsphere(500)
#' res2 <- arcdistmat(g@vertices, rand)
#'
#'	@export
arcdistmat<-function(points1, points2=NULL, origin=c(0,0,0), output="distance", radius=authRadius){
	# output argument
	if(!output%in%c("distance", "deg", "rad")) stop("Invalid \'output\' argument.")
	if(output=="distance") method<-T
	if(output%in%c("deg", "rad")) method<-F
	
	present<-T
	if(is.null(points2)){
		points2<-points1
		present<-F
	}
	
	if(!is.numeric(points1) | !is.numeric(points2) | !is.numeric(origin)) stop("Invalid coordinate input.")
	
	
	if(!ncol(points1)%in%c(2,3) | 
		!ncol(points2)%in%c(2,3) | 
		!sum(is.finite(origin))%in%c(2,3)) stop("Invalid coordinate input.")
	
	if(sum(is.na(points1))>0 | sum(is.na(points2))>0) stop("The coordinates include NAs")
	if(ncol(points1)==2){
		points1<-PolToCar(matrix(points1, ncol=2, nrow=1),radius=radius, origin=origin)
		points1<-points1[1,]
	}
	
	
	
	if(ncol(points2)==2){
		points2<-PolToCar(matrix(points2, ncol=2, nrow=1),radius=radius, origin=origin)
		points2<-points2[1,]
	}
		
	if(present){
		distMat<- .Call(Cpp_icosa_ArcDistMat_, points1, points2, origin, method)
	}else{
		distMat<- .Call(Cpp_icosa_SymmetricArcDistMat_, points1, origin, method)
	}
	
	rownames(distMat)<-rownames(points1)
	colnames(distMat)<-rownames(points2)
	
	#acos() out of domain error
		if(sum(is.nan(distMat))>0 & output=="distance"){
			v<-points1[1,]-origin
			distMat[is.nan(distMat)] <- pi*sqrt(v[1]^2+v[2]^2+v[3]^2)
		} 
		
		if(sum(is.nan(distMat))>0 & output%in%c("deg","rad")){
			v<-points1[1,]-origin
			distMat[is.nan(distMat)] <- pi
		} 

		
	if(output=="deg"){
		distMat<-distMat*180/pi
	}
	return(distMat)
}






blankSphere <- function (x, y=NULL, z=NULL, res=100, radius = authRadius, color="white", add=FALSE, ...) {
	
	# lat and long values
	lat <- matrix(seq(90, -90, len = res)*pi/180, res, res, byrow = TRUE)
	long <- matrix(seq(-180, 180, len = res)*pi/180, res, res)
	
	# the center of the sphere
	center <-c(x,y,z)
	
	add2 <- add
	x <- center[1] + radius*cos(lat)*cos(long)
	y <- center[2] + radius*cos(lat)*sin(long)
	z <- center[3] + radius*sin(lat)
	rgl::persp3d(x, y, z, specular="white", add=add2, color=color, ...)
	
}



collapse<-function(vect){
	oldClass<-class(vect)
	factorized<-factor(vect)
	newFactor<-factor(.Call(Cpp_icosa_Collapse_, factorized))
	levels(newFactor)<-levels(factorized)
	newVect<-as.character(newFactor)
	class(newVect) <- oldClass
	return(newVect)
}

#function to rotate a single point
rotateOnePoint<-function(coords, angles,origin)
{
	#coords<-c(0,1,0)
	#angles<-c(pi/2,pi/2,pi)
	
	#the rotation matrix
	rotMat<-function(theta)
	{
		mat<-matrix(NA,ncol=2,nrow=2)
		mat[1,1]<-cos(theta)
		mat[1,2]<--sin(theta)
		mat[2,1]<-sin(theta)
		mat[2,2]<-cos(theta)
		return(mat)
	}
	
	#location vector
	locVec<-coords-origin
			
	#first rotation
	#around x
	xMat<-matrix(locVec[2:3],ncol=1, nrow=2)
	xMat<-rotMat(angles[1])%*%xMat
	locVec<-c(locVec[1],as.numeric(xMat))
	
	#second rotation
	yMat<-matrix(locVec[c(1,3)],ncol=1, nrow=2)
	yMat<-rotMat(angles[2])%*%yMat
	locVec<-c(yMat[1], locVec[2],yMat[2])
	
	#third rotation
	zMat<-matrix(locVec[c(1,2)],ncol=1, nrow=2)
	zMat<-rotMat(angles[3])%*%zMat
	locVec<-c(zMat[1:2], locVec[3])
	
	return(locVec)

}
	
# function to create random points on the sphere
#' Random point generation on the surface of a sphere
#' 
#' This function will create a predefined number of points randomly distributed
#' on the surface of a sphere with a given radius.
#' 
#' The function uses a three dimension normal distribution to generate points, 
#' which are then projected to the surface of the sphere.
#' 
#' @param n The number of random points to be created.
#' 
#' @param radius The radius of the sphere
#' 
#' @param origin The center of the sphere (XYZ coordinates).
#' 
#' @param output The coordinate system of the new points. Can either be 
#'	\code{"cartesian"} for XYZ coordiates or \code{"polar"} for spherical, 
#'	longitude-latitudes coordinates.
#'
#' @return A 3-column (XYZ) or a 2-column (long-lat) numeric matrix.
#' 
#' @examples
#'	randomPoints <- rpsphere(20000)
#'	points3d(randomPoints)
#' 
#' @export
rpsphere <- function(n=1, output="cartesian", radius=authRadius, origin=c(0,0,0)){
	if(!is.numeric(radius) |
	length(radius)!=1) stop("Invalid input for argument \'radius\'.")
	
	if(!is.numeric(n) |
	length(n)!=1) stop("Invalid input for argument \'n\'.")

	if(!is.numeric(origin) |
	length(origin)!=3) stop("Invalid input for argument \'origin\'.")
	
	
	if(!output%in%c("polar", "cartesian")) 
	
	stop("Invalid input for argument \'output\'.")
		#random variables
		x1 <- stats::rnorm(n, 0, 1)
		y1 <- stats::rnorm(n, 0, 1)
		z1 <- stats::rnorm(n, 0, 1)
		origPoints<-cbind(x1,y1,z1)
	
	# location vectors
	vectors<-origPoints
	vectors[,1]<-origPoints[,1]
	vectors[,2]<-origPoints[,2]
	vectors[,3]<-origPoints[,3]
	
	# distances from the origin
	dists<-sqrt(vectors[,1]^2+vectors[,2]^2+vectors[,3]^2)
	
	#project the point to the sphere
	newPoints<-origPoints
	newPoints[,1]<-origPoints[,1]*radius/dists
	newPoints[,2]<-origPoints[,2]*radius/dists
	newPoints[,3]<-origPoints[,3]*radius/dists
		
	# column names
	colnames(newPoints)<-c("x", "y", "z")
	
	#outputs
	if(output=="polar"){
		result<-CarToPol(newPoints, norad=TRUE, origin=origin)
		colnames(result) <- c("long", "lat")
	}else{
		result<-newPoints
		result[,1] <- result[,1]+origin[1]
		result[,2] <- result[,2]+origin[2]
		result[,3] <- result[,3]+origin[3]
	}
	
	return(result)
}



# small, fast utility function for the lookup of vertices (used in lookup!)
whichVertices<-function(vertices, data){
	# result if no vertex found
	vertIndex<-NULL
	
	#the only total check 
	ctrl1<-data[,1]%in%vertices[,1]
	
	#first coordinate match
	if(sum(ctrl1)>0){
		datSub<-data[ctrl1,,drop=FALSE]
		indSub<-which(ctrl1)
		ctrl2<-datSub[,2]%in%vertices[,2]
		
		#second coordinate match too
		if(sum(ctrl2)>0){
			indSub2<-indSub[ctrl2]
			datSub2<-datSub[ctrl2,,drop=FALSE ]
			ctrl3<-datSub2[,3]%in%vertices[,3]
			
			#third matches as well: vertex!
			if(sum(ctrl3)>0){
				vertIndex<-indSub2[ctrl3]
			}
		}
	
	}

	return(vertIndex)
}


# function to create a legend for a heatmap
#' Legend for a heatmap with predefined colors.
#'
#' This function will invoke the plot function to draw a heatmap legend.
#' 
#' The 'percents' refer to the plotting area measured from the lower left corner.
#' @rdname heatMapLegend
#'
#' @param cols Character vector, containnig the ordered colors that are used for the heatmap.
#' @param tick.text The values on the heatmap legend. If missing, will be calculated with minVal and maxVal. Should have the length as 'ticks'.
#' @param minVal If tick.text is missing, the lowest value in the heatmap
#' @param maxVal If tick.text is missing, the highest value in the heatmap
#' @param ticks The number of ticks/values in the heatmap legend (the bar will be divided to this number).
#' @param tick.cex Letter size of the values on the legend.
#' @param varName The label of the variable name plotted to the heatmap.
#' @param barWidth The width (percent) of the bar featuring the colors of the heatmap.
#' @param barHeight The height (percent)of the bar featuring the colors of the heatmap.
#' @param tickLength The length (percent) of the ticks at the bars.
#' @param xLeft the x coordinate of the lower left hand corner of the bar.
#' @param yBot the y coordinate of the lower left hand corner of the bar.
#' @param add indicates wheter a new plot should be drawn or not. Defaults to FALSE.
#' @param ... arguments passed to the plot() function.
#' @export		
heatMapLegend<-function(cols, minVal, varName, tick.text, maxVal,ticks=5, tick.cex=1.5, barWidth=3, barHeight=50,tickLength=1, xLeft=88, yBot=25, add=FALSE, ...){
	
	if(!add){
		plot(NULL, NULL, xlim=c(0,100), ylim=c(0,100), axes=FALSE, xlab="", ylab="",xaxs="i", yaxs="i",...)
	}
	#	barWidth<-3
	#	tickLength=1
	#	xLeft<-88
	#	minVal<-min(x@values)
	#	maxVal<-max(x@values)
	
		x2<-xLeft+barWidth
		x3<-x2+tickLength
		x4<-x3+tickLength
		
		
	#	ybot<-25
		ytop<-yBot+barHeight
		
		ySubLab<-ytop*1.1
	
		
		ts<-seq(yBot,ytop, length.out=ticks)
		
		if(missing(tick.text)){
			tick.text<-seq(minVal,maxVal, length.out=ticks)
		#	tick.text<-format(tick.text, digits=5)
		}
		
		for(i in 1:ticks){
			graphics::segments(x0=x2,x1=x3, y0=ts[i], y1=ts[i])
			
			graphics::text(label=format(tick.text[i], digits=5), x=x4, y=ts[i], pos=4,cex=tick.cex)
		}
		
		rectTops<-seq(yBot, ytop,length.out=length(cols)+1)
		for(i in 2:length(rectTops)){
			graphics::rect(xleft=xLeft, xright=x2, ytop=rectTops[i],ybottom=rectTops[i-1], 
			col=cols[i], border=NA)
		
		}
		graphics::rect(ytop=yBot, ybottom=ytop, xleft=xLeft, xright=x2)
	
		if(missing(varName)) varName<-""
		graphics::text(varName, x=xLeft, y=ySubLab, pos=4, cex=tick.cex)
	
}
	

# utility function to test whether the grid linked to a layer can host it or not
checkLinkedGrid <-function(gridObj, fl){
	if(!prod(gridObj@tessellation)==prod(fl@tessellation) |
		fl@gridclass!=class(gridObj) |
		sum(fl@names%in%rownames(gridObj@faces))!=length(fl@names))
		stop("The linked grid doesn't match the facelayer's grid structure.")
}




#' Surface centroid point of a spherical point cloud
#' 
#' This function the projected place of the centroid from a pointset on the sphere
#' 
#' The function implements great circle calculations to infer on the place of the centroid, which makes it resource demanding. This is necessary
#'	to avoid a particual error that frequently occurrs with other methods for centroid calculation, namely that the place of the centroid is right,
#' 	but on the opposite hemisphere.
#' 
#' @param data  Numeric matrix, XYZ or longitude-latitude coordinates of the set of points.
#' 
#' @param output Character value, the coordinate system of the output points. Can either be \code{"polar"} for
#' 	longitude-latitude or \code{"cartesian"} for XYZ data.
#'
#' @param center Numeric vector, The center of the sphere in XYZ coordinates (default is 0,0,0).
#' 
#' @param radius Numeric value, the radius of the circle in case the input points have only polar coordinates.
#'	Unused when XYZ coordinates are entered. Defaults to the authalic radius of Earth ca. 6371.007km.
#'
#' @param inner Single positive integer, the number of points inserted between every two points of the spherical centroid. Heavilly impacts the performance.
#' 
#' @return Either an XYZ or a long-lat numeric vector.
#' 
#' @examples
#'	# generate some random points
#'	allData <- rpsphere(1000)
#'	# select only a subset
#'	points<-allData[allData[,1]>3000,]
#'	# the spherical centroid
#'	sc <- surfacecentroid(points)
#'	sc
#'	
#'	#3d plot
#'	plot3d(points)
#'	points3d(sc[1], sc[2], sc[3], col="red", size=5)
#'
#' @export surfacecentroid
surfacecentroid<-function(data, output="polar", center=c(0,0,0), radius=authRadius, inner=20)
{
	#data argument
	# which formatting?
	if(ncol(data)==2){
	
		# transform the two columns
		data<-PolToCar(data, origin=center, radius=radius)
	}
	if (ncol(data)==3){
		radVec<-data[1,]-center
		rad<-sqrt(radVec[1]^2+radVec[2]^2+radVec[3]^2)
	}
	
	#create all combinations of the great circles 
		combinations<-t(utils::combn(1:nrow(data),2))
		
		# C indexing (0)
		combinations<- combinations-1
	
	# get the coordinates+ combinations + resolution var = matrix of all the great circles
	 all<- .Call(Cpp_icosa_centroidPoints_, data, combinations, center, inner)
		
	
	#the 3d centroid of the point cloud
		centroid3d<-apply(all, 2, mean, na.rm=TRUE)
		if(output=="cartesian"){
			radVec<-(centroid3d-center)
			retCentroid<-centroid3d/(sqrt(radVec[1]^2+radVec[2]^2+radVec[3]^2))*rad
			return(retCentroid)
		}
	
	#transform back to spherical coordinates
	if(output=="polar"){
		#the longitude problem!!!
		xSign<-sign(centroid3d[1])
	
		theta<-atan(centroid3d[2]/centroid3d[1])
		phi<-atan(sqrt(centroid3d[1]^2+centroid3d[2]^2)/centroid3d[3])
	
	#transform spherical coordinates to long/lat
		theta<-theta/pi*180
		phi<-phi/pi*180
	
		#convert to lat-long
		if(phi>=0) lat<-90-phi
		if(phi<0) lat<--90-phi
		
		if(xSign<0 & theta<=0) long<-180+theta
		if(xSign<0 & theta>0) long<--180+theta
		if(xSign>=0) long<-theta
		
		#return value
		centroidLongLat<-c(long, lat)
		names(centroidLongLat)<-c("long", "lat")
	
	
	#return value
		return(centroidLongLat)
	}		
}



#' Spherical convex hull
#' 
#' This function calculates a possible implementation of the spherical convex hull
#' 
#' With the method \code{centroidprojection} the function calls the surfacecentroid() 
#'	function to get the a reference point from the shape. Then all the points are 'projected' 
#'	close to this point using the great circles linking them to the reference point.
#'	Each such great circle will be devided to an equal number of points and the closest
#'	 will replace the original point coordinates in the convex hull algorithm implemented in \code{grDevices::chull()}. 
#' 
#' @param data  Numeric matrix, XYZ or longitude-latitude coordinates of the set of points.
#' 
#' @param center Numeric vector, The center of the sphere in XYZ coordinates (default is 0,0,0).
#' @param method Character value, indicating the method to create the spherical convex hulls.
#' @param inner Single positive integer, the number of points inserted between every two points of the spherical centroid. Heavilly impacts the performance.
#'
#' @param param Single positive integer, indicates the number of divisions in the centroidprojection method. The higher the number, the closer the replacement points are to #'	the centroid.
#' 
#' @return Either an XYZ or a long-lat numeric vector.
#' 
#' @examples
#'	# generate some random points
#'	allData <- rpsphere(1000)
#'	# select only a subset
#'	points<-allData[allData[,1]>3000,]
#'	chullsphere(points)
#'	
#'
#' @export chullsphere
chullsphere<-function(data, center=c(0,0,0), method="centroidprojection", param=200, inner=10)
{
	if(ncol(data)==2){
		# transform the two columns
		data<-PolToCar(data, origin=center, radius=authRadius)
	}
	if (ncol(data)==3){
		radVec<-data[1,]-center
		rad<-sqrt(radVec[1]^2+radVec[2]^2+radVec[3]^2)
	}
	
	#calculate the group centroid
		centroid<-surfacecentroid(data, center=center, radius=authRadius, output="cartesian", inner=10)

	#shrink the 2d surface proportionally! to the vicinity of the reference point of latLong 2d space(roughly planar area)
	if(method=="centroidprojection"){
		projectedPoints<-.Call(Cpp_icosa_projectCloseToPoint_, data, centroid, center, param)
	
		projP<- CarToPol(projectedPoints,norad=TRUE)
		
		return(grDevices::chull(projP))
	}
}


#only one for the hexagrid: subface boundaries should give back an entry even if the randomborder is FALSE
approximateFace<-function(coords, n, d, gridObj, onlyOne=FALSE, output="skeleton"){
	# test variables
#	d<-10e-6
#	n<-15
	# matrix of random coordinates around this point
	randMat<-cbind(coords[1]+stats::rnorm(n,0,d), coords[2]+stats::rnorm(n,0,d), coords[3]+stats::rnorm(n,0,d))
	
	if(class(gridObj)=="hexagrid"){
		temp<-suppressWarnings(locate(gridObj, randMat, output=output, randomborder=FALSE, forceNA=TRUE))
	}else{
		temp<-suppressWarnings(locate(gridObj, randMat, output=output, randomborder=FALSE))
	}
	temp<-unique(temp[!is.na(temp)])
	
	if(onlyOne){
		if(length(temp)==1){
			return(temp)
		}else{
			return(NA)
		}
	}else{
		return(sample(temp,1))
	}
}
		
