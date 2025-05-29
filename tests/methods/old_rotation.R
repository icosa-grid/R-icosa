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

#function to rotate a single point
# @param coords A 3-value vector (cartesian)
# @param angles A 3-value vector (cartesian)
# @param origin A 3-value vector (cartesian)
rotateOnePoint<-function(coords, angles,origin)
{
	#coords<-c(0,1,0)
	#angles<-c(pi/2,pi/2,pi)


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
