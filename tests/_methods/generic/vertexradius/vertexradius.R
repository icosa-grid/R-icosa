# input: gr: standard orientation, position and scale grid
expect_silent(vr <- vertexradius(gr))

# types
expect_true(inherits(vr, "matrix"))

# names are there
faceNames <-rownames(vr)
expect_true(all(faces(gr)%in%faceNames))
expect_true(all(faceNames%in%faces(gr)))

# first and 10th faces yield correct results
faceMat <- gr@faces
vertMat <- gr@vertices
centers <- gr@faceCenters
for(i in 1:ncol(faceMat)){
	if(!is.na(faceMat[1, i])){
		res <- expect_equivalent(
			arcdist(centers[1, ,drop=FALSE], vertMat[faceMat[1, i],, drop=FALSE], output="deg"),
			vr[1,i])
	}
	if(!is.na(faceMat[10, i])){
		res2 <- expect_equivalent(
			arcdist(centers[10, ,drop=FALSE], vertMat[faceMat[10, i],, drop=FALSE], output="deg"),
			vr[10,i])
	}
}
res
res2

# same with explict degrees
expect_silent(vrDeg <- vertexradius(gr, degree=TRUE))
expect_equal(vrDeg, vr)

# the same with kilometers
expect_silent(vrDist <- vertexradius(gr, degree=FALSE))
expect_equal(dim(vrDist), dim(vr))
expect_equal(rownames(vrDist), rownames(vr))

# the radius is giving accurate results
radius <- sqrt(sum((gr@center-gr@vertices[1,])^2))
expect_equal(vr/180*pi*radius, vrDist)

# moving of the grid
# rotation
expect_silent(gr2 <- rotate(gr))
expect_silent(vr2 <- vertexradius(gr2))
expect_equal(vr, vr2)

# translation
expect_silent(gr3 <- translate(gr, vec=c(500, 1500, 2000)))
expect_silent(vr3 <- vertexradius(gr3))
expect_equal(vr, vr3)
