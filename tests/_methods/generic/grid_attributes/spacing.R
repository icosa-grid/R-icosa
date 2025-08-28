# input: gr
expect_silent(space <- spacing(gr))

# types
expect_true(inherits(space, "numeric"))

# names are there
faceNames <- unlist(strsplit(names(space), "--"))
expect_true(all(faces(gr)%in%faceNames))
expect_true(all(faceNames%in%faces(gr)))

# all pairs are really neighbors (share exactly two vertices)
facePairs <- t(as.data.frame(strsplit(names(space), "--")))
sharedVertex <- apply(facePairs, 1, function(x){
	one <- gr@faces[x[1],]
	two <- gr@faces[x[2],]
	# pentagonal cases
	one <- one[!is.na(one)]
	two <- two[!is.na(two)]
	# exactly two vertices are shared
	sum(one%in%two)
})
expect_true(all(sharedVertex==2))


# every pair is counted appropriately just once
faceTab <- table(faceNames)

if(inherits(gr, "hexagrid")){
	expect_equal(sum(faceTab==5), 12)
	expect_equal(sum(faceTab==6), length(faces(gr))-12)
}else{
	expect_equal(sum(faceTab==3), length(faces(gr)))
}

# same with explict degrees
expect_silent(spaceDeg <- spacing(gr, degree=TRUE))
expect_equal(spaceDeg, space)

# the same with kilometers
expect_silent(spaceDist <- spacing(gr, degree=FALSE))
expect_equal(names(spaceDist), names(space))

# the radius
radius <- sqrt(sum((gr@center-gr@vertices[1,])^2))
expect_equal(space/180*pi*radius, spaceDist)

# moving of the grid
# rotation
gr2 <- rotate(gr)
expect_silent(space2 <- spacing(gr2))
expect_equal(space, space2)

# translation
gr3 <- translate(gr, vec=c(500, 1500, 2000))
expect_silent(space3 <- spacing(gr3))
expect_equal(space, space3)
