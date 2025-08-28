# input: xy

expect_silent(whole <- surfacecentroid(xyz))
expect_true(whole[1]>=-180 & whole[1] <= 180)
expect_true(whole[2]>=-90 & whole[2] <= 90)

expect_true(inherits(whole, "numeric"))
expect_true(!inherits(whole, "matrix"))

# method dispatch checks
expect_silent(xy <- CarToPol(xyz, norad=TRUE))

# in case there is just one
if(nrow(xyz)==1){
	expect_equal(xy[1,], whole)
}

expect_silent(whole2 <- surfacecentroid(xy))
expect_equal(whole, whole2)

# the sp method
expect_silent(whole <- surfacecentroid(sp::SpatialPoints(xy)))

# the data.frame method
expect_silent(whole <- surfacecentroid(as.data.frame(xy)))


# missing values should provide identical results as those without
if(nrow(xyz)>5){
	where  <- c(1, 3, nrow(xyz))
	withNA <- xyz
	# insert missing values
	withNA[where, ] <- NA
	expect_silent(surNA <- surfacecentroid(withNA))

	# without missing
	noNA <- xyz[-where,]
	expect_silent(surNoNA <- surfacecentroid(noNA))
	expect_equal(surNA, surNoNA)

}


