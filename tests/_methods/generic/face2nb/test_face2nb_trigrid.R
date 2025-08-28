# This generic test compares the neighbor-list objects that
# are built from the icosa grids to those that are built using
# spdep's poly2nb function. The resulting weight matrices are compared.

# depends on
# @param one: hexagrid-class
# @param faceNB: neigborhood


# the number of entries is the same as the number of faces
expect_equal(as.numeric(length(one)), length(faceNB))

expect_silent(space <- spacing(one))

# depending on the type of neighborhood
if(!queen){

	# number of spacings *2 is the number of links in the neighborhood structure
	expect_equal(length(unlist(faceNB)), length(space)*2)

	# list values are the same
	suppressMessages(suppressWarnings(poly <- poly2nb(one@sf, queen=queen)))
	expect_equivalent(poly, faceNB)

	# the autocorrelation structre
	ico <- spdep::nb2listw(faceNB, style="W", zero.policy=TRUE)
	pol <- spdep::nb2listw(poly, style="W", zero.policy=TRUE)

	# some attributes differ!
	expect_equivalent(ico, pol)

# the queen neighborhood
}else{
	# the different kind of connections
	connections <- unlist(lapply(faceNB, length))

	# the number of faces should be consistent
	if(length(one)>=60){
		expect_equal(sum(connections==11), 60)
		expect_equal(sum(connections==12), as.numeric(length(one))-60)
	}
	suppressMessages(suppressWarnings(poly <- poly2nb(one@sf, queen=queen)))

	# spdep-based cannot be directly compared to the pure icose implementation
	# Polar vertices problematic, when compared to sf-based neighborlist
	# dateline also f*cks things up
	# the spdep-based version only gives a subset, which should be correct, just incomplete
	include<- rep(NA, length(faceNB))
	for(k in 1:length(include)){
		include[k] <-all(poly[[k]]%in%faceNB[[k]])
	}
	expect_equal(sum(include), length(faceNB))

}
