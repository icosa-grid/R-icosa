# test for a suite of grids in the tessellation guide
for(i in 1:nrow(guide)){
	# get tessellation vector
	totVector <- as.numeric(guide[i, paste0("level", 1:4)])
	tessVector <- totVector[!is.na(totVector)]

	# generate grids
	expect_silent(one <- hexagrid(tessVector, sf=FALSE))

	# should be the same as this
	expect_equal(rownames(one@faces),faces(one))

}

# replacement should not be allowed!
expect_error(faces(one) <- paste("B", 1:nrow(one@faces)))
