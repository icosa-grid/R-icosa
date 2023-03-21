# guide
# lookup()
# type
# test the entries from the table

act <- list()
targets <- rep(NA, nrow(guide))
actuals <- rep(NA, nrow(guide))
for(i in 1:nrow(guide)){
	if(i<nrow(guide)){
		expect_silent(act[[i]] <- lookup(guide[i, "meanEdgeLength_deg"], gr=type, verbose=FALSE))
		targets[i] <- prod(act[[i]])
		actuals[i] <- prod(guide[i, "total"])
	}

	if(i==nrow(guide))	expect_error(act[[i]] <- lookup(guide[i, "meanEdgeLength_deg"], gr=type, verbose=FALSE))

}

expect_equal(targets, actuals)
