# guide
# lookup()
# type
# test the entries from the table

# based on edge length
act <- list()
targets <- rep(NA, nrow(guide))
actuals <- rep(NA, nrow(guide))
for(i in 1:nrow(guide)){
	if(i<nrow(guide)){
		expect_silent(act[[i]] <- lookup(guide[i, "meanEdgeLength_deg"], gr=type, verbose=FALSE, arg="deg"))
		targets[i] <- prod(act[[i]])
		actuals[i] <- prod(guide[i, "total"])
	}

	if(i==nrow(guide))	expect_error(act[[i]] <- lookup(guide[i, "meanEdgeLength_deg"], gr=type, verbose=FALSE, arg="deg"))

}

expect_equal(targets, actuals)

# based on spacing
act <- list()
targets <- rep(NA, nrow(guide))
actuals <- rep(NA, nrow(guide))
for(i in 1:nrow(guide)){
	# based on edge length
	if(i<nrow(guide)){
		expect_silent(act[[i]] <- lookup(guide[i, "meanSpacing_deg"], gr=type, verbose=FALSE, arg="spacing"))
		targets[i] <- prod(act[[i]])
		actuals[i] <- prod(guide[i, "total"])
	}

	if(i==nrow(guide))	expect_error(act[[i]] <- lookup(guide[i, "meanSpacing_deg"], gr=type, verbose=FALSE, arg="spacing"))

}

expect_equal(targets, actuals)
