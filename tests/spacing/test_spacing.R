library(tinytest)
library(icosa)

# working direcory
# wd <- file.path("/mnt/sky/Dropbox/Software/icosa")
setwd(wd)

# location of methods
genericloc <- "icosa/tests/_methods/generic/"

# grab 10 random tessellation vectors

data(hexguide)
testTess <- c(1:5, 10, 20, 30)


# repeat for every grid pair
for(i in 1:length(testTess)){
	# get tessellation vector
	tessel <- hexguide[testTess[i], 2:5]
	tessel<- tessel[!is.na(tessel)]

	#trigrid
	expect_silent(gr  <- trigrid(tessel))
	source(file.path(genericloc, "spacing/spacing.R"), local=TRUE)

	# hexagrid
	expect_silent(gr  <- hexagrid(tessel))
	source(file.path(genericloc, "spacing/spacing.R"), local=TRUE)

}

