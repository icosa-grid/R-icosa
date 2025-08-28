library(tinytest)
library(icosa)

diag <- FALSE
# diag <- TRUE

# working direcory
# wd <- file.path("/mnt/sky/Dropbox/Software/icosa")
setwd(wd)

# location of methods
genericloc <- "icosa/tests/_methods/generic/"

data(hexguide)
testTess <- c(1:5, 10, 20, 30)

# repeat for every grid pair
for(i in 1:length(testTess)){

	if(diag) message(i, "\n")
	# get tessellation vector
	tessel <- hexguide[testTess[i], 2:5]
	tessel<- tessel[!is.na(tessel)]

	#trigrid
	expect_silent(gr  <- trigrid(tessel))
	source(file.path(genericloc, "rotate/grid_rotate.R"), local=TRUE, print.eval=diag)

	# hexagrid
	expect_silent(gr  <- hexagrid(tessel))
	source(file.path(genericloc, "rotate/grid_rotate.R"), local=TRUE, print.eval=diag)

}
