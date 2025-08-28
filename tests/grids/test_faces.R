library(tinytest)
library(icosa)

diag <- FALSE
# diag <- TRUE

# working direcory
# wd <- file.path("/mnt/sky/Dropbox/Software/icosa")
setwd(wd)

# location of methods
genericloc <- "icosa/tests/_methods/generic/"


# Intact grids

# hexagrid
data(hexguide)
guide <- hexguide[1:10, ]

for(i in 1:nrow(guide)){
	if(diag) message(i, "\n")
	# get tessellation vector
	totVector <- as.numeric(guide[i, paste0("level", 1:4)])
	tessVector <- totVector[!is.na(totVector)]

	# generate grids
	expect_silent(one <- hexagrid(tessVector, sf=FALSE))

	# actual tests
	source(file.path(genericloc,"grids/faces.R"), local=TRUE, print.eval=diag)
}


# Intact grids
# trigrids
data(triguide)
guide <- triguide[1:10, ]

for(i in 1:nrow(guide)){
	if(diag) message(i, "\n")
	# get tessellation vector
	totVector <- as.numeric(guide[i, paste0("level", 1:4)])
	tessVector <- totVector[!is.na(totVector)]

	# generate grids
	expect_silent(one <- trigrid(tessVector, sf=FALSE))

	# actual tests
	source(file.path(genericloc,"grids/faces.R"), local=TRUE, print.eval=diag)
}
