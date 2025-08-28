# Construct neighborhood data object for spatial autocorrelation assessment
# - face2nb

library(tinytest)
library(icosa)
suppressPackageStartupMessages(library(spdep))
suppressPackageStartupMessages(library(sf))

setwd(file.path(Sys.getenv("Software"), "/icosa"))

# location of methods
genericloc <- "icosa/tests/_methods/generic/"

# correct sf
suppressMessages(sf_use_s2(FALSE))

# trigrids
data(triguide)
guide <- triguide[1:10, ]

# for diagnositics
diag <- FALSE

for(i in 1:nrow(guide)){

	# get the relevant bits
	totVector <- as.numeric(guide[i, paste0("level", 1:4)])
	tessVector <- totVector[!is.na(totVector)]

	# generate grids
	# KNOWN issue: sf creation produces warnings with some trigrids
	one <- trigrid(tessVector, sf=TRUE)

	# the queen neighborhood
	queen <- TRUE

	# icosa function
	expect_silent(faceNB <- face2nb(one, queen=queen))

	# compare one to faceNB
	source(file.path(genericloc, "face2nb/test_face2nb_trigrid.R"), local=TRUE, print.eval=diag)

	# the rook neighborhood
	queen <- FALSE

	# icosa function
	expect_silent(faceNB <- face2nb(one, queen=queen))

	# compare one to faceNB
	source(file.path(genericloc, "face2nb/test_face2nb_trigrid.R"), local=TRUE, print.eval=diag)

}


# hexagrids
data(hexguide)
guide <- hexguide[1:10, ]

# run ht
for(i in 1:nrow(guide)){

	# get the relevant bits
	totVector <- as.numeric(guide[i, paste0("level", 1:4)])
	tessVector <- totVector[!is.na(totVector)]

	# generate grids
	expect_silent(one <- hexagrid(tessVector, sf=TRUE))

	# icosa function
	expect_silent(faceNB <- face2nb(one))

	# compare one to faceNB
	source(file.path(genericloc, "face2nb/test_face2nb_hexagrid.R"), local=TRUE, print.eval=diag)
}
