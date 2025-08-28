# Test the interaction with the Paleomap plates
# This is a very good test set, because the static polygons
# sometimes include mixed feature collection types (i.e. lines
# and polygons).

library(icosa)
suppressPackageStartupMessages(library(rgplates))
library(tinytest)
suppressPackageStartupMessages(library(chronosphere))

# set up the working directory
# setwd("/mnt/sky/Dropbox/Software/icosa/")
setwd(wd)

# main switch to create reference output
update <- FALSE

# load the downloaded model
model <- chronosphere::fetch(src="paleomap",ser="model",
	datadir="data/chronosphere/",
	verbose=FALSE)

# the reconstruction ages
ages <- seq(540, 0, -5)

# the grid
suppressMessages(hex <- hexagrid(deg=5, sf=TRUE))

# turn off spherical geometry
suppressMessages(sf::sf_use_s2(FALSE))

# get the models
lPlates <- rgplates::reconstruct("static_polygons", age=ages, model=model)

for(i in 1:length(ages)){

	# the current age
	plates <- lPlates[[i]]

	# the occupied cellls
	expect_silent(occ <- occupied(hex, plates))

	# 1. create reference output: check with
	if(update){
		# create a png for this
		png(paste0("icosa/tests/_results/occupied/paleomap_static_polygons/", ages[i], ".png"), width=2000, height=1000)
			plot(hex, reset=FALSE, border=NA, col=NA)
			plot(plates$geometry, add=TRUE, border="blue", lwd=2)
			plot(hex, occ, col="#99000099", add=TRUE, border=NA)
			plot(hex, add=TRUE, border="gray", col=NA)
		dev.off()
		# and save the output
		saveRDS(occ, paste0("icosa/tests/_results/occupied/paleomap_static_polygons/", ages[i], ".rds"))
		# loop counter
		cat(i, "\r")
		flush.console()
	# otherwise: do a test on the results
	}else{
		reference <- readRDS(paste0("icosa/tests/_results/occupied/paleomap_static_polygons/", ages[i], ".rds"))
		expect_equal(occ, reference)
	}

}
