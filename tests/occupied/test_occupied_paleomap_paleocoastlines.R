library(icosa)
suppressPackageStartupMessages(library(rgplates))
library(tinytest)
suppressPackageStartupMessages(library(chronosphere))

# set up the working directory
#setwd("/mnt/sky/Dropbox/Software/icosa/")
setwd(wd)

# main switch to create reference output
update <- FALSE

# load in prelim data (replace later)
paleocoastlines <- chronosphere::fetch(src="paleomap",ser="paleocoastlines",
	datadir="data/chronosphere/",
	verbose=FALSE)

# creata a grid
suppressMessages(hex <- hexagrid(deg=5, sf=TRUE))

# turn off spherical geometry
suppressMessages(sf::sf_use_s2(FALSE))

# the ages for this
ages <- rownames(paleocoastlines)


for(i in 1:length(ages)){
	# the margins
	suppressMessages(margin <- sf::st_union(paleocoastlines[ages[i],"margin"]))

	# the landmass
	suppressMessages(coast <- sf::st_union(paleocoastlines[ages[i],"coast"]))

	# the shelf
	suppressMessages(shelf <- sf::st_difference(margin, coast))

	# the occupied cellls
	expect_silent(occ <- occupied(hex, shelf))

	# 1. create reference output: check with
	if(update){
		# create a png for this
		png(paste0("icosa/tests/_results/occupied/paleomap_paleocoastlines/", ages[i], ".png"), width=2000, height=1000)
			plot(hex, reset=FALSE, border=NA, col=NA)
			plot(shelf, add=TRUE, border="blue", lwd=2)
			plot(hex, occ, col="#99000099", add=TRUE, border=NA)
			plot(hex, add=TRUE, border="gray", col=NA)
		dev.off()

		# and save the output
		saveRDS(occ, paste0("icosa/tests/_results/occupied/paleomap_paleocoastlines/", ages[i], ".rds"))

		# loop counter
		cat(i, "\r")
		flush.console()
	}else{
		reference <- readRDS(paste0("icosa/tests/_results/occupied/paleomap_paleocoastlines/", ages[i], ".rds"))
		expect_equal(occ, reference)

	}

}

