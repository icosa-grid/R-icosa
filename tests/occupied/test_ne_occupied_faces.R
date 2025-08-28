library(icosa)
library(tinytest)
suppressPackageStartupMessages(library(sf))

diag <- FALSE
setwd(wd)

# location of methods
genericloc <- "icosa/tests/_methods/generic/"

# read in data
ne <- st_read(file.path(system.file(package="icosa"),"extdata/ne_110m_land.shx" ), quiet=TRUE)

# sf object
sf <- ne

# spdf
spdf <- as(ne,"Spatial")

# sp
sp<- methods::as(spdf,"SpatialPolygons")

# triangular coarse
expect_silent(gr <- trigrid(4))
sol <- 191 # correct solution (number of occurpied cells) based on visual check!
source(file.path(genericloc,"occupied/test_occupied_vector.R"), local=TRUE, print.eval=diag)

# hexagonal fine grid
expect_silent(gr <- hexagrid(c(8,10)))
sol <- 20652 # correct solution (number of occurpied cells) based on visual check!
source(file.path(genericloc,"occupied/test_occupied_vector.R"), local=TRUE, print.eval=diag)
