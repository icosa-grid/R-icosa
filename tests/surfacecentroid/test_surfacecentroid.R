# Testing the surfacecentroid

# 2025-08-27
library(tinytest)
library(icosa)

# working dir
 wd <- file.path(Sys.getenv("Dropbox"), "Software/icosa")

setwd(wd)

# location of generic functions
genericloc <- "icosa/tests/_methods/generic/"

# diagnose
diag <- FALSE
#diag <- TRUE

################################################################################
# 1. basic testing - Structure
# create a couple of coordinates (original)
set.seed(0)
expect_silent(xyz <- rpsphere(300))
source(file.path(genericloc, "surfacecentroid/test_surfacecentroids.R"), local=TRUE, print.eval=diag)

# single row
xyz <- xyz[1, , drop=FALSE]
source(file.path(genericloc, "surfacecentroid/test_surfacecentroids.R"), local=TRUE, print.eval=diag)

# numeric vector
expect_error(surfacecentroid(xyz[1,]))

################################################################################
# 2. Conceptual testing

# generate 100000 points
set.seed(0)
expect_silent(xyz <- rpsphere(10000))

# create a grid
expect_silent(hex <- hexagrid(2))

# locate cells
expect_silent(cells <- locate(hex, xyz))

# iterate through the cells

sampled <- sort(unique(cells))

for(i in 1:length(sampled)){
	# grab those that come from one cell
	thisCell <- xyz[cells==sampled[i], ]

	# calculate the centroid
	expect_silent(thisCentroid <- surfacecentroid(thisCell))

	# which cell does this fall on?
	expect_silent(thisCentroidCell <- locate(hex,thisCentroid ))

	# Becuase the cells are convex, centroids defined by their cells MUST be on the cell
	expect_equal(thisCentroidCell, sampled[i])

}


################################################################################
# 3. Random weights

trials <- 100
focal <- "F8"

# store these for assessment
allCentroids <- matrix(NA, ncol=2, nrow=trials)

for(i in 1:100){
	set.seed(i)

	# grab those that come from one cell
	thisCell <- xyz[cells==focal, ]

	# also applies to weighting (applying random weights!)
	expect_silent(thisCentroidW <- surfacecentroid(thisCell, w = runif(nrow(thisCell))))

	# which cell does this fall on?
	expect_silent(thisCentroidCell <- locate(hex,thisCentroidW ))

	# Becuase the cells are convex, centroids defined by their cells MUST be on the cell
	# EVEN WHEN weighted differently!
	expect_equal(thisCentroidCell, focal)

	# store
	allCentroids[i, ] <- thisCentroidW
}

################################################################################
# 4. directed weights (specific check)
three <- matrix(
	c(
		-36, 42,
		81, 28,
		-12, -31
	),
	ncol=2, byrow=TRUE
)

expect_silent(xyz <- PolToCar(three))
source(file.path(genericloc, "surfacecentroid/test_surfacecentroids.R"), local=TRUE, print.eval=diag)

# check progressibev weighting towards point 3
expect_silent(cent <- surfacecentroid(three))
expect_silent(cent2 <- surfacecentroid(three, w=c(1,1,2)))
expect_silent(cent4 <- surfacecentroid(three, w=c(1,1,4)))
expect_silent(cent8 <- surfacecentroid(three, w=c(1,1,8)))


## # checking plot
## plot(three)
## text(three, label=1:3, pos=2)

## points(x=cent[1], y=cent[2], pch=3, col="blue", lwd=3)
## text(x=cent[1], y=cent[2], label="Centroid", col="blue", pos=2)

## points(x=cent2[1], y=cent2[2], pch=3, col="red", lwd=3)
## text(x=cent2[1], y=cent2[2], label="x2", col="red", pos=2)
## points(x=cent4[1], y=cent4[2], pch=3, col="red", lwd=3)
## text(x=cent4[1], y=cent4[2], label="x4", col="red", pos=2)
## points(x=cent8[1], y=cent8[2], pch=3, col="red", lwd=3)
## text(x=cent8[1], y=cent8[2], label="x8", col="red", pos=2)
