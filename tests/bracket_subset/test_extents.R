# read packages
library(tinytest)
library(icosa)
suppressPackageStartupMessages(library(terra))

# working direcory
# wd <- file.path("/mnt/sky/Dropbox/Software/icosa")
setwd(wd)
out <- NULL
gr <- NULL

# location of methods
genericloc <- "icosa/tests/_methods/generic/"

diag <- FALSE
# 80 valid subsets

if(diag )message("\nTrigrid tests\n")

# 1.Trigrid
expect_silent(gr <- trigrid(10))

# cross check
x <- seq(-180, 180, 30)
y <- seq(-90, 90, 30)

# xmin
for(i in 1:(length(x)-1)){
	# xmax
	for(j in (i+1):length(x)){
		# ymin
		for(k in 1:(length(y)-1)){
			# ymin
			for(l in (k+1):(length(y))){
#				message("xmin: ", x[i], " xmax: ", x[j], " ymin: ", y[k], " ymax: ", y[l])

				# create subscript extent file
				subscript<- ext(c(xmin=x[i], xmax=x[j], ymin=y[k], ymax=y[l]))

				# create facelayer with values
				expect_silent(fl <- facelayer(gr))
				expect_silent(values(fl) <- 1:length(gr))

				# execut subset
				expect_silent(out <- fl[subscript])

				if(length(out)>0){
					# test of basic output
					source(file.path(genericloc, "bracket_subset/test_facelayer_bracket.R"), local=TRUE, print.eval=diag)

					# correct subsetting
					expect_silent(outNames <- names(out))
					expect_silent(cent <- centers(gr))
					# ... based on the face centers
					subCenters <- cent[outNames, , drop=FALSE]

					# longitude
					minX <- min(subCenters[,1])
					maxX <- max(subCenters[,1])

					# latitude
					minY <- min(subCenters[,2])
					maxY <- max(subCenters[,2])

					expect_true(subscript[1]<=minX)
					expect_true(subscript[2]>=maxX)
					expect_true(subscript[3]<=minY)
					expect_true(subscript[4]>=maxY)

					if(!(subscript[2] >= maxX)){
						stop("error!")

					}
				 }
			}

		}

	}
}

if(diag) message("\nHexagrid tests\n")

# 2. Hexagrid
rm(fl)
expect_silent(gr <- hexagrid(10))

# cross check
x <- seq(-180, 180, 30)
y <- seq(-90, 90, 30)

# xmin
for(i in 1:(length(x)-1)){
	# xmax
	for(j in (i+1):length(x)){
		# ymin
		for(k in 1:(length(y)-1)){
			# ymin
			for(l in (k+1):(length(y))){
#				message("xmin: ", x[i], " xmax: ", x[j], " ymin: ", y[k], " ymax: ", y[l])

				# create subscript extent file
				subscript<- ext(c(xmin=x[i], xmax=x[j], ymin=y[k], ymax=y[l]))

				# create facelayer with values
				expect_silent(fl <- facelayer(gr))
				expect_silent(values(fl) <- 1:length(gr))

				# execut subset
				expect_silent(out <- fl[subscript])

				if(length(out)>0){
					# test of basic output
					source(file.path(genericloc, "bracket_subset/test_facelayer_bracket.R"), local=TRUE, print.eval=diag)

					# correct subsetting
					expect_silent(outNames <- names(out))
					expect_silent(cent <- centers(gr))
					# ... based on the face centers
					subCenters <- cent[outNames, , drop=FALSE]

					# longitude
					minX <- min(subCenters[,1])
					maxX <- max(subCenters[,1])

					# latitude
					minY <- min(subCenters[,2])
					maxY <- max(subCenters[,2])

					expect_true(subscript[1]<=minX)
					expect_true(subscript[2]>=maxX)
					expect_true(subscript[3]<=minY)
					expect_true(subscript[4]>=maxY)
				 }
			}

		}

	}
}
