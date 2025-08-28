library(tinytest)
library(icosa)

# working direcory
# wd <- file.path("/mnt/sky/Dropbox/Software/icosa")
setwd(wd)

# location of methods
genericloc <- "icosa/tests/_methods/generic/"

diag <- FALSE

# arcs()
# single point - basics expeted
set.seed(1)
expect_silent(ps <- rpsphere(1, output="polar"))
source(file.path(genericloc, "arcs/arcs.R"), local=TRUE, print.eval=diag)

# two points
set.seed(1)
expect_silent(ps <- rpsphere(2, output="polar"))
source(file.path(genericloc, "arcs/arcs.R"), local=TRUE, print.eval=diag)

set.seed(1)
np<- ceiling(sort( runif(100, 1, 100)))

for(i in 1:length(np)){
	# plotting them
	set.seed(i)
	expect_silent(ps <- rpsphere(np[i], output="polar"))
	source(file.path(genericloc, "arcs/arcs.R"), local=TRUE, print.eval=diag)
}
