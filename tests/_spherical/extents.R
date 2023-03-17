library(terra)
library(icosa)
library(tinytest)

subscript<- ext(c(xmin=-30, xmax=30, ymin=-30, ymax=30))

gr <- hexagrid(10)

thing <- facelayer(gr)
values(thing) <- 1:length(gr)

run_test_file("tests/generic/bracket_subset/test_bracket.R", reporter=SummaryReporter)

