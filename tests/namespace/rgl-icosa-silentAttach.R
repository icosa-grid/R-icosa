library(testthat)

context("two-way loading with rgl")
expect_silent({
	library(icosa)
	library(rgl)
})



detach("package:rgl", unload=TRUE) 
detach("package:icosa", unload=TRUE) 


expect_silent({
	library(rgl)
	library(icosa)
})

detach("package:rgl", unload=TRUE) 
detach("package:icosa", unload=TRUE) 
