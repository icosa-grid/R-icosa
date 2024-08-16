test_that("icosa loaded on its own",{
	library(icosa)
	devtools::reload("icosa")

	a<- hexagrid(4)

	expect_silent({
		plot3d(a)
	})
})

detach("package:icosa", unload=TRUE) 

test_that("plot3d still works after rgl is loaded",{
	library(icosa)
	devtools::reload("icosa")
	
	# now load the rgl
	library(rgl)
	a<- hexagrid(4)

	expect_silent({
		plot3d(a)
	})
})

detach("package:icosa", unload=TRUE) 
detach("package:rgl", unload=TRUE) 


test_that("plot3d works is icosa is loaded on top of rgl",{
	# now load the rgl
	library(rgl)
	library(icosa)
	devtools::reload("icosa")
	
	a<- hexagrid(4)

	expect_silent({
		plot3d(a)
	})
})

