# icosa loaded on its own"
library(icosa)

expect_silent(a<- hexagrid(4))

expect_silent({
	plot3d(a)
})

unloadNamespace("package:icosa")

#"plot3d still works after rgl is loaded"
library(icosa)
#devtools::reload("icosa")

# now load the rgl
library(rgl)
expect_silent(a<- hexagrid(4))

expect_silent({
	plot3d(a)
})

unloadNamespace("package:icosa")
unloadNamespace("package:rgl")


#plot3d works is icosa is loaded on top of rgl"
# now load the rgl
library(rgl)
library(icosa)
#devtools::reload("icosa")

expect_silent(a<- hexagrid(4))

expect_silent({
	plot3d(a)
})

close3d()
