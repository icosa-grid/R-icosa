library(tinytest)

# rgl-first
expect_silent({
	library(rgl)
	library(icosa)
})

expect_silent(hexagrid(tessellation=1))

unloadNamespace("package:rgl")
unloadNamespace("package:icosa")
