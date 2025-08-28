library(tinytest)

# icosa-first
expect_silent({
	library(icosa)
	library(rgl)
})

expect_silent(hexagrid(1))

unloadNamespace("package:rgl")
unloadNamespace("package:icosa")
