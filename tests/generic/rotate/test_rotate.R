expect_silent(ro <- rotate(gr))
expect_true(inherits(ro, "trigrid"))

# one thing
expect_error(ro <- rotate(gr, pi))

# rotate by pi
expect_silent(ro <- rotate(gr, c(pi, pi, pi)))
