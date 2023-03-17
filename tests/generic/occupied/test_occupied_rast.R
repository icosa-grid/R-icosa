expect_silent(oc <- occupied(gr,r))
expect_true(inherits(oc, "logical"))
expect_equal(sol, sum(oc))


expect_silent(fl <- occupied(gr,r, out="facelayer"))
expect_true(inherits(fl, "facelayer"))
expect_equal(sol, sum(fl))

#plot(r, col="red")
#plot(fl, add=TRUE, col="#00990044", border="black")

