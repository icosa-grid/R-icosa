expect_silent(re <- resample(r, gr))
expect_equal(sum(re), sol)


## fl<-facelayer(gr)
## fl[] <-re
