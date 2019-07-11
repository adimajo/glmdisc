context("test-discretize-link")

test_that("discretize_link works with one-dimensional continuous data", {
     set.seed(1)
     x = matrix(runif(100), nrow = 100, ncol = 1)
     cuts = seq(0,1,length.out = 4)
     xd = as.numeric(cut(x,cuts))
     
     link = nnet::multinom(e ~ x, data=data.frame(e=xd,x=x), maxit=50)
     res = discretize_link(link,data.frame(x),3)
     expect_type(res, "character")
     expect_length(res, 100)
     expect_equal(nlevels(factor(res)), 3)
})

test_that("discretize_link works with one-dimensional categorical data", {
     set.seed(1)
     x = matrix(runif(100), nrow = 100, ncol = 1)
     cuts = seq(0,1,length.out = 4)
     xd = as.numeric(cut(x,cuts))
     link = nnet::multinom(e ~ x, data=data.frame(e=xd,x=x), maxit=50)
     
     res = discretize_link(link,data.frame(x),3)
     expect_type(res, "character")
     expect_length(res, 100)
     expect_equal(nlevels(factor(res)), 3)
})
