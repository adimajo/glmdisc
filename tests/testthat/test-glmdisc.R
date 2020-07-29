context("test-glmdisc")

test_that("glmdisc errors for type of input data", {
     expect_error(glmdisc("toto", "tztz", iter = 50, m_start = 4, test = FALSE, validation = FALSE, criterion = "aic"))
})
