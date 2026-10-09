test_that("Gini works", {
  set.seed(1)
  expect_error(normalizedGini(c(1, 1, 1, 0, 0), c(0.7, 0.9, 0.5, 0.6)))
  expect_true(normalizedGini(c(1, 1, 1, 0, 0), c(0.7, 0.9, 0.5, 0.6, 0.3)) <= 1)
  expect_true(normalizedGini(c(1, 1, 1, 0, 0), c(0.7, 0.9, 0.5, 0.6, 0.3)) >= 0)
})
