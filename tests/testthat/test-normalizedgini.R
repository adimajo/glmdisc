test_that("Gini works", {
  set.seed(1)
  expect_error(normalizedGini(c(1, 1, 1, 0, 0), c(0.7, 0.9, 0.5, 0.6)))
  expect_equal(normalizedGini(c(1, 1, 1, 0, 0), c(0.7, 0.9, 0.5, 0.6, 0.3)), 2 * 5 / 6 - 1)
})

test_that("Gini is 1 for a perfect, -1 for an inverted and 0 for a constant score", {
  set.seed(1)
  y <- c(1, 0, 1, 0, 0, 1)
  expect_equal(normalizedGini(y, y), 1)
  expect_equal(normalizedGini(y, 1 - y), -1)
  expect_equal(normalizedGini(y, rep(0.3, 6)), 0)
})

test_that("Gini does not depend on the order of the observations when scores tie", {
  set.seed(1)
  y <- rbinom(300, 1, 0.3)
  p <- round(runif(300), 1)
  o <- sample(300)
  expect_equal(normalizedGini(y, p), normalizedGini(y[o], p[o]))
})

test_that("Gini equals 2 AUC - 1", {
  set.seed(2)
  y <- rbinom(200, 1, 0.4)
  p <- runif(200) + 0.3 * y
  auc <- mean(outer(p[y == 1], p[y == 0], ">") + 0.5 * outer(p[y == 1], p[y == 0], "=="))
  expect_equal(normalizedGini(y, p), 2 * auc - 1)
})
