test_that("a numeric link recovers the intervals it was fitted on", {
  set.seed(1)
  x <- runif(300)
  e <- as.integer(cut(x, seq(0, 1, length.out = 4)))
  link <- fit_numeric_link(x, e, rep(TRUE, 300), 1:3, na_label = 99L)
  link$S <- 1:3
  P <- link_probs(link, x)
  expect_equal(dim(P), c(300, 3))
  expect_equal(colnames(P), c("1", "2", "3"))
  expect_equal(unname(rowSums(P)), rep(1, 300))
  expect_gt(mean(link_map(link, x) == e), 0.9)
})

test_that("a numeric link can be fitted by ordered logit, and warm-started", {
  set.seed(1)
  x <- runif(360)
  # noisy labels: with perfectly separated classes polr cannot be fitted
  e <- as.integer(cut(x + rnorm(360, sd = 0.1), c(-Inf, 1 / 3, 2 / 3, Inf)))
  train <- rep(TRUE, 360)
  ordered <- fit_numeric_link(x, e, train, 1:3, na_label = 99L, reg_type = "polr")
  expect_equal(ordered$reg, "polr")
  ordered$S <- 1:3
  expect_equal(unname(rowSums(link_probs(ordered, x))), rep(1, 360))
  expect_equal(sort(unique(link_map(ordered, x))), 1:3)

  poly <- fit_numeric_link(x, e, train, 1:3, na_label = 99L)
  again <- fit_numeric_link(x, e, train, 1:3, na_label = 99L, previous = poly)
  expect_equal(again$coef, poly$coef, tolerance = 1e-3)
})

test_that("polr falls back to the multinomial link for perfectly separated classes", {
  x <- seq(0, 1, length.out = 90)
  e <- rep(1:3, each = 30)
  link <- fit_numeric_link(x, e, rep(TRUE, 90), 1:3, na_label = 99L, reg_type = "polr")
  expect_equal(link$reg, "poly")
})

test_that("missing values of a numeric feature get their own label", {
  set.seed(1)
  x <- runif(200)
  x[1:20] <- NA
  e <- ifelse(is.na(x), 99L, as.integer(cut(x, c(0, .5, 1))))
  link <- fit_numeric_link(x, e, rep(TRUE, 200), c(1L, 2L, 99L), na_label = 99L)
  P <- link_probs(link, x)
  expect_equal(unname(P[1:20, 3]), rep(1, 20))
  expect_equal(unname(P[21:200, 3]), rep(0, 180))
})

test_that("missing values are spread over all labels when no training row has one", {
  set.seed(1)
  x <- runif(100)
  e <- as.integer(cut(x, c(0, .5, 1)))
  link <- fit_numeric_link(x, e, rep(TRUE, 100), 1:2, na_label = 99L)
  expect_equal(unname(link_probs(link, NA_real_)[1, ]), c(0.5, 0.5))
})

test_that("a factor link is the contingency table of the quantized and the raw values", {
  set.seed(1)
  x <- sample(1:4, 200, replace = TRUE)
  e <- ifelse(x %in% 1:2, 1L, 2L)
  link <- fit_factor_link(x, e, rep(TRUE, 200), 1:2, 4L, as.character(1:4))
  P <- link_probs(link, x)
  expect_equal(unname(P[, 1]), as.numeric(x %in% 1:2))
  link$S <- 1:2
  expect_equal(link_map(link, x), e)
})

test_that("a level of the raw factor that was never seen in training gets uniform probabilities", {
  link <- fit_factor_link(c(1L, 1L, 2L, 2L), c(1L, 1L, 2L, 2L), rep(TRUE, 4), 1:2, 3L, c("a", "b", "c"))
  expect_equal(unname(link_probs(link, 3L)[1, ]), c(0.5, 0.5))
})

test_that("a numeric link with a single class has probability one", {
  link <- fit_numeric_link(runif(10), rep(2L, 10), rep(TRUE, 10), 2L, na_label = 99L)
  expect_equal(unname(link_probs(link, c(0.1, 0.9))[, 1]), c(1, 1))
})

test_that("constant_link outputs its only label", {
  expect_equal(constant_link(3L)$S, 3L)
})
