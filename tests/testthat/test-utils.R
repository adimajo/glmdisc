test_that("normalize_rows normalizes and handles all-zero rows", {
  set.seed(1)
  expect_equal(normalize_rows(matrix(0, ncol = 3, nrow = 1)), matrix(1 / 3, ncol = 3, nrow = 1))
  expect_equal(normalize_rows(matrix(c(1, 3), nrow = 1)), matrix(c(0.25, 0.75), nrow = 1))
})

test_that("sample_rows follows the row probabilities", {
  set.seed(1)
  p <- matrix(rep(c(0.1, 0.6, 0.3), each = 20000), ncol = 3)
  draws <- sample_rows(p)
  expect_equal(as.numeric(table(factor(draws, 1:3))) / 20000, c(0.1, 0.6, 0.3), tolerance = 0.02)
  # deterministic rows
  expect_equal(sample_rows(rbind(c(0, 1, 0), c(0, 0, 1), c(1, 0, 0))), c(2L, 3L, 1L))
  # unnormalized rows are accepted
  expect_equal(sample_rows(rbind(c(0, 5, 0))), 2L)
})

test_that("resample_unseen_levels only produces levels seen in the training rows", {
  set.seed(1)
  levs <- c("1", "2", "3")
  current <- factor(c("1", "1", "2", "3", "3", "2"), levels = levs)
  train <- c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
  target <- !train
  prob <- matrix(c(0.1, 0.1, 0.8), nrow = 6, ncol = 3, byrow = TRUE, dimnames = list(NULL, levs))
  for (how in c("sample", "map")) {
    res <- resample_unseen_levels(current, prob, train, target, how)
    expect_s3_class(res, "factor")
    expect_equal(levels(res), levs)
    expect_false(any(as.character(res)[target] == "3"))
    # untouched rows stay as they were
    expect_equal(as.character(res)[!target | as.character(current) != "3"], as.character(current)[!target | as.character(current) != "3"])
  }
  # most likely remaining level is picked for MAP
  expect_equal(as.character(resample_unseen_levels(current, prob, train, target, "map"))[4:5], c("1", "1"))
})

test_that("resample_unseen_levels works when the probabilities put no mass on seen levels", {
  set.seed(1)
  levs <- c("1", "2")
  current <- factor(c("1", "2"), levels = levs)
  prob <- matrix(c(0, 1), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(NULL, levs))
  res <- resample_unseen_levels(current, prob, c(TRUE, FALSE), c(FALSE, TRUE), "map")
  expect_equal(as.character(res), c("1", "1"))
})

test_that("bernoulli_loglik counts both classes", {
  set.seed(1)
  expect_equal(bernoulli_loglik(c(1, 0), c(0.8, 0.3)), log(0.8) + log(0.7))
  # a model that is confidently wrong on negatives must be penalized
  expect_lt(bernoulli_loglik(c(1, 0), c(0.8, 0.99)), bernoulli_loglik(c(1, 0), c(0.8, 0.01)))
})
