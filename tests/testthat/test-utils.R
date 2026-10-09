test_that("normalize_rows normalizes and handles all-zero rows", {
  set.seed(1)
  expect_equal(normalize_rows(matrix(0, ncol = 3, nrow = 1)), matrix(1 / 3, ncol = 3, nrow = 1))
  expect_equal(normalize_rows(matrix(c(1, 3), nrow = 1)), matrix(c(0.25, 0.75), nrow = 1))
})

test_that("drop_interaction_terms matches whole variable names only", {
  set.seed(1)
  terms <- c("V1:V2", "V1:V10", "V10:V12", "V3:V4")
  expect_equal(drop_interaction_terms(terms, "V1"), c("V10:V12", "V3:V4"))
  expect_equal(drop_interaction_terms(terms, c("V10", "V3")), "V1:V2")
  expect_equal(drop_interaction_terms(terms, character(0)), terms)
  expect_equal(drop_interaction_terms(character(0), "V1"), character(0))
})

test_that("bernoulli_loglik counts both classes", {
  set.seed(1)
  expect_equal(bernoulli_loglik(c(1, 0), c(0.8, 0.3)), log(0.8) + log(0.7))
  # a model that is confidently wrong on negatives must be penalized
  expect_lt(bernoulli_loglik(c(1, 0), c(0.8, 0.99)), bernoulli_loglik(c(1, 0), c(0.8, 0.01)))
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

test_that("resample_unseen_labels only produces labels seen in the training rows", {
  set.seed(1)
  labels <- c(1L, 2L, 3L)
  current <- c(1L, 1L, 2L, 3L, 3L, 2L)
  train <- c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
  prob <- matrix(c(0.1, 0.1, 0.8), nrow = 6, ncol = 3, byrow = TRUE)
  for (how in c("sample", "map")) {
    res <- resample_unseen_labels(current, prob, labels, train, how)
    expect_type(res, "integer")
    expect_false(any(res[!train] == 3L))
    # rows that were already fine are untouched
    expect_equal(res[current != 3L], current[current != 3L])
  }
  # the most likely remaining label is picked by the MAP
  expect_equal(resample_unseen_labels(current, prob, labels, train, "map")[4:5], c(1L, 1L))
  # nothing to do: same object back
  expect_identical(resample_unseen_labels(c(1L, 2L), prob[1:2, ], labels, c(TRUE, TRUE), "map"), c(1L, 2L))
})

test_that("resample_unseen_labels handles rows without any mass on seen labels", {
  labels <- c(1L, 2L)
  prob <- matrix(c(0, 1), nrow = 2, ncol = 2, byrow = TRUE)
  res <- resample_unseen_labels(c(1L, 2L), prob, labels, c(TRUE, FALSE), "map")
  expect_equal(res, c(1L, 1L))
  res <- resample_unseen_labels(c(1L, 2L), prob, labels, c(TRUE, FALSE), "sample")
  expect_equal(res, c(1L, 1L))
})
