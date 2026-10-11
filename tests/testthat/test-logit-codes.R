make_codes <- function(n = 600) {
  set.seed(1)
  ci <- cbind(sample(1:3, n, replace = TRUE), sample(1:2, n, replace = TRUE), sample(1:4, n, replace = TRUE))
  eta <- c(0, 0.8, -0.6)[ci[, 1]] + c(0, -1)[ci[, 2]] + c(0, 0.5, 1, -0.4)[ci[, 3]] - 0.3
  list(ci = ci, K = c(3L, 2L, 4L), y = rbinom(n, 1, plogis(eta)))
}
no_pairs <- matrix(integer(0), 0L, 2L)

test_that("fit_logit_codes agrees with glm on the corresponding factors", {
  d <- make_codes()
  rows <- rep(TRUE, 600)
  model <- fit_logit_codes(d$ci, d$K, d$y, rows, no_pairs)
  reference <- stats::glm(d$y ~ factor(d$ci[, 1]) + factor(d$ci[, 2]) + factor(d$ci[, 3]), family = stats::binomial)
  expect_equal(model$npar, length(stats::coef(reference)))
  expect_equal(model$loglik, as.numeric(stats::logLik(reference)), tolerance = 1e-4)
  expect_equal(logit_eta(model, d$ci), as.numeric(stats::predict(reference)), tolerance = 1e-3)
})

test_that("the fit only uses the rows in the mask", {
  d <- make_codes()
  rows <- seq_along(d$y) <= 400
  model <- fit_logit_codes(d$ci, d$K, d$y, rows, no_pairs)
  reference <- stats::glm(d$y[rows] ~ factor(d$ci[rows, 1]) + factor(d$ci[rows, 2]) + factor(d$ci[rows, 3]), family = stats::binomial)
  expect_equal(model$loglik, as.numeric(stats::logLik(reference)), tolerance = 1e-4)
})

test_that("interactions add (K_a - 1)(K_b - 1) coefficients and are looked up correctly", {
  d <- make_codes()
  rows <- rep(TRUE, 600)
  pairs <- matrix(c(1L, 3L), ncol = 2)
  model <- fit_logit_codes(d$ci, d$K, d$y, rows, pairs)
  no_interaction <- fit_logit_codes(d$ci, d$K, d$y, rows, no_pairs)
  expect_equal(model$npar - no_interaction$npar, (3 - 1) * (4 - 1))
  expect_gte(model$loglik, no_interaction$loglik - 1e-6)
  reference <- stats::glm(d$y ~ factor(d$ci[, 1]) * factor(d$ci[, 3]) + factor(d$ci[, 2]), family = stats::binomial)
  expect_equal(logit_eta(model, d$ci), as.numeric(stats::predict(reference)), tolerance = 1e-2)
  expect_equal(dim(model$gamma[[1]]), c(3, 4))
  expect_true(all(model$gamma[[1]][1, ] == 0) && all(model$gamma[[1]][, 1] == 0))
})

test_that("features with a single value do not enter the model", {
  d <- make_codes()
  d$ci[, 2] <- 1L
  d$K[2] <- 1L
  model <- fit_logit_codes(d$ci, d$K, d$y, rep(TRUE, 600), no_pairs)
  expect_equal(model$npar, 1 + (3 - 1) + (4 - 1))
  expect_equal(model$beta[[2]], 0)
})

test_that("feature_contribution reproduces the linear predictor when the value changes", {
  d <- make_codes()
  pairs <- matrix(c(1L, 3L), ncol = 2)
  model <- fit_logit_codes(d$ci, d$K, d$y, rep(TRUE, 600), pairs)
  eta <- logit_eta(model, d$ci)
  for (j in 1:3) {
    contribution <- feature_contribution(model, d$ci, j)
    expect_equal(dim(contribution), c(600, d$K[j]))
    for (k in seq_len(d$K[j])) {
      changed <- d$ci
      changed[, j] <- k
      expect_equal(eta - contribution[cbind(1:600, d$ci[, j])] + contribution[, k], logit_eta(model, changed))
    }
  }
})

test_that("selection_criterion evaluates the right set with the right formula", {
  d <- make_codes()
  train <- seq_along(d$y) <= 400
  validation <- !train
  model <- fit_logit_codes(d$ci, d$K, d$y, train, no_pairs)
  prob <- stats::plogis(logit_eta(model, d$ci))
  expect_equal(selection_criterion("gini", TRUE, model, d$ci, d$y, train, validation), normalizedGini(d$y[validation], prob[validation]))
  expect_equal(selection_criterion("gini", FALSE, model, d$ci, d$y, train, validation), normalizedGini(d$y[train], prob[train]))
  expect_equal(selection_criterion("aic", TRUE, model, d$ci, d$y, train, validation), bernoulli_loglik(d$y[validation], prob[validation]))
  expect_equal(selection_criterion("bic", TRUE, model, d$ci, d$y, train, validation), bernoulli_loglik(d$y[validation], prob[validation]))
  expect_equal(selection_criterion("aic", FALSE, model, d$ci, d$y, train, validation), 2 * model$loglik - 2 * model$npar)
  expect_equal(selection_criterion("bic", FALSE, model, d$ci, d$y, train, validation), 2 * model$loglik - log(400) * model$npar)
})
