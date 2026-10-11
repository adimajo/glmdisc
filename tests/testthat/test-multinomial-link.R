set.seed(1)
n <- 1500
x <- runif(n)
classes <- as.integer(cut(x + rnorm(n, 0, 0.12), c(-Inf, 0.25, 0.5, 0.75, Inf)))
z <- (x - mean(x)) / sd(x)
start <- matrix(0, 2, 4)

test_that("the Newton solver reaches the likelihood of nnet::multinom", {
  skip_if_not_installed("nnet")
  fit <- fit_multinomial_1d(z, classes - 1L, 4L, start, ridge = 0.001, maxit = 100L, tol = 1e-9)
  expect_true(fit$converged)
  probs <- multinomial_1d_probs(z, fit$coef)
  expect_equal(unname(rowSums(probs)), rep(1, n))
  reference <- suppressWarnings(nnet::multinom(factor(classes) ~ z, trace = FALSE, maxit = 1000, reltol = 1e-12))
  reference_probs <- stats::predict(reference, type = "probs")
  loglik <- function(p) sum(log(p[cbind(seq_len(n), classes)]))
  expect_equal(loglik(probs), loglik(reference_probs), tolerance = 1e-3)
  expect_lt(max(abs(probs - reference_probs)), 0.01)
})

test_that("a warm start needs fewer iterations", {
  cold <- fit_multinomial_1d(z, classes - 1L, 4L, start, ridge = 0.1, maxit = 30L, tol = 1e-6)
  warm <- fit_multinomial_1d(z, classes - 1L, 4L, cold$coef, ridge = 0.1, maxit = 30L, tol = 1e-6)
  expect_lt(warm$iter, cold$iter)
  expect_equal(warm$coef, cold$coef, tolerance = 1e-4)
})

test_that("the ridge penalty keeps the coefficients finite for separated classes", {
  separated <- sort(runif(300))
  groups <- findInterval(separated, c(1 / 3, 2 / 3))
  fit <- fit_multinomial_1d((separated - mean(separated)) / sd(separated), groups, 3L, matrix(0, 2, 3), ridge = 0.1, maxit = 30L, tol = 1e-6)
  expect_true(all(is.finite(fit$coef)))
  expect_lt(max(abs(fit$coef)), 100)
})

test_that("the solver validates its inputs", {
  expect_error(fit_multinomial_1d(z, classes - 1L, 1L, matrix(0, 2, 1)), "two classes")
  expect_error(fit_multinomial_1d(z, classes, 4L, start), "values in 0..K-1")
  expect_error(fit_multinomial_1d(z, classes - 1L, 4L, matrix(0, 2, 3)), "2 x K")
  expect_error(fit_multinomial_1d(z[-1], classes - 1L, 4L, start), "same length")
})
