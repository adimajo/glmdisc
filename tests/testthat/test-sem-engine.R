simulate_intervals <- function(n, d = 3, theta = c(-1.5, 0.5, 1.5), offset = -1.5) {
  x <- matrix(runif(n * d), n, d)
  xd <- apply(x, 2, function(column) as.integer(cut(column, c(0, 1 / 3, 2 / 3, 1))))
  y <- rbinom(n, 1, plogis(rowSums(sapply(seq_len(d), function(j) theta[xd[, j]])) + offset))
  list(x = x, y = y)
}

test_that("the chain keeps iterating instead of stopping after the first sweep", {
  # Regression test: after the first sweep the quantized values used to be character
  # columns with 0 levels, every feature was declared "disappeared" and the run ended
  # at iteration 2, returning a random quantization.
  set.seed(2)
  data <- simulate_intervals(600)
  fit <- glmdisc(data$x, data$y, iter = 30, m_start = 6, interact = FALSE)
  expect_length(fit@performance$criterionEvolution, 30)
  levels_per_feature <- vapply(fit@best.disc$bestLinkFunction, function(link) length(link$S), integer(1))
  expect_true(all(levels_per_feature >= 2))
})

test_that("a planted discretization is approximately recovered", {
  set.seed(2)
  data <- simulate_intervals(1500)
  fit <- glmdisc(data$x, data$y, iter = 150, m_start = 8, interact = FALSE)
  expect_gt(fit@performance$performance, 0.5)
  for (cuts in cutpoints(fit)) {
    expect_true(any(abs(cuts - 1 / 3) < 0.1))
    expect_true(any(abs(cuts - 2 / 3) < 0.1))
  }
})

test_that("interactions are found when the effect is purely an interaction", {
  set.seed(1)
  n <- 2000
  x <- matrix(runif(n * 3), n, 3)
  y <- rbinom(n, 1, plogis(2.5 * ((x[, 1] > 0.5) != (x[, 2] > 0.5)) - 1.25))
  without <- glmdisc(x, y, iter = 150, m_start = 4, interact = FALSE)
  with <- glmdisc(x, y, iter = 150, m_start = 4, interact = TRUE)
  expect_gt(with@performance$performance, without@performance$performance + 0.2)
  expect_true(any(grepl(":", names(stats::coef(with@best.disc[[1]])))))
})

test_that("missing values of a numeric feature are a level of their own", {
  set.seed(1)
  n <- 1200
  x <- runif(n)
  missing <- rbinom(n, 1, 0.2) == 1
  y <- rbinom(n, 1, plogis(ifelse(missing, 2, ifelse(x > 0.5, 1, -1))))
  x[missing] <- NA
  fit <- glmdisc(data.frame(x = x, z = runif(n)), y, iter = 80, m_start = 5, interact = FALSE)
  expect_true("NA" %in% levels(fit@disc.data$x))
  expect_gt(mean(fit@disc.data$labels[fit@disc.data$x == "NA"]), 0.7)
  # predictions are available for observations with a missing value
  new <- data.frame(x = c(NA, 0.9, 0.1), z = 0.5)
  p <- predict(fit, new)
  expect_length(p, 3)
  expect_gt(p[1], p[3])
})

test_that("every combination of test / validation / criterion runs and gives a consistent object", {
  set.seed(3)
  data <- simulate_intervals(500)
  for (test in c(TRUE, FALSE)) {
    for (validation in c(TRUE, FALSE)) {
      for (criterion in c("gini", "aic", "bic")) {
        fit <- glmdisc(data$x, data$y, iter = 8, m_start = 5, test = test, validation = validation, criterion = criterion, interact = FALSE)
        expect_s4_class(fit, "glmdisc")
        expect_equal(nrow(fit@disc.data), if (test || validation) 100 else floor(500 * 1))
        expect_equal(nrow(fit@disc.data), nrow(fit@cont.data))
        expect_true(is.finite(fit@performance$performance))
      }
    }
  }
})

test_that("criteria are computed on the right set", {
  set.seed(4)
  data <- simulate_intervals(400)
  # with a validation set, aic and bic are the (unpenalized) validation log-likelihood: identical
  set.seed(10)
  a <- glmdisc(data$x, data$y, iter = 5, m_start = 4, test = FALSE, validation = TRUE, criterion = "aic", interact = FALSE)
  set.seed(10)
  b <- glmdisc(data$x, data$y, iter = 5, m_start = 4, test = FALSE, validation = TRUE, criterion = "bic", interact = FALSE)
  expect_equal(unlist(a@performance$criterionEvolution), unlist(b@performance$criterionEvolution))
  expect_true(all(unlist(a@performance$criterionEvolution) < 0))
  # without validation set the BIC penalizes more than the AIC
  set.seed(10)
  a <- glmdisc(data$x, data$y, iter = 5, m_start = 4, test = FALSE, validation = FALSE, criterion = "aic", interact = FALSE)
  set.seed(10)
  b <- glmdisc(data$x, data$y, iter = 5, m_start = 4, test = FALSE, validation = FALSE, criterion = "bic", interact = FALSE)
  expect_true(all(unlist(b@performance$criterionEvolution) < unlist(a@performance$criterionEvolution)))
})

test_that("results are reproducible with the same seed and differ across seeds", {
  data <- simulate_intervals(300)
  run <- function(seed) {
    set.seed(seed)
    glmdisc(data$x, data$y, iter = 10, m_start = 4, interact = FALSE)@performance$criterionEvolution
  }
  expect_equal(run(1), run(1))
  expect_false(isTRUE(all.equal(run(1), run(2))))
})

test_that("factor features are supported, including unseen levels at prediction time", {
  set.seed(1)
  n <- 600
  f <- sample(letters[1:5], n, replace = TRUE)
  y <- rbinom(n, 1, plogis(ifelse(f %in% c("a", "b"), -1, 1)))
  fit <- glmdisc(data.frame(f = factor(f), x = runif(n)), y, iter = 30, m_start = 5, interact = FALSE)
  expect_equal(names(cutpoints(fit)), c("f", "x"))
  expect_equal(names(cutpoints(fit)$f), letters[1:5])
  expect_error(predict(fit, data.frame(f = factor("z"), x = 0.5)), "Unseen")
  expect_length(predict(fit, data.frame(f = factor(c("a", "e")), x = c(0.1, 0.2))), 2)
})
