
test_that("predict works with continuous data", {
  set.seed(1)
  x <- matrix(runif(150), nrow = 50, ncol = 3)
  cuts <- seq(0, 1, length.out = 4)
  xd <- apply(x, 2, function(col) as.numeric(cut(col, cuts)))
  theta <- t(matrix(c(0, 0, 0, 2, 2, 2, -2, -2, -2), ncol = 3, nrow = 3))
  log_odd <- rowSums(t(sapply(seq_along(xd[, 1]), function(row_id) sapply(seq_along(xd[row_id, ]), function(element) theta[xd[row_id, element], element]))))
  y <- rbinom(50, 1, 1 / (1 + exp(-log_odd)))
  sem_disc <- glmdisc(x, y, iter = 20, m_start = 4, test = FALSE, validation = FALSE, criterion = "aic")
  pred_sem <- predict(sem_disc, x)

  expect_length(pred_sem, 50)
  expect_true(all(pred_sem <= 1), TRUE)
  expect_true(all(pred_sem >= 0), TRUE)
})

test_that("predict works with categorical data", {
  set.seed(1)
  x <- matrix(runif(150), nrow = 50, ncol = 3)
  cuts <- seq(0, 1, length.out = 4)
  xd <- apply(x, 2, function(col) as.numeric(cut(col, cuts)))
  theta <- t(matrix(c(0, 0, 0, 2, 2, 2, -2, -2, -2), ncol = 3, nrow = 3))
  log_odd <- rowSums(t(sapply(seq_along(xd[, 1]), function(row_id) sapply(seq_along(xd[row_id, ]), function(element) theta[xd[row_id, element], element]))))
  y <- rbinom(50, 1, 1 / (1 + exp(-log_odd)))
  sem_disc <- glmdisc(data.frame(x.1 = factor(xd[, 1]), x.2 = factor(xd[, 2]), x.3 = factor(xd[, 3])), y, iter = 20, m_start = 4, test = FALSE, validation = FALSE, criterion = "aic")
  pred_sem <- predict(sem_disc, rbind(data.frame(x.1 = factor(xd[, 1]), x.2 = factor(xd[, 2]), x.3 = factor(xd[, 3]))))

  expect_length(pred_sem, 50)
  expect_true(all(pred_sem <= 1), TRUE)
  expect_true(all(pred_sem >= 0), TRUE)
})

test_that("predict errors with other data columns", {
  set.seed(1)
  x <- matrix(runif(150), nrow = 50, ncol = 3)
  cuts <- seq(0, 1, length.out = 4)
  xd <- apply(x, 2, function(col) as.numeric(cut(col, cuts)))
  theta <- t(matrix(c(0, 0, 0, 2, 2, 2, -2, -2, -2), ncol = 3, nrow = 3))
  log_odd <- rowSums(t(sapply(seq_along(xd[, 1]), function(row_id) sapply(seq_along(xd[row_id, ]), function(element) theta[xd[row_id, element], element]))))
  y <- rbinom(50, 1, 1 / (1 + exp(-log_odd)))
  sem_disc <- glmdisc(data.frame(x.1 = factor(xd[, 1]), x.2 = factor(xd[, 2]), x.3 = factor(xd[, 3])), y, iter = 20, m_start = 4, test = FALSE, validation = FALSE, criterion = "aic")
  expect_error(predict(sem_disc, x[, 1:2]))
})

test_that("predict errors with other data types", {
  set.seed(1)
  x <- matrix(runif(150), nrow = 50, ncol = 3)
  cuts <- seq(0, 1, length.out = 4)
  xd <- apply(x, 2, function(col) as.numeric(cut(col, cuts)))
  theta <- t(matrix(c(0, 0, 0, 2, 2, 2, -2, -2, -2), ncol = 3, nrow = 3))
  log_odd <- rowSums(t(sapply(seq_along(xd[, 1]), function(row_id) sapply(seq_along(xd[row_id, ]), function(element) theta[xd[row_id, element], element]))))
  y <- rbinom(50, 1, 1 / (1 + exp(-log_odd)))
  sem_disc <- glmdisc(data.frame(x.1 = factor(xd[, 1]), x.2 = factor(xd[, 2]), x.3 = factor(xd[, 3])), y, iter = 20, m_start = 4, test = FALSE, validation = FALSE, criterion = "aic")
  expect_error(predict(sem_disc, x))
})

test_that("predict errors with unseen categorical data", {
  set.seed(1)
  x <- matrix(runif(150), nrow = 50, ncol = 3)
  cuts <- seq(0, 1, length.out = 4)
  xd <- apply(x, 2, function(col) as.numeric(cut(col, cuts)))
  theta <- t(matrix(c(0, 0, 0, 2, 2, 2, -2, -2, -2), ncol = 3, nrow = 3))
  log_odd <- rowSums(t(sapply(seq_along(xd[, 1]), function(row_id) sapply(seq_along(xd[row_id, ]), function(element) theta[xd[row_id, element], element]))))
  y <- rbinom(50, 1, 1 / (1 + exp(-log_odd)))
  sem_disc <- glmdisc(data.frame(x.1 = factor(xd[, 1]), x.2 = factor(xd[, 2]), x.3 = factor(xd[, 3])), y, iter = 20, m_start = 4, test = FALSE, validation = FALSE, criterion = "aic")
  expect_error(predict(sem_disc, rbind(
    data.frame(x.1 = factor(xd[, 1]), x.2 = factor(xd[, 2]), x.3 = factor(xd[, 3])),
    data.frame(x.1 = factor(10), x.2 = factor(10), x.3 = factor(10))
  )))
})

test_that("predict returns one probability per row, consistent with discretize()", {
  set.seed(1)
  n <- 400
  x <- matrix(runif(n * 3), n, 3)
  xd <- apply(x, 2, function(col) as.numeric(cut(col, seq(0, 1, length.out = 4))))
  theta <- matrix(c(0, 2, -2), 3, 3)
  y <- rbinom(n, 1, plogis(rowSums(sapply(1:3, function(j) theta[xd[, j], j]))))
  sem_disc <- glmdisc(x, y, iter = 40, m_start = 5, test = FALSE, validation = FALSE, criterion = "aic", interact = FALSE)
  newdata <- as.data.frame(matrix(runif(150), 50, 3))
  names(newdata) <- colnames(x) <- colnames(as.data.frame(x))
  prediction <- predict(sem_disc, as.data.frame(x))
  expect_length(prediction, n)
  expect_true(all(prediction > 0 & prediction < 1))
  expect_equal(prediction, as.numeric(sem_disc@best.disc[[1]]$fitted.values))
  expect_length(predict(sem_disc, newdata), 50)
  # a missing value in a numeric feature does not break prediction
  newdata[1, 1] <- NA
  expect_length(predict(sem_disc, newdata), 50)
})

test_that("predictlogisticRegression errors instead of crashing on non-conformable inputs", {
  set.seed(1)
  expect_error(predictlogisticRegression(matrix(1, nrow = 3, ncol = 2), c(1, 2, 3)), "Non-conformable")
  expect_equal(predictlogisticRegression(matrix(0L, nrow = 2, ncol = 1), 1), c(0.5, 0.5))
})

test_that("stats::predict works on a glmdisc object", {
  set.seed(1)
  x <- matrix(runif(300), 100, 3)
  y <- rbinom(100, 1, 0.5)
  fit <- glmdisc(x, y, iter = 5, m_start = 3, test = FALSE, validation = FALSE, criterion = "aic", interact = FALSE)
  expect_equal(stats::predict(fit, as.data.frame(x)), predict(fit, as.data.frame(x)))
})
