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

test_that("predict gives NA with a warning for quantized values unseen during training", {
  set.seed(1)
  n <- 400
  x <- matrix(runif(n * 3), n, 3)
  xd <- apply(x, 2, function(col) as.numeric(cut(col, seq(0, 1, length.out = 4))))
  theta <- matrix(c(0, 2, -2), 3, 3)
  log_odd <- rowSums(sapply(1:3, function(j) theta[xd[, j], j]))
  y <- rbinom(n, 1, 1 / (1 + exp(-log_odd)))
  sem_disc <- glmdisc(x, y, iter = 40, m_start = 5, test = FALSE, validation = FALSE, criterion = "aic", interact = FALSE)
  encoder <- sem_disc@parameters$encoder
  # a feature with at least 3 quantized levels, so that one can be removed
  var <- encoder$facVars[lengths(encoder$lvls[encoder$facVars]) >= 3][1]
  expect_false(is.na(var))
  col <- as.integer(sub("V", "", var))

  # Pretend that the last level of `var` was never seen in training: drop it from the
  # encoder together with its coefficient.
  lvls <- encoder$lvls[[var]]
  dropped <- lvls[length(lvls)]
  position <- sum(lengths(encoder$lvls[encoder$facVars[seq_len(match(var, encoder$facVars))]]))
  sem_disc@parameters$encoder$lvls[[var]] <- lvls[-length(lvls)]
  sem_disc@best.disc[[1]]$coefficients <- sem_disc@best.disc[[1]]$coefficients[-position]

  newdata <- as.data.frame(x)
  has_dropped_level <- discretize(sem_disc, newdata)[, col] == dropped
  expect_true(any(has_dropped_level))
  expect_warning(prediction <- predict(sem_disc, newdata), "not seen in the training set")
  expect_length(prediction, nrow(newdata))
  expect_true(all(is.na(prediction[has_dropped_level])))
  expect_false(anyNA(prediction[!has_dropped_level]))
})

