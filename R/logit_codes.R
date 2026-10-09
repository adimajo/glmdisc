# Logistic regression p(y | q) on quantized features, represented by integer codes.
#
# `ci` is an n x d integer matrix: ci[i, j] in 1..K[j] is the (compacted) quantized value
# of feature j for observation i. The model has an intercept, one coefficient per
# non-baseline value of each feature (the first value is the baseline), and optionally
# interaction coefficients for the pairs of features in `pairs` (a matrix with columns
# a < b); features with a single value (K == 1) never take part in the model.
#
# The fitted model is stored as lookup tables so that linear predictors are obtained by
# indexing, without ever rebuilding design matrices:
#   eta_i = b0 + sum_j beta[[j]][ci[i, j]] + sum_p gamma[[p]][ci[i, a_p], ci[i, b_p]]

# 0/1 indicators of the non-baseline values of feature j (n x (K - 1))
indicators <- function(codes, k) {
  outer(codes, 2:k, "==") * 1
}

# Fit the model on the rows `rows` (logical mask) of the data.
fit_logit_codes <- function(ci, K, y, rows, pairs, start = NULL) {
  d <- ncol(ci)
  n_rows <- sum(rows)
  ind <- lapply(seq_len(d), function(j) if (K[j] > 1L) indicators(ci[rows, j], K[j]) else NULL)

  blocks <- c(list(matrix(1, n_rows, 1L)), ind[K > 1L])
  for (p in seq_len(nrow(pairs))) {
    a <- pairs[p, 1L]
    b <- pairs[p, 2L]
    blocks[[length(blocks) + 1L]] <-
      ind[[a]][, rep(seq_len(K[a] - 1L), times = K[b] - 1L), drop = FALSE] *
        ind[[b]][, rep(seq_len(K[b] - 1L), each = K[a] - 1L), drop = FALSE]
  }
  X <- do.call(cbind, blocks)
  if (is.null(start)) start <- rep(0, ncol(X))
  fit <- RcppNumerical::fastLR(X, y[rows], start = start)
  coefs <- as.numeric(fit$coefficients)

  # unpack into lookup tables
  position <- 1L
  b0 <- coefs[position]
  beta <- vector("list", d)
  for (j in seq_len(d)) {
    if (K[j] > 1L) {
      beta[[j]] <- c(0, coefs[position + seq_len(K[j] - 1L)])
      position <- position + K[j] - 1L
    } else {
      beta[[j]] <- 0
    }
  }
  gamma <- vector("list", nrow(pairs))
  for (p in seq_len(nrow(pairs))) {
    a <- pairs[p, 1L]
    b <- pairs[p, 2L]
    size <- (K[a] - 1L) * (K[b] - 1L)
    g <- matrix(0, K[a], K[b])
    g[-1L, -1L] <- coefs[position + seq_len(size)]
    gamma[[p]] <- g
    position <- position + size
  }
  list(
    b0 = b0, beta = beta, gamma = gamma, pairs = pairs, K = K,
    loglik = fit$loglikelihood, npar = length(coefs), coefficients = coefs
  )
}

# Linear predictor for all rows of `ci`.
logit_eta <- function(model, ci) {
  eta <- rep(model$b0, nrow(ci))
  for (j in seq_len(ncol(ci))) {
    eta <- eta + model$beta[[j]][ci[, j]]
  }
  for (p in seq_len(nrow(model$pairs))) {
    eta <- eta + model$gamma[[p]][cbind(ci[, model$pairs[p, 1L]], ci[, model$pairs[p, 2L]])]
  }
  eta
}

# n x K[j] matrix with, for each row, the part of the linear predictor that depends on
# the value of feature j: beta_j[k] plus the interaction terms with the current values
# of the other features. Used by the Gibbs step: eta_i with feature j set to k is
# (eta_i - contribution[i, ci[i, j]]) + contribution[i, k].
feature_contribution <- function(model, ci, j) {
  n <- nrow(ci)
  contribution <- matrix(model$beta[[j]], nrow = n, ncol = length(model$beta[[j]]), byrow = TRUE)
  for (p in seq_len(nrow(model$pairs))) {
    a <- model$pairs[p, 1L]
    b <- model$pairs[p, 2L]
    if (a == j) {
      contribution <- contribution + t(model$gamma[[p]])[ci[, b], , drop = FALSE]
    } else if (b == j) {
      contribution <- contribution + model$gamma[[p]][ci[, a], , drop = FALSE]
    }
  }
  contribution
}

# Value of the selection criterion (higher is better) for a fitted model.
selection_criterion <- function(criterion, validation, model, ci, y, train, validation_rows) {
  rows <- if (validation) validation_rows else train
  prob <- stats::plogis(logit_eta(model, ci[rows, , drop = FALSE]))
  if (criterion == "gini") {
    return(normalizedGini(y[rows], prob))
  }
  if (validation) {
    # no need to penalize on held-out data
    return(bernoulli_loglik(y[rows], prob))
  }
  penalty <- if (criterion == "aic") 2 else log(sum(train))
  2 * model$loglik - penalty * model$npar
}
