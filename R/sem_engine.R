# The SEM-Gibbs algorithm itself.
#
# The quantization of every feature is held as an integer matrix `codes` (n x d) whose
# entries are *labels*: arbitrary integers, with the label `m_start + 1` reserved for
# missing values of numeric features. A sweep of the algorithm
#   1. compacts the labels present in the training rows into 1..K_j,
#   2. fits the logistic regression p(y | e) on the current labels,
#   3. for each feature in random order, fits the link p(e_j | x_j), records its
#      most probable label (the "MAP" quantization, `emap`) and draws a new label from
#      p(e_j | x_j) * p(y | e_j, e_-j),
#   4. fits the logistic regression on the MAP quantization, evaluates the selection
#      criterion with it and keeps the best sweep so far,
#   5. (optionally) makes one Metropolis-Hastings move on the set of interactions.

# Pairs (a, b), a < b, of features with an active interaction and at least two values.
active_pairs <- function(delta, K) {
  if (is.null(delta)) {
    return(matrix(integer(0), 0L, 2L))
  }
  pairs <- which(delta == 1 & upper.tri(delta), arr.ind = TRUE)
  pairs <- pairs[K[pairs[, 1L]] > 1L & K[pairs[, 2L]] > 1L, , drop = FALSE]
  matrix(as.integer(pairs), ncol = 2L)
}

sem_engine <- function(X, types, y, split, criterion, iter, m_start, reg_type, interact, verbose, patience = NULL) {
  n <- nrow(X)
  d <- ncol(X)
  train <- split$train
  validation_rows <- split$validation
  use_validation <- any(validation_rows)
  na_label <- m_start + 1L
  is_numeric <- types == "numeric"
  x_raw <- lapply(seq_len(d), function(j) if (is_numeric[j]) X[[j]] else as.integer(X[[j]]))
  x_levels <- lapply(seq_len(d), function(j) if (is_numeric[j]) NULL else levels(X[[j]]))
  rows <- seq_len(n)

  # Initial quantization, at random
  codes <- matrix(0L, n, d)
  for (j in seq_len(d)) {
    if (is_numeric[j]) {
      observed <- !is.na(X[[j]])
      codes[observed, j] <- sample.int(m_start, sum(observed), replace = TRUE)
      codes[!observed, j] <- na_label
    } else {
      codes[, j] <- sample.int(min(m_start, nlevels(X[[j]])), n, replace = TRUE)
    }
  }

  delta <- p_delta <- NULL
  if (interact) {
    initial <- initialize_interaction(d, X, y, train)
    delta <- initial$delta
    p_delta <- initial$p_delta
  }

  history <- list()
  best <- NULL
  best_value <- -Inf
  previous_links <- vector("list", d)

  for (it in seq_len(iter)) {
    labels_present <- lapply(seq_len(d), function(j) sort(unique(codes[train, j])))
    K <- lengths(labels_present)
    if (all(K == 1L)) {
      if (it <= 2L) stop("All variables discretized in one value after one pass: try changing the seed")
      if (verbose) message("Early stopping rule: all variables discretized in one value")
      break
    }

    # Compact the labels into 1..K_j; held-out rows with a label unknown to the
    # training rows (only possible for the random initialization) are redrawn.
    ci <- matrix(0L, n, d)
    for (j in seq_len(d)) {
      unknown <- !(codes[, j] %in% labels_present[[j]])
      if (any(unknown)) {
        codes[unknown, j] <- sample(labels_present[[j]], sum(unknown), replace = TRUE)
      }
      ci[, j] <- match(codes[, j], labels_present[[j]])
    }

    model_e <- fit_logit_codes(ci, K, y, train, active_pairs(delta, K))
    eta <- logit_eta(model_e, ci)

    links <- vector("list", d)
    map_codes <- codes # labels of the MAP quantization

    for (j in sample.int(d)) {
      labels_j <- labels_present[[j]]
      if (K[j] == 1L) {
        links[[j]] <- constant_link(labels_j)
        next
      }

      # p(e_j | x_j)
      link <- if (is_numeric[j]) {
        fit_numeric_link(x_raw[[j]], codes[, j], train, labels_j, na_label, reg_type, previous_links[[j]])
      } else {
        fit_factor_link(x_raw[[j]], codes[, j], train, labels_j, length(x_levels[[j]]), x_levels[[j]])
      }
      P <- link_probs(link, x_raw[[j]])

      # most probable label, restricted to the labels that training rows get
      map_j <- resample_unseen_levels(
        factor(labels_j[max.col(P, ties.method = "first")], levels = labels_j),
        P, train, !train, "map"
      )
      map_codes[, j] <- as.integer(as.character(map_j))
      link$S <- sort(unique(map_codes[train, j]))

      # draw e_j from p(e_j | x_j) * p(y | e_j, e_-j)
      contribution <- feature_contribution(model_e, ci, j)
      current <- contribution[cbind(rows, ci[, j])]
      p1 <- stats::plogis(contribution + (eta - current))
      W <- P * (p1 * y + (1 - p1) * (1 - y))
      drawn <- resample_unseen_levels(
        factor(labels_j[sample_rows(W)], levels = labels_j),
        W, train, !train, "sample"
      )
      codes[, j] <- as.integer(as.character(drawn))
      ci[, j] <- match(codes[, j], labels_j)
      eta <- eta - current + contribution[cbind(rows, ci[, j])]

      links[[j]] <- previous_links[[j]] <- link
    }

    # Logistic regression on the MAP quantization: evaluated and (if best) returned
    S <- lapply(seq_len(d), function(j) links[[j]]$S)
    Ke <- lengths(S)
    ec <- matrix(0L, n, d)
    for (j in seq_len(d)) ec[, j] <- match(map_codes[, j], S[[j]])
    pairs_m <- active_pairs(delta, Ke)
    model_m <- fit_logit_codes(ec, Ke, y, train, pairs_m)

    value <- selection_criterion(criterion, use_validation, model_m, ec, y, train, validation_rows)
    history[[it]] <- value
    if (isTRUE(value >= best_value)) {
      best_value <- value
      best <- list(links = links, pairs = pairs_m, model = model_m, iteration = it, value = value, K = Ke)
    }

    if (interact) {
      delta <- metropolis_hastings_step(delta, p_delta, ec, Ke, y, train, model_m, verbose, colnames(X))
    }

    if (verbose) {
      message(
        "Iteration ", it, " ended with a performance of ", criterion, " = ", value,
        " (levels per feature: ", paste(Ke, collapse = "/"), ")"
      )
    }

    if (!is.null(patience) && it - best$iteration >= patience) {
      if (verbose) message("Early stopping rule: no improvement of the criterion for ", patience, " iterations")
      break
    }
  }

  if (is.null(best)) stop("No discretization could be fitted.")
  list(best = best, history = history)
}

# One Metropolis-Hastings move on the interaction matrix `delta`: flip one pair, fit the
# logistic regression with the proposed set of interactions and accept with a probability
# based on the BIC difference and on the proposal (see the thesis). Returns the new delta.
metropolis_hastings_step <- function(delta, p_delta, ec, Ke, y, train, model_current, verbose, feature_names) {
  d <- nrow(delta)
  transition <- abs(delta - p_delta)
  if (sum(transition) <= 0) {
    return(delta)
  }
  pq <- sample.int(d^2, 1L, prob = as.vector(transition))
  delta_new <- delta
  delta_new[pq] <- 1 - delta_new[pq]

  model_new <- fit_logit_codes(ec, Ke, y, train, active_pairs(delta_new, Ke))
  penalty <- log(sum(train))
  bic_new <- 2 * model_new$loglik - penalty * model_new$npar
  bic_current <- 2 * model_current$loglik - penalty * model_current$npar
  alpha <- exp(bic_new - bic_current) * transition[pq] / (1 - transition[pq])
  if (is.na(alpha)) alpha <- 0

  if (verbose) {
    involved <- arrayInd(pq, c(d, d))
    message(
      "Current interaction being tested is between variables ", feature_names[involved[1L, 1L]],
      " and ", feature_names[involved[1L, 2L]], " with a probability of acceptance of ", min(alpha, 1)
    )
  }
  if (stats::runif(1L) < min(alpha, 1)) delta_new else delta
}

# Starting point of the interaction search: delta is drawn at random and p_delta, the
# proposal distribution of the Metropolis-Hastings algorithm, compares for each pair of
# features the BIC of a logistic regression with and without their interaction.
initialize_interaction <- function(d, X, y, train) {
  delta <- matrix(sample(0:1, d^2, replace = TRUE, prob = c(0.5, 0.5)), nrow = d, ncol = d)
  delta[lower.tri(delta)] <- 0
  diag(delta) <- 0

  p_delta <- matrix(0, nrow = d, ncol = d)
  n_train <- sum(train)

  for (j in seq_len(d - 1L)) {
    for (k in (j + 1L):d) {
      rows <- train & !is.na(X[[j]]) & !is.na(X[[k]])
      if (nlevels(factor(X[[j]][train & !is.na(X[[j]]) & !is.na(X[[k]])])) == 1) {
        stop(paste0("Some levels are scarce such that feature ", j, " only has 1 level in train. Try with validation = F"))
      }
      if (nlevels(factor(X[[k]][rows])) == 1) {
        stop(paste0("Some levels are scarce such that feature ", k, " only has 1 level in train. Try with validation = F"))
      }
      pair <- data.frame(labels = y[rows], X1 = X[[j]][rows], X2 = X[[k]][rows], stringsAsFactors = TRUE)
      without <- suppressWarnings(stats::glm(labels ~ X1 + X2, family = stats::binomial(link = "logit"), data = pair))
      with <- suppressWarnings(stats::glm(labels ~ X1 + X2 + X1:X2, family = stats::binomial(link = "logit"), data = pair))
      p_delta[j, k] <- 1 / (1 + exp(-without$deviance - log(n_train) * length(without$coefficients) +
        with$deviance + log(n_train) * length(with$coefficients)))
    }
  }
  list(delta = delta, p_delta = p_delta)
}
