# Internal helpers for glmdisc().

# Row-normalize a probability matrix; rows that are all zero / NaN become
# uniform (this is what prop.table.robust() did).
normalize_rows <- function(x) {
  tab <- x / rowSums(x)
  tab[is.na(tab)] <- 1 / ncol(tab)
  tab
}

# Draw one column index per row of a probability matrix (rows need not sum to 1).
# Vectorized inverse-CDF sampling, replaces apply(p, 1, function(p) sample(...)).
sample_rows <- function(prob) {
  prob <- normalize_rows(prob)
  cum <- prob
  if (ncol(prob) > 1L) {
    for (k in 2:ncol(prob)) cum[, k] <- cum[, k - 1L] + prob[, k]
  }
  u <- stats::runif(nrow(prob)) * cum[, ncol(prob)]
  pmin(rowSums(cum < u) + 1L, ncol(prob))
}

# Make sure that rows of `target` (logical mask over the n observations) only
# take values that also occur in the training rows `train`.
#
# `current`: factor of length n (current quantization of one feature);
# `prob`: n x K matrix of probabilities whose colnames are the candidate
# levels; `how`: sample from (stochastic, e) or take the arg-max (emap).
# Candidates that never occur in `train` get probability 0 for the offending rows.
# Returns the updated factor (same levels as `current`).
resample_unseen_levels <- function(current, prob, train, target, how = c("sample", "map")) {
  how <- match.arg(how)
  chr <- as.character(current)
  seen <- unique(chr[train])
  bad <- which(target & !(chr %in% seen))
  if (length(bad) == 0L) {
    return(current)
  }
  p <- prob[bad, , drop = FALSE]
  p[, !(colnames(p) %in% seen)] <- 0
  p <- normalize_rows(p)
  # rows with no mass on seen levels (all zero before normalization) are uniform
  # over all columns by normalize_rows(); restrict them to seen levels
  empty <- rowSums(p[, colnames(p) %in% seen, drop = FALSE]) == 0
  if (any(empty)) {
    p[empty, ] <- 0
    p[empty, colnames(p) %in% seen] <- 1
    p <- normalize_rows(p)
  }
  idx <- if (how == "sample") sample_rows(p) else max.col(p, ties.method = "first")
  chr[bad] <- colnames(p)[idx]
  factor(chr, levels = levels(current))
}

# Log-likelihood of binary labels given predicted P(y = 1).
bernoulli_loglik <- function(labels, prob) {
  sum(log(labels * prob + (1 - labels) * (1 - prob)))
}

# Remove interaction terms ("V1:V2") that involve an excluded main effect
# ("V1"). Matching is on whole tokens: excluding "V1" must not drop "V10:V12".
drop_interaction_terms <- function(terms, excluded) {
  if (length(terms) == 0L || length(excluded) == 0L) {
    return(terms)
  }
  involves_excluded <- vapply(
    strsplit(terms, ":", fixed = TRUE),
    function(parts) any(parts %in% excluded),
    logical(1)
  )
  terms[!involves_excluded]
}
