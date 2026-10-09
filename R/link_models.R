# "Link" models p(e | x): probability of each quantized value (label) e given the
# raw feature x, for one feature at a time.
#
# A link is a list with
#   type   "numeric" or "factor"
#   labels integer labels of the quantized values it can output (sorted); for a numeric
#          feature this may include the label `na_label` reserved for missing values
# plus type-specific fields (see fit_numeric_link() and fit_factor_link()).
# Later, glmdisc() adds the fields `S` (labels that the MAP is allowed to return) and
# `relabel` (label -> name of the level shown to the user).

# ---- numeric features --------------------------------------------------------------

# Fit p(e | x) for a numeric feature on the training rows.
#   x        numeric vector (all observations; NA allowed)
#   codes    integer labels of the current quantization (all observations)
#   train    logical mask of training rows
#   labels   sorted labels present among the training rows
#   na_label label reserved for missing values
#   reg_type "poly" (multinomial logit, default) or "polr" (ordered logit)
#   previous the link fitted for this feature at the previous sweep (warm start), or NULL
fit_numeric_link <- function(x, codes, train, labels, na_label, reg_type = "poly", previous = NULL) {
  fit_labels <- setdiff(labels, na_label)
  rows <- train & !is.na(x) & codes %in% fit_labels
  mu <- mean(x[rows])
  sdv <- stats::sd(x[rows])
  if (!is.finite(sdv) || sdv == 0) sdv <- 1
  link <- list(
    type = "numeric", reg = "poly", labels = labels, fit_labels = fit_labels,
    na_label = na_label, mu = mu, sdv = sdv, coef = NULL, model = NULL
  )
  if (length(fit_labels) < 2L) {
    return(link)
  }
  z <- (x[rows] - mu) / sdv
  cls <- match(codes[rows], fit_labels)

  if (reg_type == "polr" && length(fit_labels) > 2L) {
    # classes ordered by the mean of x
    ordered_labels <- fit_labels[order(tapply(z, cls, mean))]
    f <- factor(codes[rows], levels = ordered_labels, ordered = TRUE)
    model <- tryCatch(
      suppressWarnings(MASS::polr(f ~ z, data = data.frame(f = f, z = z), Hess = FALSE, model = FALSE)),
      error = function(e) NULL
    )
    if (!is.null(model)) {
      link$reg <- "polr"
      link$model <- model
      link$ordered_labels <- ordered_labels
      return(link)
    }
  }

  # multinomial logit (also the fallback when polr fails): fit relative to the first
  # label, warm-started from the previous sweep's coefficients when available
  start <- matrix(0, 2, length(fit_labels))
  if (!is.null(previous) && !is.null(previous$coef) && all(fit_labels %in% previous$fit_labels)) {
    # same standardization as before (the training rows are the same), so the old
    # coefficients are valid starting values once re-expressed against the new baseline
    old <- previous$coef[, match(fit_labels, previous$fit_labels), drop = FALSE]
    start <- old - old[, 1]
  }
  fit <- fit_multinomial_1d(z, cls - 1L, length(fit_labels), start, ridge = 0.1, maxit = 15L, tol = 1e-3)
  link$coef <- fit$coef
  link
}

# ---- factor features ---------------------------------------------------------------

# Fit p(e | x) for a factor feature: contingency table of e (rows) by the level of x
# (columns) on the training rows. `xi` is the integer code of the level of x (1..L).
fit_factor_link <- function(xi, codes, train, labels, n_levels, x_levels) {
  tab <- matrix(0, nrow = length(labels), ncol = n_levels, dimnames = list(as.character(labels), x_levels))
  counts <- table(factor(codes[train], levels = labels), factor(xi[train], levels = seq_len(n_levels)))
  tab[] <- as.numeric(counts)
  list(type = "factor", labels = labels, tab = tab, x_levels = x_levels)
}

# ---- probabilities and MAP ---------------------------------------------------------

# n x length(link$labels) matrix of probabilities p(e = label | x), colnames = labels.
# `x` is the numeric vector (type "numeric") or the integer level codes (type "factor").
link_probs <- function(link, x) {
  labels <- link$labels
  n <- length(x)
  if (link$type == "factor") {
    P <- normalize_rows(t(link$tab)[x, , drop = FALSE])
    colnames(P) <- as.character(labels)
    return(P)
  }

  P <- matrix(0, n, length(labels), dimnames = list(NULL, as.character(labels)))
  is_na <- is.na(x)
  fit_cols <- match(link$fit_labels, labels)
  if (any(!is_na)) {
    if (length(link$fit_labels) == 1L) {
      P[!is_na, fit_cols] <- 1
    } else {
      z <- (x[!is_na] - link$mu) / link$sdv
      if (link$reg == "polr") {
        p <- stats::predict(link$model, newdata = data.frame(z = z), type = "probs")
        p <- p[, match(link$fit_labels, link$ordered_labels), drop = FALSE]
      } else {
        p <- multinomial_1d_probs(z, link$coef)
      }
      P[!is_na, fit_cols] <- p
    }
  }
  if (any(is_na)) {
    na_col <- match(link$na_label, labels)
    if (is.na(na_col)) {
      P[is_na, ] <- 1 / length(labels)
    } else {
      P[is_na, na_col] <- 1
    }
  }
  P
}

# Most probable label for each row, restricted to the labels in `link$S`.
link_map <- function(link, x) {
  P <- link_probs(link, x)
  cols <- match(link$S, link$labels)
  link$S[max.col(P[, cols, drop = FALSE], ties.method = "first")]
}

# A link for a feature that is constant after quantization.
constant_link <- function(label) {
  list(type = "constant", labels = label, S = label)
}
