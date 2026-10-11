# Shared pieces of the benchmark scripts: datasets, baselines, glmdisc wrappers and the
# repetition runner. Sourced by real_data.R and interactions.R (which set `sem_iter` first).

suppressMessages({
  library(glmdisc)
  library(scorecard)
  library(discretization)
})

# ---- datasets -------------------------------------------------------------------------

as_binary <- function(x, positive) as.integer(x == positive)

load_data <- function(name, package) {
  environment <- new.env()
  utils::data(list = name, package = package, envir = environment)
  environment[[name]]
}

load_datasets <- function(extra = FALSE) {
  german <- load_data("germancredit", "scorecard")
  pima <- rbind(load_data("Pima.tr", "MASS"), load_data("Pima.te", "MASS"))
  default <- load_data("Default", "ISLR")
  credit_card <- load_data("CreditCard", "AER")
  simulate <- function(kind) {
    function(rep) {
      set.seed(90000L + rep)
      n <- 3000L
      x <- matrix(stats::runif(n * 5L), n, 5L, dimnames = list(NULL, paste0("x", 1:5)))
      linear_predictor <- switch(kind,
        steps = {
          steps <- apply(x[, 1:3], 2, function(col) as.integer(cut(col, c(0, 1 / 3, 2 / 3, 1))))
          rowSums(sapply(1:3, function(j) c(-1.5, 0.5, 1.5)[steps[, j]])) - 1.5
        },
        ushape = 6 * (x[, 1] - 0.5)^2 * 4 + 6 * (x[, 2] - 0.5)^2 * 4 + x[, 3] - 3.5,
        xor = 2.5 * ((x[, 1] > 0.5) != (x[, 2] > 0.5)) - 1.25,
        mixed = {
          steps <- apply(x[, 1:3], 2, function(col) as.integer(cut(col, c(0, 1 / 3, 2 / 3, 1))))
          rowSums(sapply(1:3, function(j) c(-1, 0.3, 1)[steps[, j]])) + 1.5 * ((x[, 1] > 0.5) & (x[, 2] > 0.5)) - 1.5
        }
      )
      list(X = as.data.frame(x), y = stats::rbinom(n, 1L, stats::plogis(linear_predictor)))
    }
  }

  datasets <- list(
    sim_steps = list(
      description = "Simulation: logit is a step function of 3 of 5 uniform features",
      generate = simulate("steps")
    ),
    sim_ushape = list(
      description = "Simulation: logit is U-shaped (non-monotone) in 2 of 5 uniform features",
      generate = simulate("ushape")
    ),
    sim_xor = list(
      description = "Simulation: outcome driven by the XOR of two thresholded features (pure interaction)",
      generate = simulate("xor")
    ),
    german = list(
      description = "German credit (UCI Statlog): 1000 loans, 7 numeric + 13 categorical features",
      X = german[, setdiff(names(german), "creditability")],
      y = as_binary(german$creditability, "bad")
    ),
    pima = list(
      description = "Pima Indians diabetes (MASS::Pima.tr + Pima.te): 532 women, 7 numeric features",
      X = pima[, setdiff(names(pima), "type")],
      y = as_binary(pima$type, "Yes")
    ),
    default = list(
      description = "ISLR::Default, simulated credit card default: 10000 customers, 3 features",
      X = default[, c("student", "balance", "income")],
      y = as_binary(default$default, "Yes")
    ),
    creditcard = list(
      description = "AER::CreditCard: 1319 card applications (share and expenditure removed: they leak the outcome)",
      X = credit_card[, setdiff(names(credit_card), c("card", "share", "expenditure"))],
      y = as_binary(credit_card$card, "yes")
    )
  )
  if (extra) {
    datasets$sim_mixed <- list(
      description = "Simulation: step-wise main effects plus one moderate interaction between x1 and x2",
      generate = simulate("mixed")
    )
  }
  datasets
}

# ---- helpers ----------------------------------------------------------------------------

gini <- function(y, p) {
  ranks <- rank(p)
  n1 <- sum(y)
  2 * ((sum(ranks[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * (length(y) - n1))) - 1
}

clean_types <- function(X) {
  for (j in seq_along(X)) if (is.character(X[[j]]) || is.logical(X[[j]])) X[[j]] <- factor(X[[j]])
  X
}

# Make the factors of `test` use the levels of `train` (unseen levels -> most frequent level).
align_factors <- function(train, test) {
  for (j in seq_along(train)) {
    if (is.factor(train[[j]])) {
      mode_level <- names(which.max(table(train[[j]])))
      value <- as.character(test[[j]])
      value[!(value %in% levels(train[[j]]))] <- mode_level
      test[[j]] <- factor(value, levels = levels(train[[j]]))
      train[[j]] <- droplevels(train[[j]])
      test[[j]] <- factor(as.character(test[[j]]), levels = levels(train[[j]]))
    }
  }
  test
}

# Median imputation of numeric features, with train medians; factor NA -> own level.
impute <- function(train, test) {
  for (j in seq_along(train)) {
    if (is.numeric(train[[j]])) {
      m <- stats::median(train[[j]], na.rm = TRUE)
      train[[j]][is.na(train[[j]])] <- m
      test[[j]][is.na(test[[j]])] <- m
    }
  }
  list(train = train, test = test)
}

fit_glm <- function(train_df, test_df, y_train) {
  # features that are constant (or a single level) on the training set carry no information
  keep <- vapply(train_df, function(column) length(unique(column)) > 1L, logical(1L))
  train_df <- train_df[, keep, drop = FALSE]
  test_df <- test_df[, keep, drop = FALSE]
  train_df$y <- y_train
  model <- suppressWarnings(stats::glm(y ~ ., family = stats::binomial, data = train_df))
  p <- suppressWarnings(stats::predict(model, newdata = test_df, type = "response"))
  list(p = as.numeric(p), npar = sum(!is.na(stats::coef(model))))
}

# Number of pairs of features whose interaction is in the final model.
count_interactions <- function(fit) {
  sum(grepl(":", attr(stats::terms(fit@best.disc[[3]]), "term.labels"), fixed = TRUE))
}

# ---- competitors ------------------------------------------------------------------------

method_raw <- function(train, test, y_train, ...) {
  i <- impute(train, test)
  fit_glm(i$train, align_factors(i$train, i$test), y_train)
}

method_quantile <- function(train, test, y_train, bins = 5L, ...) {
  i <- impute(train, test)
  tr <- i$train
  te <- align_factors(i$train, i$test)
  for (j in seq_along(tr)) {
    if (is.numeric(tr[[j]])) {
      breaks <- unique(stats::quantile(tr[[j]], seq(0, 1, length.out = bins + 1L)))
      breaks[1L] <- -Inf
      breaks[length(breaks)] <- Inf
      if (length(breaks) < 3L) next
      tr[[j]] <- cut(tr[[j]], breaks)
      te[[j]] <- factor(as.character(cut(te[[j]], breaks)), levels = levels(tr[[j]]))
    }
  }
  fit_glm(tr, te, y_train)
}

method_mdlp <- function(train, test, y_train, ...) {
  i <- impute(train, test)
  tr <- i$train
  te <- align_factors(i$train, i$test)
  numeric_cols <- which(vapply(tr, is.numeric, logical(1L)))
  if (length(numeric_cols)) {
    cuts <- discretization::mdlp(data.frame(tr[, numeric_cols, drop = FALSE], y = y_train))$cutp
    for (k in seq_along(numeric_cols)) {
      j <- numeric_cols[k]
      cutpoints <- suppressWarnings(as.numeric(cuts[[k]]))
      if (length(cutpoints) == 0L || all(is.na(cutpoints))) {
        tr[[j]] <- factor(rep("all", nrow(tr)))
        te[[j]] <- factor(rep("all", nrow(te)), levels = "all")
        next
      }
      breaks <- c(-Inf, sort(cutpoints), Inf)
      tr[[j]] <- cut(tr[[j]], breaks)
      te[[j]] <- factor(as.character(cut(te[[j]], breaks)), levels = levels(tr[[j]]))
    }
  }
  fit_glm(tr, te, y_train)
}

method_woe <- function(train, test, y_train, ...) {
  dt <- data.frame(train, y = y_train)
  bins <- suppressWarnings(scorecard::woebin(dt, y = "y", positive = 1, print_step = 0, no_cores = 1L))
  train_woe <- scorecard::woebin_ply(dt, bins, print_step = 0, no_cores = 1L)
  test_woe <- scorecard::woebin_ply(data.frame(test), bins, print_step = 0, no_cores = 1L)
  train_woe$y <- y_train
  model <- suppressWarnings(stats::glm(y ~ ., family = stats::binomial, data = train_woe))
  list(p = as.numeric(suppressWarnings(stats::predict(model, newdata = test_woe, type = "response"))),
       npar = sum(!is.na(stats::coef(model))))
}

method_glmdisc <- function(train, test, y_train, interact = FALSE, ...) {
  fit <- glmdisc::glmdisc(train, y_train,
    iter = sem_iter, m_start = 10, test = FALSE, validation = TRUE,
    proportions = 0.25, criterion = "gini", interact = interact
  )
  test <- align_factors(train, test)
  list(p = as.numeric(predict(fit, test)), npar = length(stats::coef(fit@best.disc[[1]])),
       n_interactions = count_interactions(fit))
}

method_glmdisc_bic <- function(train, test, y_train, interact = FALSE, ...) {
  # no validation set: the BIC picks the quantization and all training data are used
  fit <- glmdisc::glmdisc(train, y_train,
    iter = sem_iter, m_start = 10, test = FALSE, validation = FALSE,
    criterion = "bic", interact = interact
  )
  test <- align_factors(train, test)
  list(p = as.numeric(predict(fit, test)), npar = length(stats::coef(fit@best.disc[[1]])),
       n_interactions = count_interactions(fit))
}

# ---- one repetition ---------------------------------------------------------------------

run_repetition <- function(dataset_name, dataset, rep, methods, skip_interactions_above = 12L) {
  if (!is.null(dataset$generate)) dataset <- c(dataset, dataset$generate(rep))
  set.seed(1000L + rep)
  X <- clean_types(dataset$X)
  y <- dataset$y
  train_index <- sort(unlist(lapply(split(seq_along(y), y), function(idx) sample(idx, floor(0.7 * length(idx))))))
  train <- X[train_index, , drop = FALSE]
  test <- X[-train_index, , drop = FALSE]
  rows <- list()
  for (name in names(methods)) {
    if (grepl("interact", name) && ncol(X) > skip_interactions_above) next
    set.seed(5000L + rep)
    elapsed <- system.time(result <- tryCatch(methods[[name]](train, test, y[train_index]), error = function(e) e))[["elapsed"]]
    if (inherits(result, "error")) {
      rows[[length(rows) + 1L]] <- data.frame(dataset = dataset_name, rep = rep, method = name, gini = NA_real_,
                                              npar = NA_integer_, secs = elapsed, note = conditionMessage(result),
                                              interactions = NA_integer_)
    } else {
      ok <- !is.na(result$p)
      rows[[length(rows) + 1L]] <- data.frame(dataset = dataset_name, rep = rep, method = name,
                                              gini = gini(y[-train_index][ok], result$p[ok]),
                                              npar = result$npar, secs = elapsed, note = "",
                                              interactions = if (is.null(result$n_interactions)) NA_integer_ else result$n_interactions)
    }
  }
  do.call(rbind, rows)
}

