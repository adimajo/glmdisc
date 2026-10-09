# Benchmark of glmdisc against common ways of preparing features for logistic regression,
# on real datasets that ship with CRAN packages (no download needed).
#
# Usage:  Rscript inst/benchmarks/real_data.R [repeats] [iter] [outdir]
#   repeats  number of random stratified 70/30 splits per dataset (default 20)
#   iter     number of SEM iterations of glmdisc (default 300)
#   outdir   where to write results.csv and summary.txt (default: ./benchmark-results)
#
# Required packages: glmdisc, scorecard, discretization, MASS, ISLR, AER.
# The script is not part of the package tests: it takes a few minutes.

args <- commandArgs(trailingOnly = TRUE)
repeats <- if (length(args) >= 1L) as.integer(args[1L]) else 20L
sem_iter <- if (length(args) >= 2L) as.integer(args[2L]) else 300L
outdir <- if (length(args) >= 3L) args[3L] else "benchmark-results"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

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

load_datasets <- function() {
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
        xor = 2.5 * ((x[, 1] > 0.5) != (x[, 2] > 0.5)) - 1.25
      )
      list(X = as.data.frame(x), y = stats::rbinom(n, 1L, stats::plogis(linear_predictor)))
    }
  }

  list(
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
       best_iteration = which.max(unlist(fit@performance$criterionEvolution)))
}

method_glmdisc_bic <- function(train, test, y_train, interact = FALSE, ...) {
  # no validation set: the BIC picks the quantization and all training data are used
  fit <- glmdisc::glmdisc(train, y_train,
    iter = sem_iter, m_start = 10, test = FALSE, validation = FALSE,
    criterion = "bic", interact = interact
  )
  test <- align_factors(train, test)
  list(p = as.numeric(predict(fit, test)), npar = length(stats::coef(fit@best.disc[[1]])))
}

methods <- list(
  logit_raw = method_raw,
  logit_quantile5 = method_quantile,
  logit_mdlp = method_mdlp,
  logit_woe = method_woe,
  glmdisc = method_glmdisc,
  glmdisc_bic = method_glmdisc_bic,
  glmdisc_interact = function(...) method_glmdisc(..., interact = TRUE)
)

# ---- one repetition ---------------------------------------------------------------------

run_repetition <- function(dataset_name, dataset, rep) {
  if (!is.null(dataset$generate)) dataset <- c(dataset, dataset$generate(rep))
  set.seed(1000L + rep)
  X <- clean_types(dataset$X)
  y <- dataset$y
  train_index <- sort(unlist(lapply(split(seq_along(y), y), function(idx) sample(idx, floor(0.7 * length(idx))))))
  train <- X[train_index, , drop = FALSE]
  test <- X[-train_index, , drop = FALSE]
  rows <- list()
  for (name in names(methods)) {
    if (name == "glmdisc_interact" && ncol(X) > 12L) next
    set.seed(5000L + rep)
    elapsed <- system.time(result <- tryCatch(methods[[name]](train, test, y[train_index]), error = function(e) e))[["elapsed"]]
    if (inherits(result, "error")) {
      rows[[length(rows) + 1L]] <- data.frame(dataset = dataset_name, rep = rep, method = name, gini = NA_real_,
                                              npar = NA_integer_, secs = elapsed, note = conditionMessage(result))
    } else {
      ok <- !is.na(result$p)
      rows[[length(rows) + 1L]] <- data.frame(dataset = dataset_name, rep = rep, method = name,
                                              gini = gini(y[-train_index][ok], result$p[ok]),
                                              npar = result$npar, secs = elapsed, note = "")
    }
  }
  do.call(rbind, rows)
}

# ---- main -----------------------------------------------------------------------------------

datasets <- load_datasets()
cores <- max(1L, min(parallel::detectCores() - 2L, 6L))
results <- list()
for (name in names(datasets)) {
  message("== ", name, ": ", datasets[[name]]$description)
  runs <- parallel::mclapply(seq_len(repeats), function(r) run_repetition(name, datasets[[name]], r),
                             mc.cores = cores, mc.set.seed = FALSE)
  results[[name]] <- do.call(rbind, runs)
}
results <- do.call(rbind, results)
utils::write.csv(results, file.path(outdir, "results.csv"), row.names = FALSE)

# ---- summary ------------------------------------------------------------------------------------

summary_rows <- list()
for (name in unique(results$dataset)) {
  sub <- results[results$dataset == name, ]
  reference <- sub[sub$method == "logit_raw", c("rep", "gini")]
  names(reference)[2L] <- "gini_raw"
  for (m in unique(sub$method)) {
    s <- merge(sub[sub$method == m, ], reference, by = "rep")
    diff <- s$gini - s$gini_raw
    summary_rows[[length(summary_rows) + 1L]] <- data.frame(
      dataset = name, method = m, n_ok = sum(!is.na(s$gini)),
      gini_mean = mean(s$gini, na.rm = TRUE), gini_sd = stats::sd(s$gini, na.rm = TRUE),
      diff_vs_raw = mean(diff, na.rm = TRUE), diff_se = stats::sd(diff, na.rm = TRUE) / sqrt(sum(!is.na(diff))),
      npar = mean(s$npar, na.rm = TRUE), secs = mean(s$secs)
    )
  }
}
summary_table <- do.call(rbind, summary_rows)
summary_table[, c("gini_mean", "gini_sd", "diff_vs_raw", "diff_se")] <-
  round(summary_table[, c("gini_mean", "gini_sd", "diff_vs_raw", "diff_se")], 3)
summary_table$npar <- round(summary_table$npar, 1)
summary_table$secs <- round(summary_table$secs, 1)
capture <- utils::capture.output(print(summary_table, row.names = FALSE))
writeLines(c(sprintf("glmdisc benchmark: %d random 70/30 splits per dataset, %d SEM iterations", repeats, sem_iter),
             "gini = 2*AUC-1 on the held-out 30%; diff_vs_raw = paired difference with the raw logit (mean, standard error)",
             "", capture), file.path(outdir, "summary.txt"))
cat(paste(capture, collapse = "\n"), "\n")
