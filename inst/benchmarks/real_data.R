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

source(file.path(dirname(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1L])), "common.R"))

methods <- list(
  logit_raw = method_raw,
  logit_quantile5 = method_quantile,
  logit_mdlp = method_mdlp,
  logit_woe = method_woe,
  glmdisc = method_glmdisc,
  glmdisc_bic = method_glmdisc_bic,
  glmdisc_interact = function(...) method_glmdisc(..., interact = TRUE)
)

# ---- main -----------------------------------------------------------------------------------

datasets <- load_datasets()
cores <- max(1L, min(parallel::detectCores() - 2L, 6L))
results <- list()
for (name in names(datasets)) {
  message("== ", name, ": ", datasets[[name]]$description)
  runs <- parallel::mclapply(seq_len(repeats), function(r) run_repetition(name, datasets[[name]], r, methods),
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
