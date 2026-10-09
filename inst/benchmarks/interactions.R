# Does the acceptance rule of the Metropolis-Hastings interaction search matter on real data?
#
# Compares glmdisc without interactions with the four variants of the acceptance rule:
#   thesis    exp(-delta BIC) * t / (1 - t)                   (scale 1, no proposal correction)
#   half      exp(-delta BIC / 2) * t / (1 - t)               (BIC posterior, no proposal correction)
#   hastings  exp(-delta BIC) with the proposal correction    (scale 1)
#   posterior exp(-delta BIC / 2) with the proposal correction (BIC posterior, correct sampler)
# where t = |delta - p_delta| (see ?glmdisc:::mh_log_acceptance). Only datasets with at most
# 12 features are used (the search is quadratic in the number of features).
#
# Usage: Rscript inst/benchmarks/interactions.R [repeats] [iter] [outdir]

args <- commandArgs(trailingOnly = TRUE)
repeats <- if (length(args) >= 1L) as.integer(args[1L]) else 20L
sem_iter <- if (length(args) >= 2L) as.integer(args[2L]) else 300L
outdir <- if (length(args) >= 3L) args[3L] else "benchmark-results"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

source(file.path(dirname(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1L])), "common.R"))

with_rule <- function(scale, hastings) {
  force(scale)
  force(hastings)
  function(...) {
    old <- options(glmdisc.mh_scale = scale, glmdisc.mh_hastings = hastings)
    on.exit(options(old))
    method_glmdisc(..., interact = TRUE)
  }
}

methods <- list(
  logit_raw = method_raw,
  glmdisc_none = function(...) method_glmdisc(..., interact = FALSE),
  interact_thesis = with_rule(1, FALSE),
  interact_half = with_rule(0.5, FALSE),
  interact_hastings = with_rule(1, TRUE),
  interact_posterior = with_rule(0.5, TRUE)
)

datasets <- load_datasets(extra = TRUE)
datasets <- datasets[c("sim_steps", "sim_xor", "sim_mixed", "pima", "default", "creditcard")]
cores <- max(1L, min(parallel::detectCores() - 2L, 6L))
results <- list()
for (name in names(datasets)) {
  message("== ", name, ": ", datasets[[name]]$description)
  runs <- parallel::mclapply(seq_len(repeats), function(r) run_repetition(name, datasets[[name]], r, methods),
    mc.cores = cores, mc.set.seed = FALSE
  )
  results[[name]] <- do.call(rbind, runs)
}
results <- do.call(rbind, results)
utils::write.csv(results, file.path(outdir, "interactions.csv"), row.names = FALSE)

summary_rows <- list()
for (name in unique(results$dataset)) {
  sub <- results[results$dataset == name, ]
  reference <- sub[sub$method == "glmdisc_none", c("rep", "gini")]
  names(reference)[2L] <- "gini_none"
  for (m in unique(sub$method)) {
    s <- merge(sub[sub$method == m, ], reference, by = "rep")
    diff <- s$gini - s$gini_none
    summary_rows[[length(summary_rows) + 1L]] <- data.frame(
      dataset = name, method = m, n_ok = sum(!is.na(s$gini)),
      gini = round(mean(s$gini, na.rm = TRUE), 3), gini_sd = round(stats::sd(s$gini, na.rm = TRUE), 3),
      diff_vs_none = round(mean(diff, na.rm = TRUE), 3),
      diff_se = round(stats::sd(diff, na.rm = TRUE) / sqrt(sum(!is.na(diff))), 3),
      interactions = round(mean(s$interactions, na.rm = TRUE), 2), secs = round(mean(s$secs), 1)
    )
  }
}
summary_table <- do.call(rbind, summary_rows)
capture <- utils::capture.output(print(summary_table, row.names = FALSE))
writeLines(c(
  sprintf("glmdisc interaction rules: %d random 70/30 splits per dataset, %d SEM iterations", repeats, sem_iter),
  "gini on the held-out 30%; diff_vs_none = paired difference with glmdisc without interactions (mean, standard error);",
  "interactions = number of pairwise interactions in the final model (mean)",
  "", capture
), file.path(outdir, "interactions_summary.txt"))
cat(paste(capture, collapse = "\n"), "\n")
