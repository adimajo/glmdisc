# How does the quality of the discretization depend on the number of SEM iterations?
#
# Simulated data (3000 observations, 5 uniform features, the logit is a step function of the
# first three), 12 repetitions, 70% used for fitting. For each iteration budget, glmdisc is fit
# with the BIC on the training data ("bic") or with a validation set and the Gini ("gini");
# the Gini is evaluated on the 30% held out.
#
# Usage: Rscript inst/benchmarks/convergence.R [outdir]   (a few minutes)

outdir <- if (length(commandArgs(TRUE))) commandArgs(TRUE)[1L] else "benchmark-results"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
suppressMessages(library(glmdisc))

gini <- function(y, p) {
  ranks <- rank(p)
  n1 <- sum(y)
  2 * ((sum(ranks[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * (length(y) - n1))) - 1
}

simulate <- function(n, seed) {
  set.seed(seed)
  x <- matrix(runif(n * 5), n, 5)
  steps <- apply(x[, 1:3], 2, function(col) as.integer(cut(col, c(0, 1 / 3, 2 / 3, 1))))
  logit <- rowSums(sapply(1:3, function(j) c(-1.5, 0.5, 1.5)[steps[, j]])) - 1.5
  list(x = as.data.frame(x), y = rbinom(n, 1, plogis(logit)))
}

one_repetition <- function(rep) {
  data <- simulate(3000, 90000 + rep)
  train <- sample(3000, 2100)
  out <- NULL
  for (iter in c(50, 100, 300, 1000, 3000)) {
    for (criterion in c("bic", "gini")) {
      set.seed(rep)
      elapsed <- system.time(
        fit <- if (criterion == "bic") {
          glmdisc(data$x[train, ], data$y[train], iter = iter, m_start = 10, test = FALSE, validation = FALSE, criterion = "bic", interact = FALSE)
        } else {
          glmdisc(data$x[train, ], data$y[train], iter = iter, m_start = 10, test = FALSE, validation = TRUE, proportions = 0.25, criterion = "gini", interact = FALSE)
        }
      )[["elapsed"]]
      out <- rbind(out, data.frame(
        rep = rep, iter = iter, criterion = criterion,
        test_gini = gini(data$y[-train], predict(fit, data$x[-train, ])), secs = elapsed,
        best_iter = which.max(unlist(fit@performance$criterionEvolution)),
        levels = mean(vapply(fit@best.disc$bestLinkFunction, function(link) length(link$S), 1L))
      ))
    }
  }
  out
}

results <- do.call(rbind, parallel::mclapply(1:12, one_repetition, mc.cores = min(6L, parallel::detectCores()), mc.set.seed = FALSE))
write.csv(results, file.path(outdir, "convergence.csv"), row.names = FALSE)
summary_table <- aggregate(cbind(test_gini, secs, best_iter, levels) ~ iter + criterion, results, mean)
print(summary_table[order(summary_table$criterion, summary_table$iter), ], digits = 3, row.names = FALSE)
