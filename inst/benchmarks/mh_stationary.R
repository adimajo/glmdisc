# Exact stationary distribution of the Metropolis-Hastings move on interactions, on a toy
# problem with 3 possible interactions (8 states). For each variant of the acceptance rule the
# transition matrix is built exactly (no simulation) and compared with the distribution it is
# supposed to sample: prior(delta) * exp(scale * score(delta)).
#
# Usage: Rscript inst/benchmarks/mh_stationary.R

suppressMessages(library(glmdisc))
acceptance <- glmdisc:::mh_log_acceptance

p_pairs <- c(0.7, 0.2, 0.5) # p_delta of pairs (1,2), (1,3), (2,3)
score <- c(0.5, -1, 1.5, -0.5, 0, 1, -2, 2) # "-BIC" of the 8 states, state = 1 + a + 2 b + 4 c
states <- as.matrix(expand.grid(a = 0:1, b = 0:1, c = 0:1))

transition_matrix <- function(scale, hastings) {
  P <- matrix(0, 8, 8)
  for (i in 1:8) {
    s <- states[i, ]
    t <- abs(s - p_pairs) # proposal weights; each pair has a single entry in p_delta
    z <- sum(t)
    for (k in 1:3) {
      s_new <- s
      s_new[k] <- 1 - s_new[k]
      j <- 1L + sum(s_new * c(1, 2, 4))
      z_new <- sum(abs(s_new - p_pairs))
      alpha <- exp(min(0, acceptance(score[j] - score[i], t[k], z, z_new, scale, hastings)))
      P[i, j] <- (t[k] / z) * alpha
    }
    P[i, i] <- 1 - sum(P[i, ])
  }
  P
}

stationary <- function(P) {
  e <- eigen(t(P))
  v <- Re(e$vectors[, which.max(Re(e$values))])
  v / sum(v)
}

target <- function(scale) {
  prior <- apply(states, 1, function(s) prod(ifelse(s == 1, p_pairs, 1 - p_pairs)))
  w <- prior * exp(scale * score)
  w / sum(w)
}

total_variation <- function(a, b) 0.5 * sum(abs(a - b))

rows <- list()
for (scale in c(1, 0.5)) {
  for (hastings in c(FALSE, TRUE)) {
    pi_chain <- stationary(transition_matrix(scale, hastings))
    rows[[length(rows) + 1L]] <- data.frame(
      scale = scale, hastings_correction = hastings,
      tv_to_intended_target = round(total_variation(pi_chain, target(scale)), 4),
      tv_to_posterior_scale_0.5 = round(total_variation(pi_chain, target(0.5)), 4),
      expected_number_of_interactions = round(sum(pi_chain * rowSums(states)), 3)
    )
  }
}
result <- do.call(rbind, rows)
cat("Toy problem, 3 candidate interactions, exact stationary distributions\n")
cat("(intended target = prior * exp(scale * score); 'posterior' = scale 0.5)\n\n")
print(result, row.names = FALSE)
cat("\nExpected number of interactions under the prior alone:", sum(p_pairs), "\n")
cat("Under the posterior (scale 0.5):", round(sum(target(0.5) * rowSums(states)), 3), "\n")
cat("Under the squared posterior (scale 1):", round(sum(target(1) * rowSums(states)), 3), "\n")
