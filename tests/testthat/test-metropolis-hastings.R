# A toy chain on the 3 possible interactions of 3 features (8 states) whose stationary
# distribution can be enumerated exactly.
toy_target <- function(p_delta, score_table, scale) {
  states <- expand.grid(a = 0:1, b = 0:1, c = 0:1)
  prior <- apply(states, 1, function(s) prod(ifelse(s == 1, p_delta[c(1, 2, 3)], 1 - p_delta[c(1, 2, 3)])))
  weight <- prior * exp(scale * score_table)
  weight / sum(weight)
}

run_toy_chain <- function(scale, hastings, n_steps = 40000, seed = 1) {
  set.seed(seed)
  p_pairs <- c(0.7, 0.2, 0.5)
  score_values <- c(0.5, -1, 1.5, -0.5, 0, 1, -2, 2)
  states <- expand.grid(a = 0:1, b = 0:1, c = 0:1)
  index_of <- function(delta) 1L + delta[1, 2] + 2L * delta[1, 3] + 4L * delta[2, 3]
  to_matrix <- function(s) {
    m <- matrix(0, 3, 3)
    m[1, 2] <- s[1]
    m[1, 3] <- s[2]
    m[2, 3] <- s[3]
    m
  }
  p_delta <- matrix(0, 3, 3)
  p_delta[1, 2] <- p_pairs[1]
  p_delta[1, 3] <- p_pairs[2]
  p_delta[2, 3] <- p_pairs[3]
  delta <- to_matrix(c(0, 0, 0))
  counts <- numeric(8)
  for (i in seq_len(n_steps)) {
    delta <- metropolis_hastings_step(delta, p_delta, function(candidate) score_values[index_of(candidate)],
      score_values[index_of(delta)],
      scale = scale, hastings = hastings
    )
    counts[index_of(delta)] <- counts[index_of(delta)] + 1
  }
  list(empirical = counts / n_steps, target = toy_target(p_pairs, score_values, scale))
}

test_that("mh_log_acceptance combines score, prior and proposal terms", {
  # no score difference, symmetric situation (t = 1/2, same normalization): acceptance 1
  expect_equal(mh_log_acceptance(0, 0.5, 1, 1, scale = 1, hastings = TRUE), 0)
  expect_equal(mh_log_acceptance(2, 0.5, 1, 1, scale = 0.5, hastings = FALSE), 1)
  # the prior ratio of a flip is t / (1 - t)
  expect_equal(mh_log_acceptance(0, 0.8, 1, 1, hastings = FALSE), log(0.8 / 0.2))
  # a pair that is always proposed (t = 1) cannot be proposed back
  expect_equal(mh_log_acceptance(0, 1, 1, 1, hastings = TRUE), -Inf)
  # Hastings correction: q_rev / q_fwd = ((1 - t) / z_new) / (t / z)
  expect_equal(
    mh_log_acceptance(0, 0.3, 1.5, 1.9, hastings = TRUE),
    log(0.3 / 0.7) + log((0.7 / 1.9) / (0.3 / 1.5))
  )
})

test_that("with the Hastings correction the chain samples prior * exp(scale * score)", {
  for (scale in c(1, 0.5)) {
    chain <- run_toy_chain(scale, hastings = TRUE)
    expect_lt(max(abs(chain$empirical - chain$target)), 0.02)
  }
})

test_that("without the Hastings correction the chain is biased (documents the thesis rule)", {
  chain <- run_toy_chain(scale = 1, hastings = FALSE)
  expect_gt(max(abs(chain$empirical - chain$target)), 0.02)
})

test_that("by default the chain samples the posterior (half the score, Hastings correction)", {
  set.seed(1)
  p_pairs <- c(0.7, 0.2, 0.5)
  score_values <- c(0.5, -1, 1.5, -0.5, 0, 1, -2, 2)
  index_of <- function(delta) 1L + delta[1, 2] + 2L * delta[1, 3] + 4L * delta[2, 3]
  p_delta <- matrix(0, 3, 3)
  p_delta[1, 2] <- p_pairs[1]
  p_delta[1, 3] <- p_pairs[2]
  p_delta[2, 3] <- p_pairs[3]
  delta <- matrix(0, 3, 3)
  counts <- numeric(8)
  for (i in 1:40000) {
    delta <- metropolis_hastings_step(delta, p_delta, function(candidate) score_values[index_of(candidate)], score_values[index_of(delta)])
    counts[index_of(delta)] <- counts[index_of(delta)] + 1
  }
  expect_lt(max(abs(counts / 40000 - toy_target(p_pairs, score_values, 0.5))), 0.02)
})

test_that("the prior of the interactions is kept away from 0 and 1 by default", {
  set.seed(1)
  n <- 600
  x <- data.frame(a = runif(n), b = runif(n), c = runif(n))
  y <- rbinom(n, 1, plogis(8 * ((x$a > 0.5) != (x$b > 0.5)) - 4)) # the (a, b) interaction is saturated
  train <- rep(TRUE, n)

  old <- options(glmdisc.mh_clamp = 0)
  raw <- glmdisc:::initialize_interaction(3, x, y, train)
  options(old)
  expect_equal(raw$p_delta[1, 2], 1)

  clamped <- glmdisc:::initialize_interaction(3, x, y, train)
  upper <- upper.tri(clamped$p_delta)
  expect_true(all(clamped$p_delta[upper] >= 0.01 & clamped$p_delta[upper] <= 0.99))
  expect_equal(clamped$p_delta[1, 2], 0.99)
  # entries outside the upper triangle stay 0 so that those pairs are never proposed
  expect_true(all(clamped$p_delta[!upper] == 0))
})
