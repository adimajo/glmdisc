link_for <- function(x, e, labels = sort(unique(e))) {
  link <- fit_numeric_link(x, e, rep(TRUE, length(x)), labels, na_label = 99L)
  link$S <- labels
  link$relabel <- stats::setNames(as.character(seq_along(labels)), labels)
  link$out_levels <- unname(link$relabel)
  link
}

test_that("discretize_link names the levels of a numeric feature", {
  set.seed(1)
  x <- runif(300)
  e <- as.integer(cut(x, seq(0, 1, length.out = 4)))
  res <- discretize_link(list(link_for(x, e)), data.frame(x = x))
  expect_type(res, "character")
  expect_length(res, 300)
  expect_equal(nlevels(factor(res)), 3)
})

test_that("discretize_link works for several features and both regression types", {
  set.seed(1)
  x <- matrix(runif(360), nrow = 120, ncol = 3)
  # noisy labels: with perfectly separated classes polr cannot be fitted
  e <- apply(x, 2, function(col) as.integer(cut(col + rnorm(120, sd = 0.1), c(-Inf, 1 / 3, 2 / 3, Inf))))
  for (reg in c("poly", "polr")) {
    links <- lapply(1:3, function(j) {
      link <- fit_numeric_link(x[, j], e[, j], rep(TRUE, 120), 1:3, na_label = 99L, reg_type = reg)
      link$S <- 1:3
      link$relabel <- c("1" = "1", "2" = "2", "3" = "3")
      link$out_levels <- c("1", "2", "3")
      link
    })
    res <- discretize_link(links, as.data.frame(x))
    expect_equal(dim(res), c(120, 3))
    for (j in 1:3) expect_equal(nlevels(factor(res[, j])), 3)
  }
})

test_that("discretize_link works for factor features", {
  set.seed(1)
  x <- sample(1:4, 200, replace = TRUE)
  e <- ifelse(x %in% 1:2, 1L, 2L)
  link <- fit_factor_link(x, e, rep(TRUE, 200), 1:2, 4L, as.character(1:4))
  link$S <- 1:2
  link$relabel <- c("1" = "1", "2" = "2")
  link$out_levels <- c("1", "2")
  res <- discretize_link(list(link), data.frame(x = factor(x)))
  expect_equal(res[, 1], as.character(e))
})

test_that("discretize_link outputs a single level for a constant feature", {
  link <- constant_link(3L)
  link$relabel <- c("3" = "1")
  link$out_levels <- "1"
  expect_equal(unname(discretize_link(list(link), data.frame(x = 1:5))[, 1]), rep("1", 5))
})
