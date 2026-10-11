stand_in <- function(types, factor_levels) {
  methods::new("glmdisc",
    parameters = list(types_data = types, factor_levels = factor_levels),
    best.disc = list(), performance = list(), disc.data = data.frame(), cont.data = data.frame()
  )
}

test_that("prepare_features classifies the columns", {
  set.seed(1)
  X <- data.frame(
    n = runif(6), i = 1:6, f = factor(c("a", "b", "a", "b", "a", "b")),
    s = c("u", "v", "u", "v", "u", "v"), l = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)
  )
  prepared <- prepare_features(X)
  expect_equal(prepared$types, c("numeric", "numeric", "factor", "factor", "factor"))
  expect_true(all(vapply(prepared$X[prepared$types == "factor"], is.factor, logical(1))))
  expect_equal(levels(prepared$X$s), c("u", "v"))
})

test_that("prepare_features accepts matrices and rejects unsupported columns", {
  set.seed(1)
  expect_equal(prepare_features(matrix(runif(6), 3, 2))$types, c("numeric", "numeric"))
  expect_error(prepare_features(data.frame(d = as.Date("2020-01-01") + 0:2, x = 1:3)), "Unsupported data types")
})

test_that("missing values of a factor become a level of their own", {
  set.seed(1)
  prepared <- prepare_features(data.frame(f = factor(c("a", NA, "b", "a"))))
  expect_equal(levels(prepared$X$f), c("a", "b", NA_LEVEL))
  expect_equal(as.character(prepared$X$f[2]), NA_LEVEL)
  # no missing value: no extra level
  expect_equal(levels(prepare_features(data.frame(f = factor(c("a", "b"))))$X$f), c("a", "b"))
})

test_that("prepare_new_features uses the training levels and types", {
  set.seed(1)
  object <- stand_in(c("numeric", "factor"), list(NULL, c("a", "b")))
  X <- prepare_new_features(object, data.frame(x = c(0.1, 0.2), f = c("b", "a")))
  expect_equal(levels(X$f), c("a", "b"))
  expect_equal(as.character(X$f), c("b", "a"))
})

test_that("prepare_new_features rejects new data that do not match the training data", {
  set.seed(1)
  object <- stand_in(c("numeric", "factor"), list(NULL, c("a", "b")))
  expect_error(prepare_new_features(object, data.frame(x = 0.1)), "Not the same number of columns")
  expect_error(prepare_new_features(object, data.frame(x = "z", f = "a")), "Not the same data types")
  expect_error(prepare_new_features(object, data.frame(x = 0.1, f = "c")), "Unseen")
  # a missing value is only accepted when it was seen in training
  expect_error(prepare_new_features(object, data.frame(x = 0.1, f = NA_character_)), "Unseen")
  with_na <- stand_in(c("numeric", "factor"), list(NULL, c("a", NA_LEVEL)))
  expect_equal(as.character(prepare_new_features(with_na, data.frame(x = 0.1, f = NA_character_))$f), NA_LEVEL)
})
