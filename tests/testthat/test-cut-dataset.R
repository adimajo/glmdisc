test_that("cut_dataset works with only validation", {
  set.seed(1)
  test_de_cut <- cut_dataset(20, c(0.1), test = F, validation = T)
  expect_type(test_de_cut[[1]], "integer")
  expect_type(test_de_cut[[2]], "integer")
  expect_null(test_de_cut[[3]])
})

test_that("cut_dataset works with only test", {
  set.seed(1)
  test_de_cut <- cut_dataset(20, c(0.1), test = T, validation = F)
  expect_type(test_de_cut[[1]], "integer")
  expect_type(test_de_cut[[2]], "integer")
  expect_null(test_de_cut[[3]])
})

test_that("cut_dataset errors with test and validation and only one proportion", {
  set.seed(1)
  expect_error(cut_dataset(20, c(0.1), test = T, validation = T))
})

test_that("cut_dataset works with test and validation", {
  set.seed(1)
  test_de_cut <- cut_dataset(20, c(0.1, 0.1), test = T, validation = T)
  expect_type(test_de_cut[[1]], "integer")
  expect_type(test_de_cut[[2]], "integer")
  expect_type(test_de_cut[[3]], "integer")
})

test_that("cut_dataset works with no test and no validation", {
  set.seed(1)
  test_de_cut1 <- cut_dataset(20, c(0.1, 0.1), test = F, validation = F)
  test_de_cut2 <- cut_dataset(20, c(0.1), test = F, validation = F)
  test_de_cut3 <- cut_dataset(20, test = F, validation = F)
  expect_type(test_de_cut1[[1]], "integer")
  expect_type(test_de_cut2[[1]], "integer")
  expect_type(test_de_cut3[[1]], "integer")
  expect_null(test_de_cut1[[2]])
  expect_null(test_de_cut2[[2]])
  expect_null(test_de_cut3[[2]])
  expect_null(test_de_cut1[[3]])
  expect_null(test_de_cut2[[3]])
  expect_null(test_de_cut3[[3]])
})

test_that("cut_dataset errors with some proportion values", {
  set.seed(1)
  expect_error(cut_dataset(20, c(-0.1), test = T, validation = F), "Argument proportions should contain 1 argument strictly between 0 and 1", fixed = TRUE)
  expect_error(cut_dataset(20, c(1.1), test = T, validation = F), "Argument proportions should contain 1 argument strictly between 0 and 1", fixed = TRUE)

  expect_error(cut_dataset(20, c(-0.1), test = F, validation = T), "Argument proportions should contain 1 argument strictly between 0 and 1", fixed = TRUE)
  expect_error(cut_dataset(20, c(1.1), test = F, validation = T), "Argument proportions should contain 1 argument strictly between 0 and 1", fixed = TRUE)
})
