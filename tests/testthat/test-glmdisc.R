context("test-glmdisc")

test_that("glmdisc works for one-dimensional continuous data", {
     x <- matrix(runif(300), nrow = 300, ncol = 1)
     cuts <- seq(0, 1, length.out = 4)
     xd <- apply(x, 2, function(col)
          as.numeric(cut(col, cuts)))
     theta <- c(0, 2, -2)
     log_odd <- t(sapply(seq_along(xd[, 1]), function(row_id)
          theta[xd[row_id, 1]]))
     y <- rbinom(300, 1, 1 / (1 + exp(-log_odd)))
     
     for (criterion in c('aic', 'bic', 'gini')) {
          for (test in c(TRUE, FALSE)) {
               for (validation in c(TRUE, FALSE)) {
                    sem_disc <- glmdisc(
                         x,
                         y,
                         iter = 20,
                         m_start = 4,
                         test = test,
                         validation = validation,
                         criterion = criterion,
                         interact = FALSE
                    )
                    expect_s4_class(sem_disc, "glmdisc")
                    expect_equal(sem_disc@parameters$iter, 20)
                    expect_equal(sem_disc@parameters$m_start, 4)
                    expect_equal(sem_disc@parameters$test, test)
                    expect_equal(sem_disc@parameters$validation,
                                 validation)
                    expect_equal(sem_disc@parameters$criterion,
                                 criterion)
                    expect_equal(sem_disc@parameters$reg_type, "poly")
                    expect_equal(sem_disc@parameters$types_data,
                                 "numeric")
                    expect_s3_class(sem_disc@parameters$encoder,
                                    "dummyVars")
               }
          }
     }
})

test_that("glmdisc works for multi-dimensional continuous data", {
     x <- matrix(runif(300), nrow = 100, ncol = 3)
     cuts <- seq(0, 1, length.out = 4)
     xd <- apply(x, 2, function(col)
          as.numeric(cut(col, cuts)))
     theta <-
          t(matrix(
               c(0, 0, 0, 2, 2, 2,-2,-2,-2),
               ncol = 3,
               nrow = 3
          ))
     log_odd <-
          rowSums(t(sapply(seq_along(xd[, 1]), function(row_id)
               sapply(seq_along(xd[row_id,]), function(element)
                    theta[xd[row_id, element], element]))))
     y <- rbinom(100, 1, 1 / (1 + exp(-log_odd)))
     for (criterion in c('aic', 'bic', 'gini')) {
          for (test in c(TRUE, FALSE)) {
               for (validation in c(TRUE, FALSE)) {
                    for (interact in c(TRUE, FALSE)) {
                         sem_disc <- glmdisc(
                              x,
                              y,
                              iter = 50,
                              m_start = 4,
                              test = test,
                              validation = validation,
                              criterion = criterion,
                              interact = interact
                         )
                         expect_s4_class(sem_disc, "glmdisc")
                         expect_equal(sem_disc@parameters$iter, 50)
                         expect_equal(sem_disc@parameters$m_start, 4)
                         expect_equal(sem_disc@parameters$test, test)
                         expect_equal(sem_disc@parameters$validation, validation)
                         expect_equal(sem_disc@parameters$criterion, criterion)
                         expect_equal(sem_disc@parameters$interact, interact)
                         expect_equal(sem_disc@parameters$reg_type, "poly")
                         expect_equal(sem_disc@parameters$types_data, c("numeric", "numeric", "numeric"))
                         expect_s3_class(sem_disc@parameters$encoder, "dummyVars")
                    }
               }
          }
     }
})

test_that("glmdisc works for one-dimensional categorical data", {
     x <- matrix(runif(300), nrow = 300, ncol = 1)
     cuts <- seq(0, 1, length.out = 4)
     xd <- apply(x, 2, function(col)
          as.numeric(cut(col, cuts)))
     theta <- c(0, 2, -2)
     log_odd <- t(sapply(seq_along(xd[, 1]), function(row_id)
          theta[xd[row_id, 1]]))
     y <- rbinom(300, 1, 1 / (1 + exp(-log_odd)))
     
     xd = data.frame(xd = factor(xd))
     
     for (criterion in c('aic', 'bic', 'gini')) {
          for (test in c(TRUE, FALSE)) {
               for (validation in c(TRUE, FALSE)) {
                    sem_disc <- glmdisc(
                         xd,
                         y,
                         iter = 50,
                         m_start = 4,
                         test = test,
                         validation = validation,
                         criterion = criterion,
                         interact = FALSE
                    )
                    expect_s4_class(sem_disc, "glmdisc")
                    expect_equal(sem_disc@parameters$iter, 50)
                    expect_equal(sem_disc@parameters$m_start, 4)
                    expect_equal(sem_disc@parameters$test, test)
                    expect_equal(sem_disc@parameters$validation, test)
                    expect_equal(sem_disc@parameters$criterion, criterion)
                    expect_false(sem_disc@parameters$interact)
                    expect_equal(sem_disc@parameters$reg_type, "poly")
                    expect_equal(sem_disc@parameters$types_data, "factor")
                    expect_s3_class(sem_disc@parameters$encoder, "dummyVars")
               }
          }
     }
})

test_that("glmdisc works for multi-dimensional categorical data", {
     x <- matrix(runif(300), nrow = 100, ncol = 3)
     cuts <- seq(0, 1, length.out = 4)
     xd <- apply(x, 2, function(col)
          as.numeric(cut(col, cuts)))
     theta <- matrix(c(0, 0, 0, 
                       2, 2, 2,
                       -2, -2, -2), ncol = 3, nrow = 3)
     log_odd <- sapply(seq_along(xd[, 1]),
                       function(row_id)
                            sapply(seq_along(xd[row_id,]),
                                   function(element)
                                        theta[xd[row_id, element], element]))
     y <- rbinom(100, 1, 1 / (1 + exp(-log_odd)))
     
     xd = data.frame(apply(xd, 2, factor), stringsAsFactors = TRUE)
     
     for (criterion in c('aic', 'bic', 'gini')) {
          for (test in c(TRUE, FALSE)) {
               for (validation in c(TRUE, FALSE)) {
                    for (interact in c(TRUE, FALSE)) {
                         sem_disc <- glmdisc(
                              xd,
                              y,
                              iter = 50,
                              m_start = 4,
                              test = test,
                              validation = validation,
                              criterion = criterion,
                              interact = interact
                         )
                         expect_s4_class(sem_disc, "glmdisc")
                         expect_equal(sem_disc@parameters$iter, 50)
                         expect_equal(sem_disc@parameters$m_start, 4)
                         expect_equal(sem_disc@parameters$test, test)
                         expect_equal(sem_disc@parameters$validation, validation)
                         expect_equal(sem_disc@parameters$criterion, criterion)
                         expect_equal(sem_disc@parameters$interact, interact)
                         expect_equal(sem_disc@parameters$reg_type, "poly")
                         expect_true(all(sem_disc@parameters$types_data == c("factor", "factor", "factor")))
                         expect_s3_class(sem_disc@parameters$encoder, "dummyVars")
                    }
               }
          }
     }
})

test_that("glmdisc works for multi-dimensional mixed data", {
     x <- matrix(runif(300), nrow = 100, ncol = 3)
     cuts <- seq(0, 1, length.out = 4)
     xd <- apply(x, 2, function(col)
          as.numeric(cut(col, cuts)))
     theta <- matrix(c(0, 0, 0, 
                       2, 2, 2,
                       -2, -2, -2), ncol = 3, nrow = 3)
     log_odd <- sapply(seq_along(xd[, 1]),
                       function(row_id)
                            sapply(seq_along(xd[row_id,]),
                                   function(element)
                                        theta[xd[row_id, element], element]))
     y <- rbinom(100, 1, 1 / (1 + exp(-log_odd)))
     
     xd = data.frame(apply(xd, 2, factor), stringsAsFactors = TRUE)
     
     x2 = data.frame(x, xd)
     
     for (criterion in c('aic', 'bic', 'gini')) {
          for (test in c(TRUE, FALSE)) {
               for (validation in c(TRUE, FALSE)) {
                    for (interact in c(TRUE, FALSE)) {
                         sem_disc <- glmdisc(
                              x2,
                              y,
                              iter = 50,
                              m_start = 4,
                              test = test,
                              validation = validation,
                              criterion = criterion,
                              interact = interact
                         )
                         expect_s4_class(sem_disc, "glmdisc")
                         expect_equal(sem_disc@parameters$iter, 50)
                         expect_equal(sem_disc@parameters$m_start, 4)
                         expect_equal(sem_disc@parameters$test, test)
                         expect_equal(sem_disc@parameters$validation, validation)
                         expect_equal(sem_disc@parameters$criterion, criterion)
                         expect_equal(sem_disc@parameters$interact, interact)
                         expect_equal(sem_disc@parameters$reg_type, "poly")
                         expect_true(all(sem_disc@parameters$types_data == c("numeric", "numeric", "numeric", "factor", "factor", "factor")))
                         expect_s3_class(sem_disc@parameters$encoder, "dummyVars")
                    }
               }
          }
     }
})

test_that("glmdisc errors for type of input data", {
     expect_error(glmdisc("toto", "tztz", iter = 50, m_start = 4, test = FALSE, validation = FALSE, criterion = "aic"))
     
     set.seed(1)
     x <- matrix(runif(300), nrow = 100, ncol = 3)
     cuts <- seq(0, 1, length.out = 4)
     xd <- apply(x, 2, function(col) as.numeric(cut(col, cuts)))
     theta <- t(matrix(c(0, 0, 0, 2, 2, 2, -2, -2, -2), ncol = 3, nrow = 3))
     log_odd <- rowSums(t(sapply(seq_along(xd[, 1]), function(row_id) sapply(seq_along(xd[row_id, ]), function(element) theta[xd[row_id, element], element]))))
     y <- rbinom(100, 1, 1 / (1 + exp(-log_odd)))
     x[, 1] <- as.integer(x[, 1])
     
     expect_error(glmdisc(x, y, iter = 50, m_start = 4, test = FALSE, validation = FALSE, criterion = "aic"))
     
     expect_error(glmdisc(x[, 2, drop=FALSE], y, iter = 50, m_start = 4, interact = TRUE, test = FALSE, validation = FALSE, criterion = "aic"))

     expect_error(glmdisc(x[1:50, ], y, iter = 50, m_start = 4, interact = FALSE, test = FALSE, validation = FALSE, criterion = "aic"))
     
     expect_error(glmdisc(x, y, iter = 50, m_start = 4, interact = FALSE, test = FALSE, validation = FALSE, criterion = "toto"))
     
     expect_error(glmdisc(x, y, iter = 50, m_start = 4, interact = FALSE, test = FALSE, validation = "toto", criterion = "aic"))
})

test_that("glmdisc warns for type of input data", {
     expect_error(glmdisc("toto", "tztz", iter = 50, m_start = 4, test = FALSE, validation = FALSE, criterion = "aic"))
     
     set.seed(1)
     x <- matrix(runif(300), nrow = 100, ncol = 3)
     cuts <- seq(0, 1, length.out = 4)
     xd <- apply(x, 2, function(col) as.numeric(cut(col, cuts)))
     theta <- t(matrix(c(0, 0, 0, 2, 2, 2, -2, -2, -2), ncol = 3, nrow = 3))
     log_odd <- rowSums(t(sapply(seq_along(xd[, 1]), function(row_id) sapply(seq_along(xd[row_id, ]), function(element) theta[xd[row_id, element], element]))))
     y <- rbinom(100, 1, 1 / (1 + exp(-log_odd)))
     
     expect_warning(glmdisc(x, y, iter = 50, m_start = 4, interact = FALSE, test = FALSE, validation = FALSE, criterion = "gini"),
                    "Using Gini index on training set might yield an overfitted model.",
                    fixed=TRUE)
     
     expect_warning(glmdisc(x, y, iter = 50, m_start = 4, interact = FALSE, test = FALSE, validation = TRUE, criterion = "aic"),
                    "No need to penalize the log-likelihood when a validation set is used. Using log-likelihood instead of AIC/BIC.",
                    fixed=TRUE)
     expect_warning(glmdisc(x, y, iter = 50, m_start = 4, interact = FALSE, test = FALSE, validation = TRUE, criterion = "bic"),
                    "No need to penalize the log-likelihood when a validation set is used. Using log-likelihood instead of AIC/BIC.",
                    fixed=TRUE)
     
     glmdisc(x, y, iter = 50, m_start = 4, interact = FALSE, test = FALSE, validation = FALSE, criterion = "bic")
     
     x[1, 1] <- NA
     
     glmdisc(x, y, iter = 50, m_start = 4, interact = FALSE, test = FALSE, validation = TRUE, criterion = "gini")
})


test_that("glmdisc m_start > nlevels categorical data", {
     set.seed(1)
     x <- matrix(runif(300), nrow = 100, ncol = 3)
     cuts <- seq(0, 1, length.out = 4)
     xd <- apply(x, 2, function(col) as.numeric(cut(col, cuts)))
     theta <- t(matrix(c(0, 0, 0, 2, 2, 2, -2, -2, -2), ncol = 3, nrow = 3))
     log_odd <- rowSums(t(sapply(seq_along(xd[, 1]), function(row_id) sapply(seq_along(xd[row_id, ]), function(element) theta[xd[row_id, element], element]))))
     y <- rbinom(100, 1, 1 / (1 + exp(-log_odd)))
     xdata = data.frame(x.1 = factor(xd[, 1]), x.2 = factor(xd[, 2]), x.3 = factor(xd[, 3]))
     glmdisc(xdata, y, iter = 50, m_start = 6, interact = FALSE, test = FALSE, validation = FALSE, criterion = "aic")
})

test_that("glmdisc stops early", {
     set.seed(1)
     x <- matrix(runif(300), nrow = 100, ncol = 3)
     cuts <- seq(0, 1, length.out = 4)
     xd <- apply(x, 2, function(col) as.numeric(cut(col, cuts)))
     theta <- t(matrix(c(0, 0, 0, 2, 2, 2, -2, -2, -2), ncol = 3, nrow = 3))
     log_odd <- rowSums(t(sapply(seq_along(xd[, 1]), function(row_id) sapply(seq_along(xd[row_id, ]), function(element) theta[xd[row_id, element], element]))))
     y <- rbinom(100, 1, 1 / (1 + exp(-log_odd)))
     glmdisc(x, y, iter = 2000, m_start = 3, interact = FALSE, test = FALSE, validation = FALSE, criterion = "aic")
})
