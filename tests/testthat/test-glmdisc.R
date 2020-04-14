test_that("glmdisc works for one-dimensional continuous data", {
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
                    sem_disc <- glmdisc(
                         x,
                         y,
                         iter = 50,
                         m_start = 4,
                         test = test,
                         validation = validation,
                         criterion = criterion
                    )
                    expect_s4_class(sem_disc, "glmdisc")
                    expect_equal(sem_disc@parameters$iter, 50)
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
     sem_disc <- glmdisc(
          x,
          y,
          iter = 50,
          m_start = 4,
          test = FALSE,
          validation = FALSE,
          criterion = "aic"
     )
     expect_s4_class(sem_disc, "glmdisc")
     expect_equal(sem_disc@parameters$iter, 50)
     expect_equal(sem_disc@parameters$m_start, 4)
     expect_false(sem_disc@parameters$test)
     expect_false(sem_disc@parameters$validation)
     expect_equal(sem_disc@parameters$criterion, "aic")
     expect_false(sem_disc@parameters$reg_type, "poly")
     expect_equal(sem_disc@parameters$types_data, "numeric")
     expect_s3_class(sem_disc@parameters$encoder, "dummyVars")
})

test_that("glmdisc works for one-dimensional categorical data", {
     x <- matrix(runif(300), nrow = 100, ncol = 1)
     cuts <- seq(0, 1, length.out = 4)
     xd <- as.numeric(cut(x, cuts))
     xd <- t(t(xd))
     theta <- matrix(c(0, 2,-2), ncol = 1, nrow = 3)
     log_odd <- sapply(seq_along(xd[, 1]),
                       function(row_id)
                            sapply(seq_along(xd[row_id,]),
                                   function(element)
                                        theta[xd[row_id, element], element]))
     y <- rbinom(100, 1, 1 / (1 + exp(-log_odd)))
     sem_disc <- glmdisc(
          x,
          y,
          iter = 50,
          m_start = 4,
          test = FALSE,
          validation = FALSE,
          criterion = "aic",
          interact = FALSE
     )
     expect_s4_class(sem_disc, "glmdisc")
     expect_equal(sem_disc@parameters$iter, 50)
     expect_equal(sem_disc@parameters$m_start, 4)
     expect_false(sem_disc@parameters$test)
     expect_false(sem_disc@parameters$validation)
     expect_equal(sem_disc@parameters$criterion, "aic")
     expect_false(sem_disc@parameters$reg_type, "poly")
     expect_equal(sem_disc@parameters$types_data, "numeric")
     expect_s3_class(sem_disc@parameters$encoder, "dummyVars")
})

test_that("glmdisc works for multi-dimensional categorical data", {
     
})

test_that("glmdisc works for multi-dimensional mixed data", {
     
})
