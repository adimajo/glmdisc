context("test-glmdisc")

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
})
