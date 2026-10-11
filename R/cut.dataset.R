# #' Randoming and sampling a dataset by its row numbers
# #'
# #' This function chooses the rows of a dataset to be included in the train, test (optional) and validation (optional) datasets according to proportions specified by the user.
# #' @param n The total number of rows to sample from.
# #' @param test TRUE if a test dataset is wanted, FALSE otherwise (default=TRUE).
# #' @param validation TRUE if a test dataset is wanted, FALSE otherwise (default=TRUE).
# #' @param proportions The list of the (2) proportions wanted for test and validation set. Only the first is used when there is only one of either test or validation that is set to TRUE. Produces an error when the sum is greater to one. Useless if both test and validation are set to FALSE. Default: list(0.2,0.2).
# #' @param seed The seed for the random number generator (optional).
# #' @keywords sample, test, train, validation

# #' @examples

# #' # We randomly separate 30 observations in 40\% of training, 30\% of test and 30\% of validation.
# #' cut_dataset(n=30,test=TRUE,
# #' validation=TRUE,proportions=c(0.3,0.3),seed=1)

cut_dataset <- function(n, proportions, test = TRUE, validation = TRUE) {
  if (test == TRUE) {
    if (validation == TRUE) {
      if (tryCatch(length(proportions) < 2 | proportions[1] <= 0 | proportions[2] <= 0 | sum(proportions) >= 1, error = function(e) TRUE)) {
        stop(simpleError("Argument proportions should contain 2 positive arguments which sum should be less than 1"))
      }
      n_test <- floor(proportions[1] * n)
      n_validation <- floor((proportions[1] + proportions[2]) * n) - n_test
      check_set_sizes(n, n_test, n_validation)
      ind_train <- sample.int(n, n)
      ind_test <- ind_train[seq_len(n_test)]
      ind_validation <- ind_train[n_test + seq_len(n_validation)]
      ind_train <- ind_train[-seq_len(n_test + n_validation)]
      return(list(ind_train, ind_test, ind_validation))
    } else {
      return(cut_one_set(n, proportions))
    }
  } else {
    if (validation == TRUE) {
      return(cut_one_set(n, proportions))
    } else {
      return(list(sample.int(n, n), NULL, NULL))
    }
  }
}

cut_one_set <- function(n, proportions) {
  if (tryCatch(length(proportions) < 1 | proportions[1] <= 0 | proportions[1] >= 1, error = function(e) TRUE)) {
    stop(simpleError("Argument proportions should contain 1 argument strictly between 0 and 1"))
  }
  n_held_out <- floor(proportions[1] * n)
  check_set_sizes(n, n_held_out)
  ind <- sample.int(n, n)
  list(ind[-seq_len(n_held_out)], ind[seq_len(n_held_out)], NULL)
}

check_set_sizes <- function(n, ...) {
  sizes <- c(...)
  if (any(sizes < 1) || n - sum(sizes) < 1) {
    stop(simpleError("The dataset is too small for the requested proportions: every set needs at least one observation."))
  }
}
