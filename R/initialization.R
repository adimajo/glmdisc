first_argument_checks <- function(criterion, labels, predictors, validation, verbose, reg_type = "poly", iter = 1L, m_start = 2L, patience = NULL) {
  if (!criterion %in% c("gini", "aic", "bic")) {
    stop(simpleError("criterion must be gini, aic or bic"))
  }
  if (!(is.matrix(predictors) || is.data.frame(predictors))) {
    stop(simpleError(paste0("`predictors` must be an array, a matrix or a dataframe, you provided a ", class(predictors)[1L])))
  }
  if (!length(labels) == nrow(predictors)) {
    stop(simpleError("labels and predictors must be of same length"))
  }
  if (!all(labels %in% c(0, 1))) {
    stop(simpleError("`labels` must be a binary vector of 0 and 1"))
  }
  if (!reg_type %in% c("poly", "polr")) {
    stop(simpleError("reg_type must be poly or polr"))
  }
  if (!(is.numeric(iter) && length(iter) == 1L && iter >= 1)) {
    stop(simpleError("`iter` must be a positive integer"))
  }
  if (!(is.numeric(m_start) && length(m_start) == 1L && m_start >= 2)) {
    stop(simpleError("`m_start` must be an integer greater than 1"))
  }
  if (!is.null(patience) && !(is.numeric(patience) && length(patience) == 1L && patience >= 1)) {
    stop(simpleError("`patience` must be NULL or a positive integer"))
  }
  if ((criterion == "gini") & (validation == FALSE)) {
    if (verbose) warning("Using Gini index on training set might yield an overfitted model.")
  }

  if ((criterion %in% c("aic", "bic")) & (validation == TRUE)) {
    if (verbose) warning("No need to penalize the log-likelihood when a validation set is used. Using log-likelihood instead of AIC/BIC.")
  }
}


second_argument_checks <- function(d, interact) {
  if (d < 2 && interact) {
    stop(simpleError("Cannot perform interaction screening with less than 2 features, set `interact = FALSE`"))
  }
}
