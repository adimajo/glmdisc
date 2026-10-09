#' Calculating the normalized Gini index
#'
#' This function calculates the Gini index of a classification rule outputting probabilities. It is a classical metric in the context of Credit Scoring.
#' It is equal to 2 times the AUC (Area Under ROC Curve) minus 1; tied predicted probabilities are given half credit, so the result does not depend on the order of the observations.
#' @param actual The numeric binary vector of the actual labels observed.
#' @param predicted The vector of the probabilities predicted by the classification rule.
#' @concept gini index
#' @export
#' @author Adrien Ehrhardt
#' @examples
#' normalizedGini(c(1, 1, 1, 0, 0), c(0.7, 0.9, 0.5, 0.6, 0.3))
normalizedGini <- function(actual, predicted) {
  if (length(actual) != length(predicted)) stop("Actual and Predicted need to be equal lengths!")
  n_pos <- sum(actual)
  n_neg <- length(actual) - n_pos
  # Mann-Whitney formulation of the AUC; midranks handle tied scores
  auc <- (sum(rank(predicted)[actual == 1]) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
  2 * auc - 1
}
