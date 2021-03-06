are_fast_packages_installed <- function(verbose = FALSE) {
  installed_NPflow <- requireNamespace("NPflow", quietly = TRUE)
  installed_Rfast <- requireNamespace("Rfast", quietly = TRUE)
  if (verbose & !installed_NPflow) message("Package NPflow not installed. Cannot use 'fast = TRUE' .")
  if (verbose & !installed_Rfast) message("Package Rfast not installed. Cannot use 'fast = TRUE' .")
  return(installed_Rfast & installed_NPflow)
}

#' glmdisc: A package for discretizing continuous features, grouping categorical features' values and optimizing it for logistic regression.
#'
#' The glmdisc package provides two important functions:
#' glmdisc and its associate method discretize.
#'
#' @section \code{\link{glmdisc}} function:
#' The \code{\link{glmdisc}} function discretizes a training set using an SEM-Gibbs based method.
#'
#'
#' @section \code{\link{discretize}} function:
#' The \code{\link{discretize}} function will discretize a new input dataset given a discretization scheme of S4 class \code{\link{glmdisc}}.
#'
#'
#' @section \code{\link{cutpoints}} function:
#' The \code{\link{cutpoints}} function will provide the cutpoints / groupings of a discretization scheme of S4 class \code{\link{glmdisc}} in a list.
#'
#'
#' @section \code{\link{predict}} function:
#' The \code{\link{predict}} function will discretize a raw test set, given a provided discretization scheme of S4 class \code{\link{glmdisc}}, using the \code{\link{discretize}} function and return the predicted probabilities.
#'
#'
#' @section Miscellaneous:
#' We provide as well the classical \code{\link{show}}, \code{\link{print}}, \code{\link{summary}} functions, as well as \code{\link{normalizedGini}} that is used to calculate the Gini index (a classical Credit Scoring goodness-of-fit indicator).
#'
#'
#' @author Adrien Ehrhardt.
#' @docType package
#' @importFrom Rcpp evalCpp
#' @useDynLib glmdisc
#' @name glmdisc-package
NULL
