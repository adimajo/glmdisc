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
#' @author Adrien Ehrhardt, Vincent Vandewalle, Christophe Biernacki, Philippe Heinrich.
#' @docType package
#' @importFrom Rcpp evalCpp
#' @useDynLib glmdisc
#' @name glmdisc-package
NULL
