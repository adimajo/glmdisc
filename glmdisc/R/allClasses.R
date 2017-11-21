

#' Class discretization.
#'
#' Class \code{discretization} represents a discretization scheme.
#'
#' @slot parameters The parameters associated with the used method.
#' @slot best.disc The best discretization scheme found by the method given its parameters.
#' @slot performance The performance obtained with the method given its parameters.
#' @slot disc.data The discretized data: test set if test is TRUE; if test is FALSE and validation is TRUE, then it provides the discretized validation set. Otherwise, it provides the discretized training set.
#' @slot disc.data The continuous data: test set if test is TRUE; if test is FALSE and validation is TRUE, then it provides the discretized validation set. Otherwise, it provides the discretized training set.
#' @name discretization-class
#' @rdname discretization-class

methods::setClass("discretization", representation(parameters = "list", best.disc = "list", performance = "list", disc.data = "data.frame", cont.data = "data.frame"))
