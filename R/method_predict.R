#' @include allClasses.R
NULL

#' Prediction on a raw test set of the best logistic regression model on discretized data
#'
#' This function discretizes a user-provided test dataset given a discretization scheme provided by an S4 "glmdisc" object.
#' It then applies the learnt logistic regression model and outputs its prediction (see \code{\link[stats]{predict.glm}}).
#' @exportMethod predict
#' @concept test discretization predict prediction
#' @docType methods
#' @name predict
#' @aliases predict
#' @description This defines the method "predict" which discretizes a new input dataset given a discretization scheme of S4 class \code{\link{glmdisc}} and returns the predicted probabilities of class 1.
#' @param ... Not used.
methods::setGeneric("predict")

#' @rdname predict
#' @aliases predict,glmdisc-method
#' @param object The S4 discretization object.
#' @param predictors The test dataframe to discretize and for which we wish to have predictions.
#' @return A numeric vector with the predicted probability of class 1 for each row of \code{predictors}.
#' @examples
#' # Simulation of a discretized logit model
#' set.seed(1)
#' x <- matrix(runif(300), nrow = 100, ncol = 3)
#' cuts <- seq(0, 1, length.out = 4)
#' xd <- apply(x, 2, function(col) as.numeric(cut(col, cuts)))
#' theta <- t(matrix(c(0, 0, 0, 2, 2, 2, -2, -2, -2), ncol = 3, nrow = 3))
#' log_odd <- rowSums(t(sapply(seq_along(xd[, 1]), function(row_id) {
#'   sapply(
#'     seq_along(xd[row_id, ]),
#'     function(element) theta[xd[row_id, element], element]
#'   )
#' })))
#' y <- rbinom(100, 1, 1 / (1 + exp(-log_odd)))
#'
#' sem_disc <- glmdisc(x, y,
#'   iter = 50, m_start = 4, test = FALSE,
#'   validation = FALSE, criterion = "aic"
#' )
#' predict(sem_disc, data.frame(x))
methods::setMethod("predict", "glmdisc", function(object, predictors, ...) {
  X <- prepare_new_features(object, predictors)
  quantized <- quantized_frame(object@best.disc[[2]], X, object@parameters$model_names)
  as.numeric(suppressWarnings(stats::predict(object@best.disc[[1]], newdata = quantized, type = "response")))
})
