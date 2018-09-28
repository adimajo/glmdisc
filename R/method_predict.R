#' Prediction on a raw test set of the best logistic regression model on discretized data.
#'
#' This function discretizes a user-provided test dataset given a discretization scheme provided by an S4 "glmdisc" object.
#' It then applies the learnt logistic regression model and outputs its prediction (see \code{\link{predict.glm}}).
#' @exportMethod predict
#' @param object The S4 discretization object.
#' @param predictors The test dataframe to discretize and for which we wish to have predictions.
#' @keywords test, discretization, predict, prediction
#' @aliases predict,glmdisc-method
#' @name predict
# #' @rdname predict-methods
#' @description This defines the method "discretize" which will discretize a new input dataset given a discretization scheme of S4 class \code{\link{glmdisc}}

methods::setGeneric("predict")

predict.glmdisc <- function(object, predictors) {
     data_disc = data.frame(discretize_link(object@best.disc[[2]],predictors,object@parameters$m_start))
     colnames(data_disc) = colnames(predictors)
     
     data_e = Filter(function(x)(length(unique(x))>1),data_disc)
     data = stats::model.matrix(object@best.disc$formulaOfBestLogisticRegression, data = data_e)
     predictlogisticRegression(data,object@best.disc[[1]]$coefficients)
}

# #' Prediction on a raw test set of the best logistic regression model on discretized data.
# #'
# #' This function discretizes a user-provided test dataset given a discretization scheme provided by an S4 "glmdisc" object.
# #' It then applies the learnt logistic regression model and outputs its prediction (see \code{\link{predict.glm}}).
# #' @rdname predict-methods
# #' @name predict
# #' @aliases predict,glmdisc-method
# #' @description This defines the method "discretize" which will discretize a new input dataset given a discretization scheme of S4 class \code{\link{glmdisc}}

methods::setMethod("predict", "glmdisc", predict.glmdisc)
