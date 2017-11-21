methods::setMethod("show",
                   methods::signature(object = "discretization"),
                   function(object) {
                        methods::show(object@best.disc[[1]])
                   })


print.discretization <- function(object) {
     print(object@best.disc[[1]])
}


summary.discretization <- function(object) {
     summary(object@best.disc[[1]])
}


#' Prediction on a raw test set of the best logistic regression model on discretized data.
#'
#' This function discretizes a user-provided test dataset given a discretization scheme provided by an S4 "discretization" object.
#' It then applies the learnt logistic regression model and outputs its prediction (see predict.glm).
#' @param object The S4 discretization object.
#' @param predictors The test dataframe to discretize and for which we wish to have predictions.
#' @keywords test, discretization, predict, prediction

predict.discretization <- function(object, predictors) {
     predict(object@best.disc[[1]],as.data.frame(discretize_link(object@best.disc[[2]],predictors)))
}


#' Constructor method "discretize" for discretization objects
#'
#' @name discretize
#' @rdname discretize-methods
#' @exportMethod discretize
#' @description This defines the generic method "discretize" which will discretize a new input dataset given a discretization scheme of S4 class discretization.

methods::setGeneric("discretize", function(object,...) attributes(object))


#' Method for discretizing a new input dataset given a discretization scheme of class "discretization".
#' @rdname discretize-methods
#' @name discretize
#' @aliases discretize,discretization-method
#' @description This defines the method "discretize" which will discretize a new input dataset given a discretization scheme of S4 class discretization.

methods::setMethod("discretize", methods::signature(object="discretization"), function(object,data) {
     # if (substr(object@method.name,1,3)=="sem") {
     glmdisc:::discretize_link(object@best.disc[[2]],data)
     # } else {
          # discretize_cutp(data,object@best.disc[[2]]@Disc.data,object@parameters[[1]])
     # }
})


#' Different kinds of plots using either plotly (if available) or the standard plot (graphics package).
#'
#' This function aims at producing useful graphs in the context of credit scoring in order to simplify the validation process
#' of the produced credit score.
#' @param discretization The S4 discretization object.
#' @param type The test dataframe to discretize and for which we wish to have predictions.
#' @param ... See additional parameters of plotly (if installed) or the standard plot function (from the graphics package).
#' @keywords test, discretization, predict, prediction

