methods::setMethod("show",
                   methods::signature(object = "glmdisc"),
                   function(object) {
                        methods::show(object@best.disc[[1]])
                   })


print.glmdisc <- function(object) {
     print(object@best.disc[[1]])
}


summary.glmdisc <- function(object) {
     summary(object@best.disc[[1]])
}

plot.glmdisc <- function(object) {
     graphics::plot(object@best.disc[[1]])
}


#' Prediction on a raw test set of the best logistic regression model on discretized data.
#'
#' This function discretizes a user-provided test dataset given a discretization scheme provided by an S4 "glmdisc" object.
#' It then applies the learnt logistic regression model and outputs its prediction (see \code{\link{predict.glm}}).
#' @exportMethod predict
#' @param object The S4 discretization object.
#' @param predictors The test dataframe to discretize and for which we wish to have predictions.
#' @keywords test, discretization, predict, prediction

methods::setGeneric("predict")

predict.glmdisc <- function(object, predictors) {
     predict(object@best.disc[[1]],data.frame(discretize_link(object@best.disc[[2]],predictors)))
}

#' Method for predicting on a new input dataset given a discretization scheme and its associated model of class \code{\link{glmdisc}}.
#' @rdname predict-methods
#' @name predict
#' @aliases predict,glmdisc-method
#' @description This defines the method "discretize" which will discretize a new input dataset given a discretization scheme of S4 class \code{\link{glmdisc}}

methods::setMethod("predict", "glmdisc", predict.glmdisc)

# #' @name discretize
# #' @rdname discretize-methods
#' @exportMethod discretize
# #' @description This defines the generic method "discretize" which will discretize a new input dataset given a discretization scheme of S4 class \code{\link{discretization}}.

methods::setGeneric("discretize", function(object,...) attributes(object))


#' Method for discretizing a new input dataset given a discretization scheme of class \code{\link{glmdisc}}.
#' @rdname discretize-methods
#' @name discretize
#' @aliases discretize,glmdisc-method
#' @description This defines the method "discretize" which will discretize a new input dataset given a discretization scheme of S4 class \code{\link{glmdisc}}

methods::setMethod("discretize", methods::signature(object="glmdisc"), function(object,data) {
     # if (substr(object@method.name,1,3)=="sem") {
     glmdisc:::discretize_link(object@best.disc[[2]],data)
     # } else {
          # discretize_cutp(data,object@best.disc[[2]]@Disc.data,object@parameters[[1]])
     # }
})


