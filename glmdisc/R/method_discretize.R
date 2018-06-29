#' Prediction on a raw test set of the best logistic regression model on discretized data.
#'
#' This function discretizes a user-provided test dataset given a discretization scheme provided by an S4 "glmdisc" object.
#' It then applies the learnt logistic regression model and outputs its prediction (see \code{\link{predict.glm}}).
#' @name discretize
#' @rdname discretize-methods
#' @exportMethod discretize
#' @description This defines the generic method "discretize" which will discretize a new input dataset given a discretization scheme of S4 class \code{\link{discretization}}.

methods::setGeneric("discretize", function(object,...) attributes(object))


# #' Method for discretizing a new input dataset given a discretization scheme of class \code{\link{glmdisc}}.
# #' @rdname discretize-methods
# #' @name discretize
# #' @aliases discretize,glmdisc-method
# #' @description This defines the method "discretize" which will discretize a new input dataset given a discretization scheme of S4 class \code{\link{glmdisc}}

methods::setMethod("discretize", methods::signature(object="glmdisc"), function(object,data) {
     # if (substr(object@method.name,1,3)=="sem") {
     glmdisc:::discretize_link(object@best.disc[[2]],data)
     # } else {
     # discretize_cutp(data,object@best.disc[[2]]@Disc.data,object@parameters[[1]])
     # }
})
