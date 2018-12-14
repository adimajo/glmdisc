#' Prediction on a raw test set of the best logistic regression model on discretized / grouped data.
#'
#' This function discretizes a user-provided test dataset given a discretization scheme provided by an S4 \code{\link{glmdisc}} object.
#' It then applies the learnt logistic regression model and outputs its prediction (see \code{\link{predict.glm}}).
#' @name discretize
#' @rdname discretize
#' @exportMethod discretize
#' @description This defines the generic method "discretize" which will discretize a new input dataset given a discretization scheme of S4 class \code{\link{glmdisc}}.

methods::setGeneric("discretize", function(object,...) attributes(object))


#' Method for discretizing a new input dataset given a discretization scheme of class \code{\link{glmdisc}}.
#' @rdname discretize
#' @name discretize
#' @aliases discretize,glmdisc-method
# #' @description This defines the method "discretize" which will discretize a new input dataset given a discretization scheme of S4 class \code{\link{glmdisc}}

methods::setMethod("discretize", methods::signature(object="glmdisc"), function(object,data) {
     glmdisc:::discretize_link(object@best.disc[[2]],data,object@parameters$m_start)
})
