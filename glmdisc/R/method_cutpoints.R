#' Method for obtaining the cutpoints and / or regroupments of a discretization of class \code{\link{glmdisc}}.
# #' @rdname cutpoints-methods
#' @name cutpoints
#' @aliases cutpoints,cutpoints-method
#' @exportMethod cutpoints
#' @description This defines the method "cutpoints" which will output the cutpoints found by the S4 class \code{\link{glmdisc}}.

methods::setGeneric("cutpoints", function(object,...) attributes(object))


# #' Method for obtaining the cutpoints and / or regroupments of a discretization of class \code{\link{glmdisc}}.
# #' @rdname cutpoints-methods
# #' @name cutpoints
# #' @aliases cutpoints,cutpoints-method
# #' @description This defines the method "cutpoints" which will output the cutpoints found by the S4 class \code{\link{glmdisc}}.

methods::setMethod("cutpoints", methods::signature(object="glmdisc"), function(object) {
     for (j in ncol(object@disc.data-1)) {
          cbind(object@disc.data[,j],object@cont.data[,j])
          
     }
})
