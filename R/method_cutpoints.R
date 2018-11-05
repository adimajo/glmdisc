#' Method for obtaining the cutpoints and / or regroupments of a discretization of class \code{\link{glmdisc}}.
# #' @rdname cutpoints-methods
#' @name cutpoints
#' @aliases cutpoints,cutpoints-method
#' @exportMethod cutpoints
#' @description This defines the method "cutpoints" which will output the cutpoints found by the S4 class \code{\link{glmdisc}}.

methods::setGeneric("cutpoints", function(object,...) attributes(object))


#' Method for obtaining the cutpoints and / or regroupments of a discretization of class \code{\link{glmdisc}}.
#' @rdname cutpoints-methods
#' @name cutpoints
#' @aliases cutpoints,cutpoints-method
#' @description This defines the method "cutpoints" which will output the cutpoints found by the S4 class \code{\link{glmdisc}}.

methods::setMethod("cutpoints", methods::signature(object="glmdisc"), function(object) {
     
     cutpoints = list()
     
     for (j in which(object@parameters$types_data=="factor")) {
          cutpoints[[j]] = apply(prop.table(object@best.disc$bestLinkFunction[[j]],2),2,which.max)
     }
               
     for (j in which(object@parameters$types_data=="numeric")) {
          data_disc = data.frame(disc = glmdisc@disc.data[,j],cont = glmdisc@cont.data[,j])
          
          cut1 = aggregate(cont ~ disc, data = data_disc, min)
          cut2 = aggregate(cont ~ disc, data = data_disc, max)
          
          cut1 = cut1[order(cut1$cont),]
          cut2 = cut2[order(cut2$cont),]
          
          cut1 = cut1[-1,]
          cut2 = cut2[-nrow(cut2),]
          
          cutpoints[[j]] = rowMeans(cbind(cut1$cont,cut2$cont))
     }
     
     names(cutpoints) = colnames(object@disc.data)[-ncol(object@disc.data)]
     
     return(cutpoints)
})
