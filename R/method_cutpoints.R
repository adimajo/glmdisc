#' Obtaining the cutpoints and / or regroupments of a discretization.
#' 
#' The method returns a list of cutpoints and / or regroupments of a discretization of class \code{\link{glmdisc}}.
#' @name cutpoints
#' @rdname cutpoints
#' @exportMethod cutpoints
#' @description This defines the generic method "cutpoints" which will provide the cutpoints of a discretization scheme of S4 class \code{\link{glmdisc}}.

methods::setGeneric("cutpoints", function(object,...) attributes(object))

#' Obtaining the cutpoints and / or regroupments of a discretization.
#' 
#' @rdname cutpoints
#' @name cutpoints
#' @aliases cutpoints,glmdisc-method
# #' @description This defines the method to provide the cutpoints of a trained glmdisc.

methods::setMethod("cutpoints", methods::signature(object="glmdisc"), function(object) {
     
     cutpoints = list()
     
     for (j in which(object@parameters$types_data=="factor")) {
          cutpoints[[j]] = apply(prop.table(object@best.disc$bestLinkFunction[[j]],2),2,which.max)
     }
               
     for (j in which(object@parameters$types_data=="numeric")) {
          data_disc = data.frame(disc = glmdisc@disc.data[,j],cont = glmdisc@cont.data[,j])
          
          cut1 = stats::aggregate(cont ~ disc, data = data_disc, min)
          cut2 = stats::aggregate(cont ~ disc, data = data_disc, max)
          
          cut1 = cut1[order(cut1$cont),]
          cut2 = cut2[order(cut2$cont),]
          
          cut1 = cut1[-1,]
          cut2 = cut2[-nrow(cut2),]
          
          cutpoints[[j]] = rowMeans(cbind(cut1$cont,cut2$cont))
     }
     
     names(cutpoints) = colnames(object@disc.data)[-ncol(object@disc.data)]
     
     return(cutpoints)
})
