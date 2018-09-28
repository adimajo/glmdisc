#' Plots for the discretized data.
#'
#' @exportMethod plot
#' @param object The S4 discretization object.
#' @keywords test, discretization, plot
#' @importFrom gam s

if (!isGeneric("plot")) {
     methods::setGeneric("plot", function(x,y,...) standardGeneric("plot"))
}

plot.glmdisc <- function(x) {
     
     # Graph 1 : ROC CURVE
     
     glm_simple_roc = simple_roc(x@disc.data$labels,predict(x,x@cont.data[,-ncol(x@cont.data)]))
     
     with(glm_simple_roc, graphics::plot(1 - FPR, TPR, col=1 + labels, xlim=c(1,0)))
     title(main = "ROC Curve on total provided data")
     lines(c(1,0),c(0,1))
     legend(c(0.4,0.4),c(paste("Gini:",round(normalizedGini(x@cont.data$labels,predict(x,x@cont.data[,-ncol(x@cont.data)])),2))))
     
     #installed_graphical_package(object@best.disc$bestLogisticRegression)
     par(ask=TRUE)
     
     # Group of Graphs 2 : risk levels for each discretized attribute
     for (j in 1:(ncol(x@disc.data)-1)) {
          graphics::spineplot(factor(x@disc.data[,j]),factor(x@disc.data$labels),main=paste("Risk levels of attribute",colnames(x@disc.data)[j]),xlab = "Discretized / grouped feature", ylab = "Class proportions")
     }
     
     # Group of Graphs 3 : continuous features + glmnet
     #par(ask=FALSE)
     for (j in which(x@parameters$types_data=="numeric")) {
          modele_gam = gam::gam(as.formula(paste("labels ~",paste("s(",gsub("+", ",5) + s(",as.character(x@best.disc$formulaOfBestLogisticRegression)[2], fixed=TRUE),",5)"))), data = x@cont.data, family = "binomial")
          gam::plot.gam(modele_gam,ask=FALSE,terms=paste0("s(",colnames(x@disc.data)[j],", 5)"), se=TRUE, main = paste("Plot of a GAM fit to attribute",colnames(x@disc.data)[j]))
          par(ask=TRUE)
     }
     #par(ask=TRUE)
     
     # Group of Graphs 4 : categorical features significance
     for (j in which(x@parameters$types_data=="factor")) {
          print(table(factor(x@disc.data[,j]),factor(x@cont.data[,j])))
     }
     
     on.exit(par(ask=FALSE))
}

# #' Method for predicting on a new input dataset given a discretization scheme and its associated model of class \code{\link{glmdisc}}.
# #' @rdname plot-methods
# #' @name plot
# #' @aliases plot,glmdisc-method
# #' @description This defines the method "plot" which will plot some useful graphs for the discretization scheme of S4 class \code{\link{glmdisc}}

methods::setMethod(f = "plot", signature = c(x="glmdisc",y="missing"), definition = plot.glmdisc)



