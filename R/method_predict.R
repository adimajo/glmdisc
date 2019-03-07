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
     
     # Features with only one level are out of the model
     #data_e = Filter(function(x)(length(unique(x))>1),data_disc)
     data_e = data_disc
     
     # # With the exception of features that had more than one level on the training set and only one in the test set and were discarded at the preceding step
     # new_df = matrix(1,ncol=length(object@parameters$encoder$facVars[!object@parameters$encoder$facVars %in% colnames(data_e)]), nrow = nrow(data_e))
     # new_df = as.data.frame(lapply(as.data.frame(new_df), factor))
     # colnames(new_df) = object@parameters$encoder$facVars[!object@parameters$encoder$facVars %in% colnames(data_e)]
     # data_e <- cbind(data_e,new_df)
     
     
     # Levels not in the training set but in the test set are removed
     for (var in object@parameters$encoder$facVars) {
          if (length(levels(data_e[,var])[!(levels(data_e[,var]) %in% unlist(unname(object@parameters$encoder$lvls[var])))]) > 0) {
               data_e <- data_e[-which(data_e[,var] == levels(data_e[,var])[!(levels(data_e[,var]) %in% unlist(unname(object@parameters$encoder$lvls[var])))]),]
               print(paste("levels", paste(levels(data_e[,var])[(!levels(data_e[,var]) %in% unlist(unname(object@parameters$encoder$lvls[var])))], collapse = ", "), "of feature", var, "were removed from test set."))
          }
     }

     data = predict(object = object@parameters$encoder, newdata = data_e)
     
     #print(ncol(data))
     #print(length(object@best.disc[[1]]$coefficients))
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
