#' @include allClasses.R
NULL

#' Prediction on a raw test set of the best logistic regression model on discretized data.
#'
#' This function discretizes a user-provided test dataset given a discretization scheme provided by an S4 "glmdisc" object.
#' It then applies the learnt logistic regression model and outputs its prediction (see \code{\link{predict.glm}}).
#' @exportMethod predict
#' @concept test discretization predict prediction
#' @docType methods
#' @name predict
#' @aliases predict
#' @description This defines the method "discretize" which will discretize a new input dataset given a discretization scheme of S4 class \code{\link{glmdisc}}
#' @param ... Essai
methods::setGeneric("predict")

#' Prediction on a raw test set of the best logistic regression model on discretized data.
#' 
#' @rdname predict
#' @aliases predict,glmdisc,ANY,ANY-method
#' @param object The S4 discretization object.
#' @param predictors The test dataframe to discretize and for which we wish to have predictions.
#' @examples
#' # Simulation of a discretized logit model
#' set.seed(1)
#' x = matrix(runif(300), nrow = 100, ncol = 3)
#' cuts = seq(0,1,length.out= 4)
#' xd = apply(x,2, function(col) as.numeric(cut(col,cuts)))
#' theta = t(matrix(c(0,0,0,2,2,2,-2,-2,-2),ncol=3,nrow=3))
#' log_odd = rowSums(t(sapply(seq_along(xd[,1]), function(row_id) sapply(seq_along(xd[row_id,]),
#' function(element) theta[xd[row_id,element],element]))))
#' y = rbinom(100,1,1/(1+exp(-log_odd)))
#'
#' sem_disc <- glmdisc(x,y,iter=50,m_start=4,test=FALSE,validation=FALSE,criterion="aic")
#' predict(sem_disc, data.frame(x))
predict.glmdisc <- function(object, predictors) {
     data_disc = data.frame(discretize_link(object@best.disc[[2]],predictors,object@parameters$m_start), stringsAsFactors=TRUE)
     #colnames(data_disc) = colnames(predictors)
     
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
               warning(paste("levels", paste(levels(data_e[,var])[(!levels(data_e[,var]) %in% unlist(unname(object@parameters$encoder$lvls[var])))], collapse = ", "), "of feature", var, "were removed from test set."))
          }
     }

     data = predict(object = object@parameters$encoder, newdata = data_e)
     
     predictlogisticRegression(data,object@best.disc[[1]]$coefficients)
}

#' Prediction on a raw test set of the best logistic regression model on discretized data.
#'
#' This function discretizes a user-provided test dataset given a discretization scheme provided by an S4 "glmdisc" object.
#' It then applies the learnt logistic regression model and outputs its prediction (see \code{\link{predict.glm}}).
#' @rdname predict
#' @name predict,glmdisc-method
#' @aliases predict,glmdisc-method
#' @description This defines the method "predict" which will predict the discretization of a new input dataset given a discretization scheme of S4 class \code{\link{glmdisc}}
methods::setMethod("predict", "glmdisc", predict.glmdisc)