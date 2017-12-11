#' Model-based multivariate discretization for logistic regression.
#'
#' This function discretizes a training set using an SEM-Gibbs based method (see References section).
#' It detects numerical features of the dataset and discretizes them ; values of categorical features (of type \code{factor}) are regrouped. This is done in a multivariate supervised way. Assessment of the correct model is done via AIC, BIC or test set error (see parameter \code{criterion}).
#' @param predictors The matrix array containing the numerical or factor attributes to discretize.
#' @param labels The actual labels of the provided predictors (0/1).
#' @param validation Boolean : True if the algorithm should use predictors to construct a validation set on which to search for the best discretization scheme using the provided criterion (default: TRUE).
#' @param test Boolean : True if the algorithm should use predictors to construct a test set on which to calculate the provided criterion using the best discretization scheme (chosen thanks to the provided criterion on either the test set (if true) or the training set (otherwise)) (default: TRUE).
#' @param criterion The criterion ('gini','aic','bic') to use to choose the best discretization scheme among the generated ones (default: 'gini'). Nota Bene: it is best to use 'gini' only when test is set to TRUE and 'aic' or 'bic' when it is not. When using 'aic' or 'bic' with a test set, the likelihood is returned as there is no need to penalize for generalization purposes.
#' @param iter The number of iterations to do in the SEM protocole (default: 1000).
#' @param m_start The maximum number of resulting categories for each variable wanted (default: 20).
#' @param reg_type The model to use between discretized and continuous features (currently, only multinomial logistic regression ('poly') and ordered logistic regression ('polr') are supported ; default: 'poly'). WARNING: 'poly' requires the \code{mnlogit} package, 'polr' requires the \code{MASS} package.
#' @param seed For reproducibility purposes (default: 1).
#' @keywords SEM, Gibbs, discretization
#' @author Adrien Ehrhardt, Vincent Vandewalle, Christophe Biernacki, Philippe Heinrich.
#' @seealso \code{\link{glm}}, \code{\link{multinom}}, \code{\link{polr}}
#' @details
#' This function finds the most appropriate discretization scheme for logistic regression. When provided with a continuous variable \eqn{X}, it tries to convert it to a categorical variable \eqn{E} which values uniquely correspond to intervals of the continuous variable \eqn{X}.
#' When provided with a categorical variable \eqn{X}, it tries to find the best regroupement of its values and subsequently creates categorical variable \eqn{E}. The goal is to perform supervised learning with logistic regression so that you have to specify a target variable \eqn{Y} denoted by \code{labels}.
#' The ‘‘discretization'' process, i.e. the transformation of \eqn{X} to \eqn{E} is done so as to achieve the best logistic regression model \eqn{p(y|e;\theta)}. It can be interpreted as a special case feature engineering algorithm.
#' Subsequently, its outputs are: the optimal discretization scheme and the logistic regression model associated with it. We also provide the parameters that were provided to the function and the evolution of the criterion with respect to the algorithm's iterations.
#' @importFrom stats predict
#' @export
#' @references
#' Celeux, G., Chauveau, D., Diebolt, J. (1995), On Stochastic Versions of the EM Algorithm. [Research Report] RR-2514, INRIA. 1995. <inria-00074164>
#'
#' Asad Hasan, Wang Zhiyu and Alireza S. Mahani (2015). mnlogit: Multinomial Logit Model. R package version 1.2.4. \url{https://CRAN.R-project.org/package=mnlogit}
#'
#' Agresti, A. (2002) \emph{Categorical Data}. Second edition. Wiley.
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
#' discretize(sem_disc,data.frame(x))


glmdisc <- function(predictors,labels,validation=TRUE,test=TRUE,criterion='gini',iter=1000,m_start=20,reg_type='poly',seed=1) {

     if (criterion %in% c('gini','aic','bic')) {
          if (length(labels)==length(predictors[,1])) {

               # Calculating lengths n and d and data types
               n = length(labels)
               d = length(predictors[1,])
               types_data <- sapply(predictors[1,], class)

               if (sum(!(types_data %in% c("numeric","factor")))>0) {
                    stop("Unsupported data types. Columns of predictors must be numeric or factor.")
               }

               # Initializing list of calculated criterion among which to select the best.
               criterion_iter=list()

               # Obtain training, test and validation datasets.
               ensemble <- cut_dataset(n,test=test,validation=validation)

               # Initializing variable E (discretization of X) at random.
               e = emap = array(0,c(n,d))
               if (sum(types_data=="numeric")>0) {
                    e[,which(types_data=="numeric")] = emap[,which(types_data=="numeric")] = sapply(which(types_data=="numeric"),function(i) as.factor(sample(1:m_start,n,replace = TRUE)))
               }
               if (sum(types_data=="factor")>0) {
                    e[,which(types_data=="factor")] = emap[,which(types_data=="factor")] = sapply(which(types_data=="factor"),function(i) as.factor(sample(1:nlevels(predictors[,i]),n,replace = TRUE)))
               }

               m = rep(m_start,d)
               m[which(types_data=="factor")] = as.vector(sapply(predictors[,which(types_data=="factor")],nlevels))
               names(m) <- paste("X", 1:length(m), sep = "")

               # Initializing "current" best logistic regression and link functions.
               current_best = 1
               best_reglog = 0
               best_link = 0

               if ((criterion=='gini') & (validation==FALSE)) {
                    warning("Using Gini index on training set might yield an overfitted model.")
               }

               if ((criterion %in% c('aic','bic')) & (validation==TRUE)) {
                    warning("No need to penalize the log-likelihood when a validation set is used. Using log-likelihood instead of AIC/BIC.")
               }

               # SEM algorithm
               for (i in 1:iter){

                    # Dataframe with e and emaps
                    # data = data.frame(e,labels = labels)
                    # data_logit = data.frame(emap,labels = labels)

                    data_e = Filter(function(x)(length(unique(x))>1),data.frame(e))
                    data_emap = Filter(function(x)(length(unique(x))>1),data.frame(emap))

                    data = stats::model.matrix(stats::as.formula(paste("~",paste(colnames(data_e),collapse = "+"))), data = data_e)
                    data_logit = stats::model.matrix(stats::as.formula(paste("~",paste(colnames(data_emap),collapse = "+"))), data = data_emap)

                    # p(y|e) and p(y|emap) training
                    # model_reglog = tryCatch((stats::glm(labels~.,family = stats::binomial(link = "logit"), data=Filter(function(x)(length(unique(x))>1),data_logit[ensemble[[1]],]), y=FALSE, model=FALSE,start=model_reglog$coefficients)),error=function(cond) (stats::glm(labels~.,family = stats::binomial(link = "logit"), data=Filter(function(x)(length(unique(x))>1),data_logit[ensemble[[1]],]), y=FALSE, model=FALSE)))
                    if (exists("model_reglog")) {
                         model_reglog = RcppNumerical::fastLR(data_logit[ensemble[[1]],],labels[ensemble[[1]]],start = model_reglog$coefficients)
                    } else {
                         model_reglog = RcppNumerical::fastLR(data_logit[ensemble[[1]],],labels[ensemble[[1]]])
                    }

                    # logit = tryCatch((stats::glm(labels ~.,family = stats::binomial(link = "logit"), data=Filter(function(x)(length(unique(x))>1),data[ensemble[[1]],]), y=FALSE, model=FALSE,start=logit$coefficients)),error=function(cond) (stats::glm(labels ~ .,family = stats::binomial(link = "logit"), data=Filter(function(x)(length(unique(x))>1),data[ensemble[[1]],]), y=FALSE, model=FALSE)))
                    if (exists("logit")) {
                         logit = RcppNumerical::fastLR(data[ensemble[[1]],],labels[ensemble[[1]]],start = logit$coefficients)
                    } else {
                         logit = RcppNumerical::fastLR(data[ensemble[[1]],],labels[ensemble[[1]]])
                    }


                    # Calculate current performance and update (if better than previous best) current best model.
                    if ((criterion=='gini')&&(validation==FALSE)) {
                         criterion_iter[[i]] = normalizedGini(labels[ensemble[[1]]],predict_fastLR(model_reglog,data_logit[ensemble[[1]],]))
                    } else if ((criterion=='gini')&&(validation==TRUE)) {
                         criterion_iter[[i]] = normalizedGini(labels[ensemble[[2]]],predict_fastLR(model_reglog,data_logit[ensemble[[2]],]))
                    } else if ((criterion=='aic')&&(validation==FALSE)) {
                         criterion_iter[[i]] = 2*model_reglog$loglikelihood-2*length(model_reglog$coefficients)
                    } else if ((criterion=='bic')&&(validation==FALSE)) {
                         criterion_iter[[i]] = 2*model_reglog$loglikelihood-log(length(ensemble[[1]]))*length(model_reglog$coefficients)
                    } else if ((criterion %in% c('aic','bic'))&&(validation==TRUE)) {
                         criterion_iter[[i]] = sum(log(labels[ensemble[[2]]]*predict_fastLR(model_reglog,data_logit[ensemble[[2]],]) + (1-labels[ensemble[[2]]])*(1-labels[ensemble[[2]]]*predict_fastLR(model_reglog,data_logit[ensemble[[2]],]))))
                    } else stop("validation must be boolean!")


                    if (criterion_iter[[i]] >= criterion_iter[[current_best]]) {
                         best_reglog = model_reglog
                         best_link = tryCatch(link,error=function(cond) list())
                         current_best = i
                    }

                    # Initialization of link function
                    link=list()

                    # Update E^j with j chosen at random
                    for (j in (d:1)) {

                         # p(e^j | x^j) training
                         if (length(unique(e[ensemble[[1]],j]))>1) {

                              # Possible suppression of values in e^j
                              m[j] = nlevels(as.factor(e[,j]))
                              lev_j <- levels(as.factor(e[,j]))

                              # Polytomic or ordered logistic regression
                              if ((reg_type=='poly')&(types_data[j]=="numeric")) {
                                   # long_dataset <- data.frame(e = as.vector(sapply(e[ensemble[[1]],j],function(var) (lev_j[seq(1:as.numeric(m[j]))]==var))),x = as.vector(sapply(predictors[ensemble[[1]],j], function(var) rep(var,as.numeric(m[j])))), names = as.character(as.vector(rep(lev_j[seq(1:as.numeric(m[j]))],length(ensemble[[1]])))),stringsAsFactors=FALSE)
                                   link[[j]] = nnet::multinom(e ~ x, data=data.frame(e=e[ensemble[[1]],j],x=predictors[ensemble[[1]],j]), start = link[[j]]$coefficients)
                              } else if (types_data[j]=="numeric") {
                                   if (exists("link[[j]]$weights")) {
                                        link[[j]] = MASS::polr(e ~ x, data=data.frame(e = factor(as.numeric(ordered(e[ensemble[[1]],j],levels = names(sort(unlist(by(predictors[ensemble[[1]],j],e[ensemble[[1]],j],mean)))))), ordered=T), x = predictors[ensemble[[1]],j]), Hess = FALSE, model = FALSE, weights = link[[j]]$weights)
                                   } else if (nlevels(as.factor(e[ensemble[[1]],j]))>2) {
                                        link[[j]] = MASS::polr(e ~ x, data=data.frame(e = factor(as.numeric(ordered(e[ensemble[[1]],j],levels = names(sort(unlist(by(predictors[ensemble[[1]],j],e[ensemble[[1]],j],mean)))))), ordered=T), x = predictors[ensemble[[1]],j]), Hess = FALSE, model = FALSE)
                                   } else {
                                        link[[j]] = stats::glm(e ~ x, data=data.frame(e = factor(e[ensemble[[1]],j]), x = predictors[ensemble[[1]],j]), family = stats::binomial(link="logit"), model = FALSE)
                                   }
                              }
                         }

                         # p(y|e^j,e^-j) calculation
                         if (as.numeric(m[j])>1) {
                              y_p = array(0,c(n,as.numeric(m[j])))
                              for (k in 1:as.numeric(m[j])) {
                                   if (j>1) {
                                        indices <- sum(as.numeric(m[1:(j-1)])) - j
                                        if (j<d) {
                                             modalites_k = cbind(data[,1:(indices+2)],matrix(0,nrow = n, ncol = as.numeric(m[j])-1),data[,(indices+2+as.numeric(m[j])):(ncol(data))])
                                        } else {
                                             modalites_k = cbind(data[,1:(indices+2)],matrix(0,nrow = n, ncol = as.numeric(m[j])-1))
                                        }
                                        if (k>1) {
                                             modalites_k[,(1+indices+k)] = rep(1,n)
                                        }
                                   } else {
                                        modalites_k = cbind(rep(1,n),matrix(0,nrow = n, ncol = as.numeric(m[1])-1),data[,(1+as.numeric(m[1])):(ncol(data))])
                                        if (k>1) {
                                             modalites_k[,k] = rep(1,n)
                                        }
                                   }

                                   p = predict_fastLR(logit,modalites_k)

                                   y_p[,k] <- (labels*p+(1-labels)*(1-p))
                              }

                              # p(e^j|reste) calculation
                              if ((types_data[j]=="numeric")) {
                                   t = predict(link[[j]], newdata = data.frame(x = predictors[,j]),type="probs")
                                   if (is.vector(t)) {
                                        t = cbind(1-t,t)
                                   }
                              } else {
                                   link[[j]] = table(e[ensemble[[1]],j],predictors[ensemble[[1]],j])
                                   t = prop.table(t(sapply(predictors[,j],function(row) link[[j]][,row])),1)
                              }

                              # Updating emap^j
                              emap[,j] <- apply(t,1,function(p) names(which.max(p)))

                              t <- prop.table(t*y_p,1)

                              # Updating e^j
                              e[,j] <- apply(t,1,function(p) sample(lev_j,1,prob = p))

                              # We control in test and validation datasets (if applicable) that there is the same values for E than in training set (not more).
                              # Test

                              if (test==TRUE) {

                                   # E

                                   ind_diff_train_test <- which(e[ensemble[[2]],j]==setdiff(factor(e[ensemble[[2]],j]),factor(e[ensemble[[1]],j])))
                                   while (!length(ind_diff_train_test)==0) {
                                        t[ind_diff_train_test,e[ensemble[[2]],j][ind_diff_train_test]] <- 0
                                        t <- prop.table(t,1)
                                        e[ensemble[[2]],j][ind_diff_train_test] <- apply(t[ind_diff_train_test,,drop=FALSE],1,function(p) sample(lev_j,1,prob = p))
                                        ind_diff_train_test <- which(e[ensemble[[2]],j]==setdiff(factor(e[ensemble[[2]],j]),factor(e[ensemble[[1]],j])))
                                   }

                                   # E_MAP

                                   ind_diff_train_test <- which(emap[ensemble[[2]],j]==setdiff(factor(emap[ensemble[[2]],j]),factor(emap[ensemble[[1]],j])))
                                   while (!length(ind_diff_train_test)==0) {
                                        if (reg_type=='poly') {
                                             if (!requireNamespace("nnet", quietly = TRUE)) {
                                                  t = predict(link[[j]], newdata = data.frame(x = predictors[,j]),type="probs")
                                             } else {
                                                  # long_dataset <- data.frame(e = as.vector(sapply(e[,j],function(var) (lev_j[seq(1:as.numeric(m[j]))]==var))),x = as.vector(sapply(predictors[,j], function(var) rep(var,as.numeric(m[j])))), names = as.character(as.vector(rep(lev_j[seq(1:as.numeric(m[j]))],n))))
                                                  t = predict(link[[j]], newdata = data.frame(x = predictors[,j]),type="probs")
                                                  # t[which(is.nan(t),arr.ind = TRUE)[,"row"],which(is.nan(t),arr.ind = TRUE)[,"col"]] = 1
                                             }
                                        } else {
                                             t = tryCatch(predict(link[[j]], newdata = data.frame(x = predictors[,j]),type="probs"), error = function(cond) matrix(c(1-predict(link[[j]], newdata = data.frame(x = predictors[,j]),type="response"),predict(link[[j]], newdata = data.frame(x = predictors[,j]),type="response")),ncol=2,dimnames = list(seq(1:n),c(min(levels(factor(e[ensemble[[1]],j]))),max(levels(factor(e[ensemble[[1]],j])))))))
                                        }
                                        t[ind_diff_train_test,emap[ensemble[[2]],j][ind_diff_train_test]] <- 0
                                        t <- prop.table(t,1)
                                        emap[ensemble[[2]],j][ind_diff_train_test] <- apply(t[ind_diff_train_test,,drop=FALSE],1,function(p) names(which.max(p)))
                                        ind_diff_train_test <- which(emap[ensemble[[2]],j]==setdiff(factor(emap[ensemble[[2]],j]),factor(emap[ensemble[[1]],j])))
                                   }
                              }


                              # Validation

                              if (validation==TRUE) {

                                   # E

                                   ind_diff_train_test <- which(e[ensemble[[3]],j]==setdiff(factor(e[ensemble[[3]],j]),factor(e[ensemble[[1]],j])))
                                   while (!length(ind_diff_train_test)==0) {
                                        t[ind_diff_train_test,e[ensemble[[3]],j][ind_diff_train_test]] <- 0
                                        t <- prop.table(t,1)
                                        e[ensemble[[3]],j][ind_diff_train_test] <- apply(t[ind_diff_train_test,,drop=FALSE],1,function(p) sample(lev_j,1,prob = p))
                                        ind_diff_train_test <- which(e[ensemble[[3]],j]==setdiff(factor(e[ensemble[[3]],j]),factor(e[ensemble[[1]],j])))
                                   }

                                   # E_MAP

                                   ind_diff_train_test <- which(emap[ensemble[[3]],j]==setdiff(factor(emap[ensemble[[3]],j]),factor(emap[ensemble[[1]],j])))
                                   while (!length(ind_diff_train_test)==0) {
                                        if (reg_type=='poly') {
                                             if (!requireNamespace("nnet", quietly = TRUE)) {
                                                  t = predict(link[[j]], newdata = data.frame(x = predictors[,j]),type="probs")
                                             } else {
                                                  # long_dataset <- data.frame(e = as.vector(sapply(e[,j],function(var) (lev_j[seq(1:as.numeric(m[j]))]==var))),x = as.vector(sapply(predictors[,j], function(var) rep(var,as.numeric(m[j])))), names = as.character(as.vector(rep(lev_j[seq(1:as.numeric(m[j]))],n))))
                                                  t = predict(link[[j]], newdata = data.frame(x = predictors[,j]),type="probs")
                                                  # t[which(is.nan(t),arr.ind = TRUE)[,"row"],which(is.nan(t),arr.ind = TRUE)[,"col"]] = 1
                                             }
                                        } else {
                                             t = tryCatch(predict(link[[j]], newdata = data.frame(x = predictors[,j]),type="probs"), error = function(cond) matrix(c(1-predict(link[[j]], newdata = data.frame(x = predictors[,j]),type="response"),predict(link[[j]], newdata = data.frame(x = predictors[,j]),type="response")),ncol=2,dimnames = list(seq(1:n),c(min(levels(factor(e[ensemble[[1]],j]))),max(levels(factor(e[ensemble[[1]],j])))))))
                                        }
                                        t[ind_diff_train_test,emap[ensemble[[3]],j][ind_diff_train_test]] <- 0
                                        t <- prop.table(t,1)
                                        emap[ensemble[[3]],j][ind_diff_train_test] <- apply(t[ind_diff_train_test,,drop=FALSE],1,function(p) names(which.max(p)))
                                        ind_diff_train_test <- which(e[ensemble[[3]],j]==setdiff(factor(e[ensemble[[3]],j]),factor(e[ensemble[[1]],j])))
                                   }
                              }

                         } else {
                              # e^j and emap^j for disappearing features
                              e[,j] <- emap[,j] <- factor(rep(1,n))

                         }
                    }
               }

               # Output preparation and calculation
               best.disc = list(best_reglog,best_link)

               if (validation==TRUE) {
                    if (criterion=="gini") {
                         if (test==TRUE) performance = normalizedGini(labels[ensemble[[3]]],predict(best.disc[[1]],discretize_link(best.disc[[2]],predictors[ensemble[[3]],]),type="response")) else performance = normalizedGini(labels[ensemble[[2]]],predict(best.disc[[1]],discretize_link(best.disc[[2]],predictors[ensemble[[3]],]),type="response"))
                    } else {
                         if (test==TRUE) performance = -2*sum(labels[ensemble[[3]]]*predict(best.disc[[1]],as.data.frame(discretize_link(best.disc[[2]],predictors[ensemble[[3]],])),type="response")+(1-labels[ensemble[[3]]])*(1-predict(best.disc[[1]],as.data.frame(discretize_link(best.disc[[2]],predictors[ensemble[[3]],])),type="response"))) else performance = criterion_iter[[current_best]]
                    }
               } else {
                    if (criterion=="gini") {
                         if (test==TRUE) performance = normalizedGini(labels[ensemble[[2]]],predict(best.disc[[1]],as.data.frame(t(emap[,which.max(criterion_iter),ensemble[[2]]])),type="response")) else performance = normalizedGini(labels[ensemble[[1]]],best.disc[[1]]$fitted.values)
                    } else {
                         if (test==TRUE) performance = -2*rowSums(labels[ensemble[[2]]]*predict(best.disc[[1]],discretize_link(best.disc[[2]],predictors[ensemble[[2]],]),type="response")+(1-labels[ensemble[[2]]])*(1-predict(best.disc[[1]],discretize_link(best.disc[[2]],predictors[ensemble[[2]],]),type="response"))) else performance = criterion_iter[[current_best]]
                    }

               }

               if (test==TRUE) {
                    return(methods::new(Class = "glmdisc", parameters = list(test,validation,criterion,iter,m_start,reg_type), best.disc = best.disc, performance = list(performance,criterion_iter), disc.data = data.frame(cbind(discretize_link(best.disc[[2]],predictors[ensemble[[3]],])),labels = labels[ensemble[[3]]]), cont.data = data.frame(cbind(predictors[ensemble[[3]],]),labels = labels[ensemble[[3]]])))
               } else if (validation==TRUE) {
                    return(methods::new(Class = "glmdisc", parameters = list(test,validation,criterion,iter,m_start,reg_type), best.disc = best.disc, performance = list(performance,criterion_iter), disc.data = data.frame(cbind(discretize_link(best.disc[[2]],predictors[ensemble[[2]],])),labels = labels[ensemble[[2]]]), cont.data = data.frame(cbind(predictors[ensemble[[2]],]),labels = labels[ensemble[[2]]])))
               } else {
                    return(methods::new(Class = "glmdisc", parameters = list(test,validation,criterion,iter,m_start,reg_type), best.disc = best.disc, performance = list(performance,criterion_iter), disc.data = data.frame(cbind(discretize_link(best.disc[[2]],predictors[ensemble[[1]],])),labels = labels[ensemble[[1]]]), cont.data = data.frame(cbind(predictors[ensemble[[1]],]),labels = labels[ensemble[[1]]])))
               }
          }
          else {
               print("labels and predictors must be of same length")
          }
     }
     else {
          print("criterion must be gini, aic or bic")
     }
}
