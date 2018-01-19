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
#' @param interact Boolean : True (default) if interaction detection is wanted (Warning: may be very memory/time-consuming).
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


glmdisc <- function(predictors,labels,interact=TRUE,validation=TRUE,test=TRUE,criterion='gini',iter=1000,m_start=20,reg_type='poly') {

     if (criterion %in% c('gini','aic','bic')) {
          if (length(labels)==length(predictors[,1])) {

               prop.table.robust <- function (x, margin = NULL) {
                    tab <- sweep(x, margin, margin.table(x, margin), "/", check.margin = FALSE)
                    tab[which(is.na(tab))] <- 1/ncol(tab)
                    tab
               }
               
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
               lev = apply(e,2,function(col) list(levels(factor(col))))

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

               if (interact==TRUE) {
                    delta <- matrix(sample(0:1,d^2,replace=TRUE,prob=c(0.5,0.5)),nrow=d,ncol=d)
                    delta[lower.tri(delta)] = 0
                    diag(delta) <- 0
                    xPrincipal <- paste0("X", 1:d)

                    p_delta = matrix(0,nrow=d,ncol=d)
                    p_delta[lower.tri(p_delta)] = 0
                    diag(p_delta) <- 0

                    for (j in 1:(d-1)) {
                         for (k in (j+1):d) {
                              sans_inter <- stats::glm(labels ~ X1 + X2, family=stats::binomial(link="logit"), data=data.frame(labels = labels[ensemble[[1]]],X1 = predictors[ensemble[[1]],j],X2 = predictors[ensemble[[1]],k]))
                              avec_inter <- stats::glm(labels ~ X1 + X2 + X1:X2, family=stats::binomial(link="logit"), data=data.frame(labels = labels[ensemble[[1]]],X1 = predictors[ensemble[[1]],j],X2 = predictors[ensemble[[1]],k]))
                              p_delta[j,k] <- 1/(1+exp(-sans_inter$deviance - log(length(ensemble[[1]]))*length(sans_inter$coefficients) + avec_inter$deviance + log(length(ensemble[[1]]))*length(avec_inter$coefficients)))
                         }
                    }
               }

               # SEM algorithm
               for (i in 1:iter){

                    # if (sum(elementwise.all.equal(m,1))==d) {stop("Early stopping rule: all variables discretized in one value")}

                    data_e = Filter(function(x)(length(unique(x))>1),data.frame(e))
                    data_emap = Filter(function(x)(length(unique(x))>1),data.frame(emap))
                    data = data.frame(e,labels = labels)
                    data_logit = data.frame(emap,labels = labels)

                    if (interact==TRUE) {
                         tab_vrai <- which(delta==1,arr.ind=TRUE)
                         ejecter_logit <- sapply(1:(ncol(data_logit)-1), function(col) length(unique(data_logit[ensemble[[1]],col]))==1)
                         ejecter <- sapply(1:(ncol(data)-1), function(col) length(unique(data[ensemble[[1]],col]))==1)

                         if (sum(tab_vrai)>0) {
                              xInter <- xInter_logit <- sapply(1:nrow(tab_vrai), function(row) paste0("X",tab_vrai[row,"row"],":X",tab_vrai[row,"col"]))

                              if (length(xPrincipal[ejecter])>0) {
                                   # while (length(grep(xPrincipal[ejecter],xInter,value=TRUE))>0) {
                                   #      xInter <- xInter[!(xInter==(grep(xPrincipal[ejecter],xInter,value=TRUE)[1]))]
                                   # }

                                   for (l in xPrincipal[ejecter]) {
                                        xInter <- xInter[!(xInter %in% (grep(l,xInter,value=TRUE)))]
                                   }

                              }

                              if (length(xPrincipal[ejecter_logit])>0) {
                                   # while (length(grep(xPrincipal[ejecter_logit],xInter_logit,value=TRUE))>0) {
                                   #      xInter_logit <- xInter_logit[!(xInter_logit==(grep(xPrincipal[ejecter_logit],xInter_logit,value=TRUE)[1]))]
                                   # }

                                   for (l in xPrincipal[ejecter_logit]) {
                                        xInter_logit <- xInter_logit[!(xInter_logit %in% (grep(l,xInter_logit,value=TRUE)))]
                                   }
                              }

                              if (length(xInter)>0) {
                                   fmla = stats::as.formula(paste("~",paste(xPrincipal[!ejecter],collapse = "+"),"+",paste(xInter,collapse = "+")))
                                   # data = stats::model.matrix(stats::as.formula(paste("~",paste(xPrincipal[!ejecter],collapse = "+"),"+",paste(xInter,collapse = "+"))), data = data_e)
                              } else {
                                   fmla = stats::as.formula(paste("~",paste(xPrincipal[!ejecter],collapse = "+")))
                                   # data = stats::model.matrix(stats::as.formula(paste("~",paste(xPrincipal[!ejecter],collapse = "+"))), data = data_e)
                              }

                              if (length(xInter_logit)>0) {
                                   fmla_logit = stats::as.formula(paste("~",paste(xPrincipal[!ejecter_logit],collapse = "+"),"+",paste(xInter_logit,collapse = "+")))
                                   # data_logit = stats::model.matrix(stats::as.formula(paste("~",paste(xPrincipal[!ejecter_logit],collapse = "+"),"+",paste(xInter_logit,collapse = "+"))), data = data_emap)
                              } else {
                                   fmla_logit = stats::as.formula(paste("~",paste(xPrincipal[!ejecter_logit],collapse = "+")))
                                   # data_logit = stats::model.matrix(stats::as.formula(paste("~",paste(xPrincipal[!ejecter_logit],collapse = "+"))), data = data_emap)
                              }

                              data = stats::model.matrix(fmla,data=data_e)
                              data_logit = stats::model.matrix(fmla_logit,data=data_emap)

                              # if (exists("model_reglog")) {
                              #      model_reglog = RcppNumerical::fastLR(data_logit[ensemble[[1]],],labels[ensemble[[1]]],start = model_reglog$coefficients)
                              # } else {
                              model_reglog = RcppNumerical::fastLR(data_logit[ensemble[[1]],],labels[ensemble[[1]]])
                              # }

                              # if (exists("logit")) {
                              #      logit = RcppNumerical::fastLR(data[ensemble[[1]],],labels[ensemble[[1]]],start = logit$coefficients)
                              # } else {
                              logit = RcppNumerical::fastLR(data[ensemble[[1]],],labels[ensemble[[1]]])
                              # }
                         } else {

                              fmla = stats::as.formula(paste("~",paste(xPrincipal[!ejecter],collapse = "+")))

                              data = stats::model.matrix(fmla, data = data_e)

                              fmla_logit = stats::as.formula(paste("~",paste(xPrincipal[!ejecter_logit],collapse = "+")))

                              data_logit = stats::model.matrix(fmla_logit, data = data_emap)


                              # if (exists("model_reglog")) {
                              #      model_reglog = RcppNumerical::fastLR(data_logit[ensemble[[1]],],labels[ensemble[[1]]],start = model_reglog$coefficients)
                              # } else {
                                   model_reglog = RcppNumerical::fastLR(data_logit[ensemble[[1]],],labels[ensemble[[1]]])
                              # }

                              # if (exists("logit")) {
                              #      logit = RcppNumerical::fastLR(data[ensemble[[1]],],labels[ensemble[[1]]],start = logit$coefficients)
                              # } else {
                                   logit = RcppNumerical::fastLR(data[ensemble[[1]],],labels[ensemble[[1]]])
                              # }
                         }
                    } else {

                         fmla = stats::as.formula(paste("~",paste(colnames(data_e),collapse = "+")))
                         fmla_logit = stats::as.formula(paste("~",paste(colnames(data_emap),collapse = "+")))

                         data = stats::model.matrix(fmla, data = data_e)
                         data_logit = stats::model.matrix(fmla_logit, data = data_emap)

                         model_reglog = RcppNumerical::fastLR(data_logit[ensemble[[1]],],labels[ensemble[[1]]])

                         logit = RcppNumerical::fastLR(data[ensemble[[1]],],labels[ensemble[[1]]])
                         
                    }


                    # Calculate current performance and update (if better than previous best) current best model.
                    if ((criterion=='gini')&&(validation==FALSE)) {
                         criterion_iter[[i]] = normalizedGini(labels[ensemble[[1]]],predictlogisticRegression(data_logit[ensemble[[1]],],model_reglog$coefficients))
                    } else if ((criterion=='gini')&&(validation==TRUE)) {
                         criterion_iter[[i]] = normalizedGini(labels[ensemble[[2]]],predictlogisticRegression(data_logit[ensemble[[2]],],model_reglog$coefficients))
                    } else if ((criterion=='aic')&&(validation==FALSE)) {
                         criterion_iter[[i]] = 2*model_reglog$loglikelihood-2*length(model_reglog$coefficients)
                    } else if ((criterion=='bic')&&(validation==FALSE)) {
                         criterion_iter[[i]] = 2*model_reglog$loglikelihood-log(length(ensemble[[1]]))*length(model_reglog$coefficients)
                    } else if ((criterion %in% c('aic','bic'))&&(validation==TRUE)) {
                         criterion_iter[[i]] = sum(log(labels[ensemble[[2]]]*predictlogisticRegression(data_logit[ensemble[[2]],],model_reglog$coefficients) + (1-labels[ensemble[[2]]])*(1-labels[ensemble[[2]]]*predictlogisticRegression(data_logit[ensemble[[2]],],model_reglog$coefficients))))
                    } else stop("validation must be boolean!")


                    if (criterion_iter[[i]] >= criterion_iter[[current_best]]) {
                         best_reglog = model_reglog
                         best_link = tryCatch(link,error=function(cond) list())
                         current_best = i
                         best_formula = fmla_logit
                    }


                    if (interact==TRUE) {
                         p_delta_transition = abs(delta-p_delta)
                         pq <- sample(1:d^2,prob=prop.table.robust(t(as.matrix(as.vector(p_delta_transition))),1),size=1)
                         delta_new = delta
                         delta_new[pq] = 1-delta_new[pq]
                         if (sum(delta_new==1)>0) {
                              xInter_new <- sapply(1:nrow(which(delta_new==1,arr.ind=TRUE)), function(row) paste0("X",which(delta_new==1,arr.ind=TRUE)[row,"row"],":X",which(delta_new==1,arr.ind=TRUE)[row,"col"]))
                              if (exists("ejecter_logit")) {
                                   if (length(xPrincipal[ejecter_logit])>0) {
                                        # if (length(grep(xPrincipal[ejecter_logit],xInter_new,value=TRUE))>0) {
                                        #      xInter_new <- xInter_new[!(xInter_new==grep(xPrincipal[ejecter_logit],xInter_new,value=TRUE))]
                                        # }

                                        for (l in xPrincipal[ejecter_logit]) {
                                             xInter_new <- xInter_new[!(xInter_new %in% (grep(l,xInter_new,value=TRUE)))]
                                        }

                                   }
                                   if (length(xInter_new)>0) {
                                        data_logit_new = stats::model.matrix(stats::as.formula(paste("~",paste(xPrincipal[!ejecter_logit],collapse = "+"),"+",paste(xInter_new,collapse = "+"))), data = data_emap)
                                   } else {
                                        data_logit_new = stats::model.matrix(stats::as.formula(paste("~",paste(xPrincipal[!ejecter_logit],collapse = "+"))), data = data_emap)
                                   }
                              } else {
                                   data_logit_new = stats::model.matrix(stats::as.formula(paste("~",paste(xPrincipal,collapse = "+"),"+",paste(xInter_new,collapse = "+"))), data = data_emap)
                              }
                         } else {
                              if (exists("ejecter_logit")) {
                                   data_logit_new = stats::model.matrix(stats::as.formula(paste("~",paste(xPrincipal[!ejecter_logit],collapse = "+"))), data = data_emap)
                              } else {
                                   data_logit_new = stats::model.matrix(stats::as.formula(paste("~",paste(xPrincipal,collapse = "+"))), data = data_emap)
                              }

                         }

                         new_logit <- RcppNumerical::fastLR(data_logit_new[ensemble[[1]],],labels[ensemble[[1]]])
                         alpha = exp(2*new_logit$loglikelihood-log(length(ensemble[[1]]))*length(new_logit$coefficients) - (2*model_reglog$loglikelihood-log(length(ensemble[[1]]))*length(model_reglog$coefficients)))*(p_delta_transition[pq])/(1-p_delta_transition[pq])

                         print(alpha)
                         if (sample(c(TRUE,FALSE),size=1,prob = c(min(alpha,1),1-min(alpha,1)))) {
                              delta <- delta_new
                         }
                    }

                    # Initialization of link function
                    link=list()
                    lev_1 = lev
                    m = apply(e,2,function(el) nlevels(as.factor(el)))
                    
                    # Update E^j with j chosen at random
                    for (j in sample(1:d)) {

                         # p(e^j | x^j) training
                         if (length(unique(e[ensemble[[1]],j]))>1) {

                              if (sum(lapply(lapply(1:d,function(j) !lev_1[[j]][[1]] %in% lev[[j]][[1]]),sum)>0)>0) {
                                   e[,which(lapply(lapply(1:d,function(j) !lev_1[[j]][[1]] %in% lev[[j]][[1]]),sum)>0)] = sapply(which(lapply(lapply(1:d,function(j) !lev_1[[j]][[1]] %in% lev[[j]][[1]]),sum)>0), function(col) factor(e[,col],levels = lev_1[[col]][[1]]))
                              }

                              # Polytomic or ordered logistic regression
                              if ((reg_type=='poly')&(types_data[j]=="numeric")) {
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
                         if ((m[j])>1) {
                              y_p = array(0,c(n,(m[j])))
                              for (k in as.numeric(unlist(lev[[j]][[1]]))) {
                                   modalites_k = data
                                   
                                   if (j>1) {
                                        modalites_k[,((3-j+sum((m[1:(j-1)]))):(1-j+sum((m[1:j]))))] = matrix(0,nrow=n,ncol=m[j]-1)
                                   } else {
                                        modalites_k[,(2:((m[1])))] = matrix(0,nrow=n,ncol=(m[j])-1)
                                   }
                                   
                                   if (paste0("X",j,k) %in% colnames(data)) {
                                        modalites_k[,paste0("X",j,k)] = rep(1,n)
                                   }
                                   
                                   p = predictlogisticRegression(modalites_k,logit$coefficients)

                                   y_p[,which(unlist(lev[[j]][[1]])==k)] <- (labels*p+(1-labels)*(1-p))
                              }

                              # p(e^j|reste) calculation
                              if ((types_data[j]=="numeric")) {
                                   t = predict(link[[j]], newdata = data.frame(x = predictors[,j]),type="probs")
                                   if (is.vector(t)) {
                                        t = cbind(1-t,t)
                                   }
                              } else {
                                   link[[j]] = table(e[ensemble[[1]],j],predictors[ensemble[[1]],j])
                                   t = prop.table.robust(t(sapply(predictors[,j],function(row) link[[j]][,row])),1)
                              }

                              # Updating emap^j
                              emap[,j] <- apply(t,1,function(p) names(which.max(p)))

                              t <- prop.table.robust(t*y_p,1)

                              # Updating e^j
                              e[,j] <- apply(t,1,function(p) sample(unlist(lev[[j]][[1]]),1,prob = p))

                              if (nlevels(as.factor(e[,j]))>1) {
                                   if (nlevels(as.factor(e[,j]))==m[j]) {
                                        if (j>1) {
                                             data[,((3-j+sum((m[1:(j-1)]))):(1-j+sum((m[1:j]))))] = stats::model.matrix(stats::as.formula("~e[,j]"),data=data.frame(e[,j]))[,-1]
                                        } else {
                                             data[,(2:(m[1]))] = stats::model.matrix(stats::as.formula("~e[,j]"),data=data.frame(e[,j]))[,-1]
                                        }
                                   } else {
                                        if (which(!lev[[j]][[1]] %in% levels(as.factor(e[,j])))[1]>1) {
                                        
                                             data[,paste0("X",j,lev[[j]][[1]][which(!lev[[j]][[1]] %in% levels(as.factor(e[,j])))])] <- matrix(0,nrow = n, ncol = sum(!lev[[j]][[1]] %in% levels(as.factor(e[,j]))))
                                             data[,paste0("X",j,levels(as.factor(e[,j])))[paste0("X",j,levels(as.factor(e[,j]))) %in% colnames(data)]] <- stats::model.matrix(stats::as.formula("~e[,j]"),data=data.frame(e[,j]))[,-1]
                                        
                                        } else {
                                             
                                             if (length(which(!lev[[j]][[1]] %in% levels(as.factor(e[,j]))))>1) {
                                                  data[,paste0("X",j,lev[[j]][[1]][which(!lev[[j]][[1]] %in% levels(as.factor(e[,j])))[-1]])] <- matrix(0,nrow = n, ncol = sum(!lev[[j]][[1]] %in% levels(as.factor(e[,j]))))
                                             }
                                             
                                             reste <- stats::model.matrix(stats::as.formula("~e[,j]"),data=data.frame(e[,j]))[,-1]
                                             data[,paste0("X",j,levels(as.factor(e[,j])))[paste0("X",j,levels(as.factor(e[,j]))) %in% colnames(data)]][,-1] <- reste
                                             data[,paste0("X",j,levels(as.factor(e[,j])))[paste0("X",j,levels(as.factor(e[,j]))) %in% colnames(data)]][,1] <- as.numeric(rowSums(reste)==0)
                                        }
                                   }
                              } else {
                                   data[,paste0("X",j,lev[[j]][[1]][which(!lev[[j]][[1]] %in% levels(as.factor(e[,j])))])] <- matrix(0,nrow = n, ncol = sum(!lev[[j]][[1]] %in% levels(as.factor(e[,j]))))
                              }
                              
                                   
                                   
                              # We control in test and validation datasets (if applicable) that there is the same values for E than in training set (not more).
                              # Test

                              if (test|validation) {

                                   # E

                                   ind_diff_train_test <- which(e[ensemble[[2]],j]==setdiff(factor(e[ensemble[[2]],j]),factor(e[ensemble[[1]],j])))
                                   while (!length(ind_diff_train_test)==0) {
                                        t[ind_diff_train_test,e[ensemble[[2]],j][ind_diff_train_test]] <- 0
                                        t <- prop.table.robust(t,1)
                                        e[ensemble[[2]],j][ind_diff_train_test] <- apply(t[ind_diff_train_test,,drop=FALSE],1,function(p) sample(unlist(lev[[j]][[1]]),1,prob = p))
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
                                        t <- prop.table.robust(t,1)
                                        emap[ensemble[[2]],j][ind_diff_train_test] <- apply(t[ind_diff_train_test,,drop=FALSE],1,function(p) names(which.max(p)))
                                        ind_diff_train_test <- which(emap[ensemble[[2]],j]==setdiff(factor(emap[ensemble[[2]],j]),factor(emap[ensemble[[1]],j])))
                                   }
                              }


                              # Validation

                              if (test&validation) {

                                   # E

                                   ind_diff_train_test <- which(e[ensemble[[3]],j]==setdiff(factor(e[ensemble[[3]],j]),factor(e[ensemble[[1]],j])))
                                   while (!length(ind_diff_train_test)==0) {
                                        t[ind_diff_train_test,e[ensemble[[3]],j][ind_diff_train_test]] <- 0
                                        t <- prop.table.robust(t,1)
                                        e[ensemble[[3]],j][ind_diff_train_test] <- apply(t[ind_diff_train_test,,drop=FALSE],1,function(p) sample(unlist(lev[[j]][[1]]),1,prob = p))
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
                                        t <- prop.table.robust(t,1)
                                        emap[ensemble[[3]],j][ind_diff_train_test] <- apply(t[ind_diff_train_test,,drop=FALSE],1,function(p) names(which.max(p)))
                                        ind_diff_train_test <- which(e[ensemble[[3]],j]==setdiff(factor(e[ensemble[[3]],j]),factor(e[ensemble[[1]],j])))
                                   }
                              }

                         } else {
                              # e^j and emap^j for disappearing features
                              e[,j] <- emap[,j] <- factor(rep(1,n))

                         }
                    }
                    lev <- apply(e,2,function(col) list(levels(factor(col))))
               }

               # Output preparation and calculation
               best.disc = list(best_reglog,best_link,best_formula)
               
               if (validation) {
                    if (criterion=="gini") {
                         if (test) performance = normalizedGini(labels[ensemble[[3]]],predictlogisticRegression(stats::model.matrix(best.disc[[3]],data=data.frame(discretize_link(best.disc[[2]],predictors[ensemble[[3]],]))),best.disc[[1]]$coefficients)) else performance = normalizedGini(labels[ensemble[[2]]],predictlogisticRegression(stats::model.matrix(best.disc[[3]],data=data.frame(discretize_link(best.disc[[2]],predictors[ensemble[[2]],]))),best.disc[[1]]$coefficients))
                    } else {
                         if (test) performance = -2*sum(labels[ensemble[[2]]]*predictlogisticRegression(stats::model.matrix(best.disc[[3]],data=data.frame(discretize_link(best.disc[[2]],predictors[ensemble[[2]],]))),best.disc[[1]]$coefficients)+(1-labels[ensemble[[2]]])*(1-predictlogisticRegression(stats::model.matrix(best.disc[[3]],data=data.frame(discretize_link(best.disc[[2]],predictors[ensemble[[2]],]))),best.disc[[1]]$coefficients))) else performance = criterion_iter[[current_best]]
                    }
               } else {
                    if (criterion=="gini") {
                         if (test) performance = normalizedGini(labels[ensemble[[2]]],predictlogisticRegression(stats::model.matrix(best.disc[[3]],data=data.frame(discretize_link(best.disc[[2]],predictors[ensemble[[2]],]))),best.disc[[1]]$coefficients)) else performance = normalizedGini(labels[ensemble[[1]]],best.disc[[1]]$fitted.values)
                    } else {
                         if (test) performance = -2*rowSums(labels[ensemble[[2]]]*predictlogisticRegression(stats::model.matrix(best.disc[[3]],data=discretize_link(best.disc[[2]],predictors[ensemble[[2]],])),best.disc[[1]]$coefficients)+(1-labels[ensemble[[2]]])*(1-predictlogisticRegression(stats::model.matrix(best.disc[[3]],data=discretize_link(best.disc[[2]],predictors[ensemble[[2]],])),best.disc[[1]]$coefficients))) else performance = criterion_iter[[current_best]]
                    }
               }
               
               
               if ((test)&(validation)) {
                    return(methods::new(Class = "glmdisc", parameters = list(test,validation,criterion,iter,m_start,reg_type), best.disc = best.disc, performance = list(performance,criterion_iter), disc.data = data.frame(cbind(discretize_link(best.disc[[2]],predictors[ensemble[[3]],])),labels = labels[ensemble[[3]]]), cont.data = data.frame(cbind(predictors[ensemble[[3]],]),labels = labels[ensemble[[3]]])))
               } else if (validation) {
                    return(methods::new(Class = "glmdisc", parameters = list(test,validation,criterion,iter,m_start,reg_type), best.disc = best.disc, performance = list(performance,criterion_iter), disc.data = data.frame(cbind(discretize_link(best.disc[[2]],predictors[ensemble[[2]],])),labels = labels[ensemble[[2]]]), cont.data = data.frame(cbind(predictors[ensemble[[2]],]),labels = labels[ensemble[[2]]])))
               } else if (test) {
                    return(methods::new(Class = "glmdisc", parameters = list(test,validation,criterion,iter,m_start,reg_type), best.disc = best.disc, performance = list(performance,criterion_iter), disc.data = data.frame(cbind(discretize_link(best.disc[[2]],predictors[ensemble[[2]],])),labels = labels[ensemble[[2]]]), cont.data = data.frame(cbind(predictors[ensemble[[2]],]),labels = labels[ensemble[[2]]])))
               } else {
                    return(methods::new(Class = "glmdisc", parameters = list(test,validation,criterion,iter,m_start,reg_type), best.disc = best.disc, performance = list(performance,criterion_iter), disc.data = data.frame(cbind(discretize_link(best.disc[[2]],predictors[ensemble[[1]],])),labels = labels[ensemble[[1]]]), cont.data = data.frame(cbind(predictors[ensemble[[1]],]),labels = labels[ensemble[[1]]])))
               }
          
          } else {
               print("labels and predictors must be of same length")
          }
     }
     else {
          print("criterion must be gini, aic or bic")
     }
}
