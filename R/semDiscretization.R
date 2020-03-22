#' Model-based multivariate discretization for logistic regression.
#'
#' This function discretizes a training set using an SEM-Gibbs based method (see References section).
#' It detects numerical features of the dataset and discretizes them ; values of categorical features (of type \code{factor}) are regrouped. This is done in a multivariate supervised way. Assessment of the correct model is done via AIC, BIC or test set error (see parameter \code{criterion}).
#' Second-order interactions can be searched through the optional \code{interaction} parameter using a Metropolis-Hastings algorithm (see References section).
#' @param predictors The matrix array containing the numerical or factor attributes to discretize.
#' @param labels The actual labels of the provided predictors (0/1).
#' @param validation Boolean : True if the algorithm should use predictors to construct a validation set on which to search for the best discretization scheme using the provided criterion (default: TRUE).
#' @param test Boolean : True if the algorithm should use predictors to construct a test set on which to calculate the provided criterion using the best discretization scheme (chosen thanks to the provided criterion on either the test set (if true) or the training set (otherwise)) (default: TRUE).
#' @param criterion The criterion ('gini','aic','bic') to use to choose the best discretization scheme among the generated ones (default: 'gini'). Nota Bene: it is best to use 'gini' only when test is set to TRUE and 'aic' or 'bic' when it is not. When using 'aic' or 'bic' with a test set, the likelihood is returned as there is no need to penalize for generalization purposes.
#' @param iter The number of iterations to do in the SEM protocole (default: 1000).
#' @param m_start The maximum number of resulting categories for each variable wanted (default: 20).
#' @param reg_type The model to use between discretized and continuous features (currently, only multinomial logistic regression ('poly') and ordered logistic regression ('polr') are supported ; default: 'poly'). WARNING: 'poly' requires the \code{mnlogit} package, 'polr' requires the \code{MASS} package.
#' @param interact Boolean : True (default) if interaction detection is wanted (Warning: may be very memory/time-consuming).
#' @param proportions The list of the proportions wanted for test and validation set. Not used when both test and validation are false. Only the first is used when there is only one of either test or validation that is set to TRUE. Produces an error when the sum is greater to one. Default: list(0.2,0.2) so that the training set has 0.6 of the input observations.
#' @concept SEM Gibbs discretization
#' @author Adrien Ehrhardt.
#' @seealso \code{\link{glm}}, \code{\link{multinom}}, \code{\link{polr}}
#' @details
#' This function finds the most appropriate discretization scheme for logistic regression. When provided with a continuous variable \eqn{X}, it tries to convert it to a categorical variable \eqn{Q} which values uniquely correspond to intervals of the continuous variable \eqn{X}.
#' When provided with a categorical variable \eqn{X}, it tries to find the best regroupement of its values and subsequently creates categorical variable \eqn{Q}. The goal is to perform supervised learning with logistic regression so that you have to specify a target variable \eqn{Y} denoted by \code{labels}.
#' The ‘‘discretization'' process, i.e. the transformation of \eqn{X} to \eqn{Q} is done so as to achieve the best logistic regression model \eqn{p(y|e;\theta)}. It can be interpreted as a special case feature engineering algorithm.
#' Subsequently, its outputs are: the optimal discretization scheme and the logistic regression model associated with it. We also provide the parameters that were provided to the function and the evolution of the criterion with respect to the algorithm's iterations.
#' @importFrom stats predict
#' @import caret
#' @importFrom graphics plot
#' @export
#' @references
#' Celeux, G., Chauveau, D., Diebolt, J. (1995), On Stochastic Versions of the EM Algorithm. [Research Report] RR-2514, INRIA. 1995. <inria-00074164>
#'
# #' Asad Hasan, Wang Zhiyu and Alireza S. Mahani (2015). mnlogit: Multinomial Logit Model. R package version 1.2.4. \url{https://CRAN.R-project.org/package=mnlogit}
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
#' print(sem_disc)
glmdisc <- function(predictors,labels,interact=TRUE,validation=TRUE,test=TRUE,criterion='gini',iter=1000,m_start=20,reg_type='poly',proportions=c(0.2,0.2)) {

     if (criterion %in% c('gini','aic','bic')) {
          if (length(labels)==length(predictors[,1])) {
               noms_colonnes = colnames(predictors)
               
               # Complete rows
               continu_complete_case = !is.na(predictors)
               
               # Calculating lengths n and d and data types
               n = length(labels)
               d = length(predictors[1,])
               types_data <- sapply(predictors[1,], class)

               if (sum(!(types_data %in% c("numeric","factor")))>0) {
                    stop(simpleError("Unsupported data types. Columns of predictors must be numeric or factor."))
               }

               # Initializing list of calculated criterion among which to select the best.
               criterion_iter=list()

               # Obtain training, test and validation datasets.
               ensemble <- cut_dataset(n,proportions,test=test,validation=validation)
               ensemble[[1]] = 1:n %in% ensemble[[1]]
               ensemble[[2]] = 1:n %in% ensemble[[2]]
               ensemble[[3]] = 1:n %in% ensemble[[3]]
               
               # Initializing variable Q (discretization of X) at random.
               e = emap = array(0,c(n,d))
               for (j in which(types_data=="numeric")) {
                    e[continu_complete_case[,j],j] = emap[continu_complete_case[,j],j] = as.factor(sample(1:m_start,sum(continu_complete_case[,j]),replace = TRUE))
                    e[!continu_complete_case[,j],j] = emap[!continu_complete_case[,j],j] = m_start+1
               }
               for (j in which(types_data=="factor")) {
                         # For categorical features, if m_start is above the number of original levels, then the quantized version is initialized at the original number of levels
                    if (m_start > nlevels(predictors[,j])) {
                         e[,j] = emap[,j] = as.factor(sample(1:nlevels(predictors[,j]),n,replace = TRUE))
                    } else {
                         # Otherwise proceed as for continuous features
                         e[,j] = emap[,j] = as.factor(sample(1:m_start,n,replace = TRUE))
                    }
               }

               # m encodes the number of levels per feature
               m = as.vector(apply(e,2,function(col) nlevels(factor(col))))
               names(m) <- paste0("X", 1:length(m))
               
               # lev encodes the list of levels of each feature
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
               
               # The interaction matrix delta is initialized at random with prob .5
               if (interact==TRUE) {
                    delta <- matrix(sample(0:1,d^2,replace=TRUE,prob=c(0.5,0.5)),nrow=d,ncol=d)
                    delta[lower.tri(delta)] = 0
                    diag(delta) <- 0
                    xPrincipal <- paste0("X", 1:d)

                    p_delta = matrix(0,nrow=d,ncol=d)
                    p_delta[lower.tri(p_delta)] = 0
                    diag(p_delta) <- 0
                    
                    # The proposal distribution of Metropolis-Hastings, p_delta, is calculated as follows
                    for (j in 1:(d-1)) {
                         for (k in (j+1):d) {
                              #print(j)
                              #print(k)
                              if(nlevels(factor(predictors[continu_complete_case[,j]&continu_complete_case[,k]&ensemble[[1]],j]))==1) stop(paste0("Some levels are scarce such that feature ",j," only has 1 level in train. Try with validation = F"))
                              if(nlevels(factor(predictors[continu_complete_case[,j]&continu_complete_case[,k]&ensemble[[1]],k]))==1) stop(paste0("Some levels are scarce such that feature ",k," only has 1 level in train. Try with validation = F"))
                              sans_inter <- stats::glm(labels ~ X1 + X2, family=stats::binomial(link="logit"), data=data.frame(labels = labels[continu_complete_case[,j]&continu_complete_case[,k]&ensemble[[1]]],X1 = predictors[continu_complete_case[,j]&continu_complete_case[,k]&ensemble[[1]],j],X2 = predictors[continu_complete_case[,j]&continu_complete_case[,k]&ensemble[[1]],k], stringsAsFactors = TRUE))
                              avec_inter <- stats::glm(labels ~ X1 + X2 + X1:X2, family=stats::binomial(link="logit"), data=data.frame(labels = labels[continu_complete_case[,j]&continu_complete_case[,k]&ensemble[[1]]],X1 = predictors[continu_complete_case[,j]&continu_complete_case[,k]&ensemble[[1]],j],X2 = predictors[continu_complete_case[,j]&continu_complete_case[,k]&ensemble[[1]],k], stringsAsFactors = TRUE))
                              p_delta[j,k] <- 1/(1+exp(-sans_inter$deviance - log(sum(ensemble[[1]]))*length(sans_inter$coefficients) + avec_inter$deviance + log(sum(ensemble[[1]]))*length(avec_inter$coefficients)))
                         }
                    }
               }

               # SEM algorithm
               for (i in 1:iter){

                    if (sum(apply(e,2,function(el) nlevels(as.factor(el))) == 1)==d) {message("Early stopping rule: all variables discretized in one value") ; break}

                    data_e = Filter(function(x)(length(unique(x))>1), data.frame(apply(e,2,factor), stringsAsFactors = TRUE))
                    data_emap = Filter(function(x)(length(unique(x))>1),data.frame(apply(emap,2,factor), stringsAsFactors = TRUE))
                    data = data.frame(e,labels = labels, stringsAsFactors = TRUE)
                    data_logit = data.frame(emap,labels = labels, stringsAsFactors = TRUE)

                    if (ncol(data_emap)==0) {message("Early stopping rule: all variables discretized in one value") ; break}
                    
                    # ejecter and ejecter_logit encode feature locations of features that have only one levels and are thus excluded from the model
                    if (interact==TRUE) {
                         tab_vrai <- which(delta==1,arr.ind=TRUE)
                         ejecter_logit <- sapply(1:(ncol(data_logit)-1), function(col) length(unique(data_logit[ensemble[[1]],col]))==1)
                         ejecter <- sapply(1:(ncol(data)-1), function(col) length(unique(data[ensemble[[1]],col]))==1)

                         # if there is at least one interaction xInter and xInter_logit encode these features as feature1:feature2
                         if (sum(tab_vrai)>0) {
                              xInter <- xInter_logit <- sapply(1:nrow(tab_vrai), function(row) paste0("X",tab_vrai[row,"row"],":X",tab_vrai[row,"col"]))

                              if (length(xPrincipal[ejecter])>0) {

                                   for (l in xPrincipal[ejecter]) {
                                        xInter <- xInter[!(xInter %in% (grep(l,xInter,value=TRUE)))]
                                   }

                              }

                              if (length(xPrincipal[ejecter_logit])>0) {

                                   for (l in xPrincipal[ejecter_logit]) {
                                        xInter_logit <- xInter_logit[!(xInter_logit %in% (grep(l,xInter_logit,value=TRUE)))]
                                   }
                              }

                              if (length(xInter)>0) {
                                   fmla = stats::as.formula(paste("~",paste(xPrincipal[!ejecter],collapse = "+"),"+",paste(xInter,collapse = "+")))
                              } else {
                                   fmla = stats::as.formula(paste("~",paste(xPrincipal[!ejecter],collapse = "+")))
                              }

                              if (length(xInter_logit)>0) {
                                   fmla_logit = stats::as.formula(paste("~",paste(xPrincipal[!ejecter_logit],collapse = "+"),"+",paste(xInter_logit,collapse = "+")))
                              } else {
                                   fmla_logit = stats::as.formula(paste("~",paste(xPrincipal[!ejecter_logit],collapse = "+")))
                              }

                              fmla_encoder = dummyVars(fmla, data_e, fullRank = FALSE, sep = "_")
                              fmla_logit_encoder = dummyVars(fmla_logit, data_emap, fullRank = FALSE, sep = "_")
                                   
                              data = predict(object = fmla_encoder, newdata = data_e)
                              data_logit = predict(object = fmla_logit_encoder, newdata = data_emap)

                              model_reglog = RcppNumerical::fastLR(data_logit[ensemble[[1]],],labels[ensemble[[1]]])

                              logit = RcppNumerical::fastLR(data[ensemble[[1]],],labels[ensemble[[1]]])

                         } else {
                              
                              # TODO : warmup starting values for logit coefficients
                              fmla = stats::as.formula(paste("~",paste(xPrincipal[!ejecter],collapse = "+")))
                              fmla_logit = stats::as.formula(paste("~",paste(xPrincipal[!ejecter_logit],collapse = "+")))
                              
                              fmla_encoder = dummyVars(fmla, data_e, fullRank = FALSE, sep = "_")
                              fmla_logit_encoder = dummyVars(fmla_logit, data_emap, fullRank = FALSE, sep = "_")
                              
                              data = predict(object = fmla_encoder, newdata = data_e)
                              data_logit = predict(object = fmla_logit_encoder, newdata = data_emap)
                              
                              model_reglog = RcppNumerical::fastLR(data_logit[ensemble[[1]],],labels[ensemble[[1]]])

                              logit = RcppNumerical::fastLR(data[ensemble[[1]],],labels[ensemble[[1]]])
                              
                         }
                    } else {
                         fmla = stats::as.formula(paste("~",paste(colnames(data_e),collapse = "+")))
                         fmla_logit = stats::as.formula(paste("~",paste(colnames(data_emap),collapse = "+")))
                         
                         fmla_encoder = dummyVars(fmla, data_e, fullRank = FALSE, sep = "_")
                         fmla_logit_encoder = dummyVars(fmla_logit, data_emap, fullRank = FALSE, sep = "_")
                         
                         data = predict(object = fmla_encoder, newdata = data_e)

                         data_logit = predict(object = fmla_logit_encoder, newdata = data_emap)
                         
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
                    } else stop(simpleError("validation must be boolean!"))

                    criterion_iter[[1]] <- -Inf

                    if (criterion_iter[[i]] >= criterion_iter[[current_best]]) {
                         best_reglog = model_reglog
                         best_link = tryCatch(link,error=function(cond) list())
                         current_best = i
                         best_formula = fmla_logit
                    }


                    if (interact==TRUE) {
                         p_delta_transition = abs(delta-p_delta)
                         pq <- sample(1:d^2,prob=prop.table(t(as.matrix(as.vector(p_delta_transition))),1),size=1)
                         delta_new = delta
                         delta_new[pq] = 1-delta_new[pq]
                         if (sum(delta_new==1)>0) {
                              xInter_new <- sapply(1:nrow(which(delta_new==1,arr.ind=TRUE)), function(row) paste0("X",which(delta_new==1,arr.ind=TRUE)[row,"row"],":X",which(delta_new==1,arr.ind=TRUE)[row,"col"]))
                              if (exists("ejecter_logit")) {
                                   if (length(xPrincipal[ejecter_logit])>0) {

                                        for (l in xPrincipal[ejecter_logit]) {
                                             xInter_new <- xInter_new[!(xInter_new %in% (grep(l,xInter_new,value=TRUE)))]
                                        }

                                   }
                                   if (length(xInter_new)>0) {

                                        fmla_logit_new = stats::as.formula(paste("~",paste(xPrincipal[!ejecter_logit],collapse = "+"),"+",paste(xInter_new,collapse = "+")))
                                        
                                        fmla_logit_new_encoder = dummyVars(fmla_logit_new, data_emap, fullRank = FALSE, sep = "_")
                                        
                                        data_logit_new = predict(object = fmla_logit_new_encoder, newdata = data_emap)
                                        
                                   } else {
                                        fmla_logit_new = stats::as.formula(paste("~",paste(xPrincipal[!ejecter_logit],collapse = "+")))
                                        
                                        fmla_logit_new_encoder = dummyVars(fmla_logit_new, data_emap, fullRank = FALSE, sep = "_")
                                        
                                        data_logit_new = predict(object = fmla_logit_new_encoder, newdata = data_emap)
                                   }
                              } else {
                                   fmla_logit_new = stats::as.formula(paste("~",paste(xPrincipal,collapse = "+"),"+",paste(xInter_new,collapse = "+")))
                                   
                                   fmla_logit_new_encoder = dummyVars(fmla_logit_new, data_emap, fullRank = FALSE, sep = "_")

                                   data_logit_new = predict(object = fmla_logit_new_encoder, newdata = data_emap)
                              }
                         } else {
                              if (exists("ejecter_logit")) {
                                   fmla_logit_new = stats::as.formula(paste("~",paste(xPrincipal[!ejecter_logit],collapse = "+")))
                                   
                                   fmla_logit_new_encoder = dummyVars(fmla_logit_new, data_emap, fullRank = FALSE, sep = "_")
                                   
                                   data_logit_new = predict(object = fmla_logit_new_encoder, newdata = data_emap)
                              } else {
                                   fmla_logit_new = stats::as.formula(paste("~",paste(xPrincipal,collapse = "+")))
                                   
                                   fmla_logit_new_encoder = dummyVars(fmla_logit_new, data_emap, fullRank = FALSE, sep = "_")
                                   
                                   data_logit_new = predict(object = fmla_logit_new_encoder, newdata = data_emap)
                              }

                         }

                         new_logit <- RcppNumerical::fastLR(data_logit_new[ensemble[[1]],],labels[ensemble[[1]]])
                         alpha = exp(2*new_logit$loglikelihood-log(sum(ensemble[[1]]))*length(new_logit$coefficients) - (2*model_reglog$loglikelihood-log(sum(ensemble[[1]]))*length(model_reglog$coefficients)))*(p_delta_transition[pq])/(1-p_delta_transition[pq])

                         if (pq %% d==0) {
                              var_interact = c(noms_colonnes[pq %/% d],noms_colonnes[d])
                         } else {
                              var_interact = c(noms_colonnes[pq %/% d+1],noms_colonnes[pq %% d])
                         }
                         
                         message("Current interaction being tested is between variables ",var_interact[1]," and ",var_interact[2]," with a probability of acceptance of ",alpha)
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
                         if (length(unique(e[continu_complete_case[,j]&ensemble[[1]],j]))>1) {

                              if (sum(lapply(lapply(1:d,function(j) !lev_1[[j]][[1]] %in% lev[[j]][[1]]),sum)>0)>0) {
                                   e[,which(lapply(lapply(1:d,function(j) !lev_1[[j]][[1]] %in% lev[[j]][[1]]),sum)>0)] = sapply(which(lapply(lapply(1:d,function(j) !lev_1[[j]][[1]] %in% lev[[j]][[1]]),sum)>0), function(col) factor(e[,col],levels = lev_1[[col]][[1]]))
                              }

                              # Polytomic or ordered logistic regression
                              if ((reg_type=='poly')&(types_data[j]=="numeric")) {
                                   link[[j]] = nnet::multinom(e ~ x, data=data.frame(e=e[continu_complete_case[,j]&ensemble[[1]],j],x=predictors[continu_complete_case[,j]&ensemble[[1]],j], stringsAsFactors = TRUE), start = link[[j]]$coefficients, trace = FALSE, Hess=FALSE, maxit=50)
                              } else if (types_data[j]=="numeric") {
                                   if (exists("link[[j]]$weights")) {
                                        link[[j]] = MASS::polr(e ~ x, data=data.frame(e = factor(as.numeric(ordered(e[continu_complete_case[,j]&ensemble[[1]],j],levels = names(sort(unlist(by(predictors[continu_complete_case[,j]&ensemble[[1]],j],e[continu_complete_case[,j]&ensemble[[1]],j],mean)))))), ordered=T), x = predictors[continu_complete_case[,j]&ensemble[[1]],j], stringsAsFactors=TRUE), Hess = FALSE, model = FALSE, weights = link[[j]]$weights)
                                   } else if (nlevels(as.factor(e[continu_complete_case[ensemble[[1]],j],][ensemble[[1]],j]))>2) {
                                        link[[j]] = MASS::polr(e ~ x, data=data.frame(e = factor(as.numeric(ordered(e[continu_complete_case[,j]&ensemble[[1]],j],levels = names(sort(unlist(by(predictors[continu_complete_case[,j]&ensemble[[1]],j],e[continu_complete_case[,j]&ensemble[[1]],j],mean)))))), ordered=T), x = predictors[continu_complete_case[,j]&ensemble[[1]],j], stringsAsFactors=TRUE), Hess = FALSE, model = FALSE)
                                   } else {
                                        link[[j]] = stats::glm(e ~ x, data=data.frame(e = factor(e[continu_complete_case[,j]&ensemble[[1]],j]), x = predictors[continu_complete_case[,j]&ensemble[[1]],j], stringsAsFactors=TRUE), family = stats::binomial(link="logit"), model = FALSE)
                                   }
                              }
                         } else {
                              link[[j]] = NA
                         }

                         # p(y|e^j,e^-j) calculation
                         if ((m[j])>1) {
                              y_p = array(0,c(n,(m[j])))
                              levels_to_sample <- unlist(lev[[j]][[1]])
                              
                              for (k in 1:length(levels_to_sample)) {
                                   modalites_k = data
                                   
                                   if (j>1) {
                                        modalites_k[,((3-j+sum((m[1:(j-1)]))):(1-j+sum((m[1:j]))))] = matrix(0,nrow=n,ncol=m[j]-1)
                                   } else {
                                        modalites_k[,(2:((m[1])))] = matrix(0,nrow=n,ncol=(m[j])-1)
                                   }
                                   
                                   if (paste0("X",j,"_",as.numeric(levels_to_sample[k])) %in% colnames(data)) {
                                        modalites_k[,paste0("X",j,"_",as.numeric(levels_to_sample[k]))] = rep(1,n)
                                   }
                                   
                                   p = predictlogisticRegression(modalites_k,logit$coefficients)

                                   y_p[,k] <- (labels*p+(1-labels)*(1-p))
                              }

                              # p(e^j|reste) calculation
                              if ((types_data[j]=="numeric")) {
                                   
                                   
                                   t = predict(link[[j]], newdata = data.frame(x = predictors[continu_complete_case[,j],][,j], stringsAsFactors=TRUE),type="probs")
                                   
                                   if (is.vector(t)) {
                                        t = cbind(1-t,t)
                                        colnames(t) = c("1","2")
                                   }
                                   
                                   if (sum(!continu_complete_case[,j])>0) {
                                        t_bis = matrix(NA,nrow = nrow(predictors), ncol = ncol(t) +1)
                                        t_bis[continu_complete_case[,j],1:ncol(t)] = t
                                        t_bis[continu_complete_case[,j],ncol(t)+1] = 0
                                        t_bis[!continu_complete_case[,j],] = t(matrix(c(rep(0,ncol(t)),1),nrow = ncol(t)+1,ncol=sum(!continu_complete_case[,j])))
                                        colnames(t_bis) = c(colnames(t),m_start+1)
                                        t = t_bis
                                   }
                                   
                              } else {
                                   link[[j]] = table(e[ensemble[[1]],j],predictors[ensemble[[1]],j])
                                   t = prop.table.robust(t(sapply(predictors[,j],function(row) link[[j]][,row])),1)
                              }

                              # Updating emap^j
                              emap[,j] <- apply(t,1,function(p) names(which.max(p)))

                              t <- prop.table.robust(t*y_p,1)

                              # Updating e^j
                              # if ((types_data[j]=="numeric")) {
                              #      
                              #      e[continu_complete_case[,j],j] <- apply(t,1,function(p) sample(levels_to_sample,1,prob = p,replace = TRUE))
                              #      e[!continu_complete_case[,j],j] <- ncol(t)+1
                              #      
                              # } else {
                              #      e[,j] <- apply(t,1,function(p) sample(levels_to_sample,1,prob = p,replace = TRUE))
                              # }
                              
                              e[,j] <- apply(t,1,function(p) sample(levels_to_sample,1,prob = p,replace = TRUE))
                              

                              if (nlevels(as.factor(e[,j]))>1) {
                                   if (nlevels(as.factor(e[,j]))==m[j]) {
                                        if (j>1) {
                                             data[,((3-j+sum((m[1:(j-1)]))):(1-j+sum((m[1:j]))))] = stats::model.matrix(stats::as.formula("~e"),data=data.frame("e"=factor(e[,j]), stringsAsFactors=TRUE))[,-1]
                                        } else {
                                             data[,(2:(m[1]))] = stats::model.matrix(stats::as.formula("~e"),data=data.frame("e"=factor(e[,j]), stringsAsFactors = TRUE))[,-1]
                                        }
                                   } else {
                                        # if (which(!lev[[j]][[1]] %in% levels(as.factor(e[,j])))[1]>1) {
                                             
                                             data[,paste0("X",j,"_",lev[[j]][[1]][which(!lev[[j]][[1]] %in% levels(as.factor(e[,j])))])] <- matrix(0,nrow = n, ncol = sum(!lev[[j]][[1]] %in% levels(as.factor(e[,j]))))
                                             data[,paste0("X",j,"_",levels(as.factor(e[,j])))[paste0("X",j,levels(as.factor(e[,j]))) %in% colnames(data)]] <- stats::model.matrix(stats::as.formula("~e[,j]"),data=data.frame(e[,j], stringsAsFactors = TRUE))[,-1]
                                        
                                        # } else {
                                        #      
                                        #      print(2)
                                        #      print(j)
                                        #      print(m[j])
                                        #      print(levels(as.factor(e[,j])))
                                        #      print(lev[[j]][[1]])
                                        #      print((!lev[[j]][[1]] %in% levels(as.factor(e[,j]))))
                                        #      print(which(!lev[[j]][[1]] %in% levels(as.factor(e[,j]))))
                                        #      print(paste0("X",j,"_",lev[[j]][[1]][which(!lev[[j]][[1]] %in% levels(as.factor(e[,j])))[-1]]))
                                        #      print(colnames(data))
                                        #      print("here again again!")
                                        #      
                                        #      if (length(which(!lev[[j]][[1]] %in% levels(as.factor(e[,j]))))>1) {
                                        #           # print("there!")
                                        #           # print(lev[[j]][[1]])
                                        #           # print(levels(as.factor(e[,j])))
                                        #           # print(!lev[[j]][[1]] %in% levels(as.factor(e[,j])))
                                        #           # print(which(!lev[[j]][[1]] %in% levels(as.factor(e[,j]))))
                                        #           # print(which(!lev[[j]][[1]] %in% levels(as.factor(e[,j])))[-1])
                                        #           # print(lev[[j]][[1]][which(!lev[[j]][[1]] %in% levels(as.factor(e[,j])))[-1]])
                                        #           # print(matrix(0,nrow = n, ncol = sum(!lev[[j]][[1]] %in% levels(as.factor(e[,j])))))
                                        #           # print(paste0("X",j,"_",lev[[j]][[1]][which(!lev[[j]][[1]] %in% levels(as.factor(e[,j])))[-1]]))
                                        #           # print(data[,paste0("X",j,"_",lev[[j]][[1]][which(!lev[[j]][[1]] %in% levels(as.factor(e[,j])))[-1]])])
                                        #           
                                        #           data[,paste0("X",j,"_",lev[[j]][[1]][which(!lev[[j]][[1]] %in% levels(as.factor(e[,j])))[-1]])] <- matrix(0,nrow = n, ncol = sum(!lev[[j]][[1]] %in% levels(as.factor(e[,j]))))
                                        #      }
                                        # 
                                        #      reste <- stats::model.matrix(stats::as.formula("~e[,j]"),data=data.frame(e[,j]))[,-1]
                                        #      data[,paste0("X",j,"_",levels(as.factor(e[,j])))[paste0("X",j,levels(as.factor(e[,j]))) %in% colnames(data)]][,-1] <- reste
                                        #      if (nlevels(as.factor(e[,j]))==2) {
                                        #           data[,paste0("X",j,"_",levels(as.factor(e[,j])))[paste0("X",j,"_",levels(as.factor(e[,j]))) %in% colnames(data)]][,1] <- as.numeric(reste==0)
                                        #      } else {
                                        #           data[,paste0("X",j,"_",levels(as.factor(e[,j])))[paste0("X",j,"_",levels(as.factor(e[,j]))) %in% colnames(data)]][,1] <- as.numeric(rowSums(reste)==0)
                                        #      }
                                        # }
                                   }
                              } else {
                                   # print("here!")
                                   data[,paste0("X",j,"_",lev[[j]][[1]][which(!lev[[j]][[1]] %in% levels(as.factor(e[,j])))])] <- matrix(0,nrow = n, ncol = sum(!lev[[j]][[1]] %in% levels(as.factor(e[,j]))))
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
                                                  if ((types_data[j]=="numeric")) {
                                                       t = predict(link[[j]], newdata = data.frame(x = predictors[,j], stringsAsFactors=TRUE),type="probs")
                                                  } else {
                                                       t = prop.table.robust(t(sapply(predictors[,j],function(row) link[[j]][,row])),1)
                                                  }
                                             } else {
                                                  if ((types_data[j]=="numeric")) {
                                                       t = predict(link[[j]], newdata = data.frame(x = predictors[,j], stringsAsFactors=TRUE),type="probs")
                                                  } else {
                                                       t = prop.table.robust(t(sapply(predictors[,j],function(row) link[[j]][,row])),1)
                                                  }
                                             }
                                        } else {
                                             if ((types_data[j]=="numeric")) {
                                                  t = tryCatch(predict(link[[j]], newdata = data.frame(x = predictors[,j], stringsAsFactors=TRUE),type="probs"), error = function(cond) matrix(c(1-predict(link[[j]], newdata = data.frame(x = predictors[,j], stringsAsFactors = TRUE),type="response"),predict(link[[j]], newdata = data.frame(x = predictors[,j], stringsAsFactors=TRUE),type="response")),ncol=2,dimnames = list(seq(1:n),c(min(levels(factor(e[ensemble[[1]],j]))),max(levels(factor(e[ensemble[[1]],j])))))))
                                             } else {
                                                  t = prop.table.robust(t(sapply(predictors[,j],function(row) link[[j]][,row])),1)
                                             }
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
                                                  if ((types_data[j]=="numeric")) {
                                                       t = predict(link[[j]], newdata = data.frame(x = predictors[,j], stringsAsFactors = TRUE),type="probs")
                                                  } else {
                                                       t = prop.table.robust(t(sapply(predictors[,j],function(row) link[[j]][,row])),1)
                                                  }
                                             } else {
                                                  if ((types_data[j]=="numeric")) {
                                                       t = predict(link[[j]], newdata = data.frame(x = predictors[,j], stringsAsFactors = TRUE),type="probs")
                                                  } else {
                                                       t = prop.table.robust(t(sapply(predictors[,j],function(row) link[[j]][,row])),1)
                                                  }
                                             }
                                        } else {
                                             if ((types_data[j]=="numeric")) {
                                                  t = tryCatch(predict(link[[j]], newdata = data.frame(x = predictors[,j], stringsAsFactors = TRUE),type="probs"), error = function(cond) matrix(c(1-predict(link[[j]], newdata = data.frame(x = predictors[,j], stringsAsFactors = TRUE),type="response"),predict(link[[j]], newdata = data.frame(x = predictors[,j], stringsAsFactors = TRUE),type="response")),ncol=2,dimnames = list(seq(1:n),c(min(levels(factor(e[ensemble[[1]],j]))),max(levels(factor(e[ensemble[[1]],j])))))))
                                             } else {
                                                  t = prop.table.robust(t(sapply(predictors[,j],function(row) link[[j]][,row])),1)
                                             }
                                        }
                                        t[ind_diff_train_test,emap[ensemble[[2]],j][ind_diff_train_test]] <- 0
                                        t <- prop.table.robust(t,1)
                                        emap[ensemble[[2]],j][ind_diff_train_test] <- apply(t[ind_diff_train_test,,drop=FALSE],1,function(p) names(which.max(p)))
                                        ind_diff_train_test <- which(emap[ensemble[[2]],j]==setdiff(factor(emap[ensemble[[2]],j]),factor(emap[ensemble[[1]],j])))
                                   }
                              }

                         } else {
                              # e^j and emap^j for disappearing features
                              e[,j] <- emap[,j] <- factor(rep(1,n))

                         }
                    }
                    lev <- apply(e,2,function(col) list(levels(factor(col))))
                    
                    message("Iteration ",i," ended with a performance of ",criterion," = ", criterion_iter[[i]])
               }

               # Output preparation and calculation
               
               ## 1st returned object
               best.disc = list(bestLogisticRegression = best_reglog,bestLinkFunction = best_link,formulaOfBestLogisticRegression = best_formula)
               
               if (validation) {
                    # if (criterion=="gini") {
                         if (test) {
                              encoder = dummyVars(best.disc[[3]],data.frame(discretize_link(best.disc[[2]],predictors[ensemble[[1]],],m_start), stringsAsFactors = TRUE), fullRank = FALSE, sep = "_")
                              newdata = data.frame(discretize_link(best.disc[[2]],predictors[ensemble[[3]],],m_start), stringsAsFactors = TRUE)
                              labels_test = as.numeric(labels[ensemble[[3]]])
                              
                              for (var in encoder$facVars) {
                                   suppressed_ind <- newdata[,var] == levels(newdata[,var])[!levels(newdata[,var]) %in% unlist(unname(encoder$lvls[var]))]
                                   if (length(suppressed_ind)>0){
                                        newdata <- newdata[!suppressed_ind, ]
                                        labels_test <- labels_test[!suppressed_ind]
                                        print(paste("levels", paste(levels(newdata[,var])[(!levels(newdata) %in% unlist(unname(encoder$lvls[var])))], collapse = ", "), "of feature", var, "were removed from test set."))
                                   }
                              }
                              
                              data_test = predict(object = encoder, newdata = newdata)
                              
                              performance = normalizedGini(labels_test,predictlogisticRegression(data_test,best.disc[[1]]$coefficients))
                         } else {
                              encoder = dummyVars(best.disc[[3]],data.frame(discretize_link(best.disc[[2]],predictors[ensemble[[1]],],m_start), stringsAsFactors = TRUE), fullRank = FALSE, sep = "_")
                              newdata = data.frame(discretize_link(best.disc[[2]],predictors[ensemble[[2]],],m_start), stringsAsFactors = TRUE)
                              labels_validation = as.numeric(labels[ensemble[[2]]])
                              
                              for (var in encoder$facVars) {
                                   suppressed_ind <- newdata[,var] == levels(newdata[,var])[!levels(newdata[,var]) %in% unlist(unname(encoder$lvls[var]))]
                                   if (length(suppressed_ind)>0){
                                        newdata <- newdata[!suppressed_ind, ]
                                        labels_validation <- labels_validation[!suppressed_ind]
                                        print(paste("levels", paste(levels(newdata[,var])[(!levels(newdata) %in% unlist(unname(encoder$lvls[var])))], collapse = ", "), "of feature", var, "were removed from test set."))
                                   }
                              }
                              
                              data_validation = predict(object = encoder, newdata = newdata)
                              
                              performance = normalizedGini(labels_validation,predictlogisticRegression(data_validation,best.disc[[1]]$coefficients))
                         }
                    # } else {
                         # if (test) performance = -2*sum(labels[ensemble[[2]]]*predictlogisticRegression(stats::model.matrix(best.disc[[3]],data=data.frame(discretize_link(best.disc[[2]],predictors[ensemble[[2]],]))),best.disc[[1]]$coefficients)+(1-labels[ensemble[[2]]])*(1-predictlogisticRegression(stats::model.matrix(best.disc[[3]],data=data.frame(discretize_link(best.disc[[2]],predictors[ensemble[[2]],]))),best.disc[[1]]$coefficients))) else performance = criterion_iter[[current_best]]
                    # }
               } else {
                    # if (criterion=="gini") {
                         if (test) {
                              encoder = dummyVars(best.disc[[3]],data.frame(discretize_link(best.disc[[2]],predictors[ensemble[[1]],],m_start), stringsAsFactors = TRUE), fullRank = FALSE, sep = "_")
                              newdata = data.frame(discretize_link(best.disc[[2]],predictors[ensemble[[2]],],m_start), stringsAsFactors = TRUE)
                              labels_test = as.numeric(labels[ensemble[[2]]])
                              for (var in encoder$facVars) {
                                   suppressed_ind <- newdata[,var] == levels(newdata[,var])[!levels(newdata[,var]) %in% unlist(unname(encoder$lvls[var]))]
                                   if (length(suppressed_ind)>0){
                                        newdata <- newdata[!suppressed_ind, ]
                                        labels_test <- labels_test[!suppressed_ind]
                                        print(paste("levels", paste(levels(newdata[,var])[(!levels(newdata) %in% unlist(unname(encoder$lvls[var])))], collapse = ", "), "of feature", var, "were removed from test set."))
                                   }
                              }
                              data_test = predict(object = encoder, newdata = data.frame(discretize_link(best.disc[[2]],predictors[ensemble[[2]],],m_start), stringsAsFactors = TRUE))
                              
                              
                              performance = normalizedGini(labels_test,predictlogisticRegression(data_test,best.disc[[1]]$coefficients))
                         } else {
                              encoder = dummyVars(best.disc[[3]],data.frame(discretize_link(best.disc[[2]],predictors[ensemble[[1]],],m_start), stringsAsFactors = TRUE), fullRank = FALSE, sep = "_")
                              #newdata = data.frame(discretize_link(best.disc[[2]],predictors[ensemble[[1]],],m_start))
                              performance = normalizedGini(as.numeric(labels[ensemble[[1]]]),best.disc[[1]]$fitted.values)
                         }
                    # } else {
                         # if (test) performance = -2*rowSums(labels[ensemble[[2]]]*predictlogisticRegression(stats::model.matrix(best.disc[[3]],data=data.frame(discretize_link(best.disc[[2]],predictors[ensemble[[2]],]))),best.disc[[1]]$coefficients)+(1-labels[ensemble[[2]]])*(1-predictlogisticRegression(stats::model.matrix(best.disc[[3]],data=data.frame(discretize_link(best.disc[[2]],predictors[ensemble[[2]],]))),best.disc[[1]]$coefficients))) else performance = criterion_iter[[current_best]]
                    # }
               }
               
               ## Good column names
               
               if (!is.null(noms_colonnes)) {
                    for (j in (length(noms_colonnes):1)) {
                         best.disc$formulaOfBestLogisticRegression = stats::as.formula(sub(paste0("X",j),noms_colonnes[j],best.disc$formulaOfBestLogisticRegression))
                    }
               }
               
               if ((test)&(validation)) {
                     disc.data = data.frame(cbind(discretize_link(best.disc[[2]],predictors[ensemble[[3]],],m_start), stringsAsFactors = TRUE),labels = labels[ensemble[[3]]])
                     
                     if (!is.null(colnames(predictors))) {
                         colnames(disc.data) = c(colnames(predictors),"labels")
                     }
                     
                     return(methods::new(Class = "glmdisc", parameters = list(test = test,validation = validation,criterion = criterion,iter = iter,m_start = m_start,reg_type = reg_type, types_data = types_data, encoder = encoder), best.disc = best.disc, performance = list(performance = performance,criterionEvolution = criterion_iter), disc.data = disc.data, cont.data = data.frame(cbind(predictors[ensemble[[3]],]),labels = labels[ensemble[[3]]], stringsAsFactors = TRUE)))
               } else if (validation) {
                    disc.data = data.frame(cbind(discretize_link(best.disc[[2]],predictors[ensemble[[2]],],m_start), stringsAsFactors = TRUE),labels = labels[ensemble[[2]]])
                    if (!is.null(colnames(predictors))) {
                         colnames(disc.data) = c(colnames(predictors),"labels")
                    }
                    return(methods::new(Class = "glmdisc", parameters = list(test = test,validation = validation,criterion = criterion,iter = iter,m_start = m_start,reg_type = reg_type, types_data = types_data, encoder = encoder), best.disc = best.disc, performance = list(performance = performance,criterionEvolution = criterion_iter), disc.data = disc.data, cont.data = data.frame(cbind(predictors[ensemble[[2]],], stringsAsFactors=TRUE),labels = labels[ensemble[[2]]], stringsAsFactors = TRUE)))
               } else if (test) {
                    disc.data = data.frame(cbind(discretize_link(best.disc[[2]],predictors[ensemble[[2]],],m_start), stringsAsFactors = TRUE),labels = labels[ensemble[[2]]])
                    if (!is.null(colnames(predictors))) {
                         colnames(disc.data) = c(colnames(predictors),"labels")
                    }
                    return(methods::new(Class = "glmdisc", parameters = list(test = test,validation = validation,criterion = criterion,iter = iter,m_start = m_start,reg_type = reg_type, types_data = types_data, encoder = encoder), best.disc = best.disc, performance = list(performance = performance,criterionEvolution = criterion_iter), disc.data = disc.data, cont.data = data.frame(cbind(predictors[ensemble[[2]],], stringsAsFactors=TRUE),labels = labels[ensemble[[2]]], stringsAsFactors = TRUE)))
               } else {
                    disc.data = data.frame(cbind(discretize_link(best.disc[[2]],predictors[ensemble[[1]],],m_start)),labels = labels[ensemble[[1]]], stringsAsFactors=TRUE)
                    if (!is.null(colnames(predictors))) {
                         colnames(disc.data) = c(colnames(predictors),"labels")
                    }
                    return(methods::new(Class = "glmdisc", parameters = list(test = test,validation = validation,criterion = criterion,iter = iter,m_start = m_start,reg_type = reg_type, types_data = types_data, encoder = encoder), best.disc = best.disc, performance = list(performance = performance,criterionEvolution = criterion_iter), disc.data = disc.data, cont.data = data.frame(cbind(predictors[ensemble[[1]],]),labels = labels[ensemble[[1]]], stringsAsFactors = TRUE)))
               }
               
          } else {
               stop(simpleError("labels and predictors must be of same length"))
          }
     } else {
          stop(simpleError("criterion must be gini, aic or bic"))
     }
}
