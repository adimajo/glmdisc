#' Model-based multivariate discretization for logistic regression
#'
#' This function discretizes a training set using an SEM-Gibbs based method (see References section).
#' It detects numerical features of the dataset and discretizes them ; values of categorical features (of type \code{factor}) are regrouped. This is done in a multivariate supervised way. Assessment of the correct model is done via AIC, BIC or test set error (see parameter \code{criterion}).
#' Second-order interactions can be searched through the optional \code{interaction} parameter using a Metropolis-Hastings algorithm (see References section).
#' @param predictors The matrix array containing the numerical or factor attributes to discretize.
#' @param labels The actual labels of the provided predictors (0/1).
#' @param validation Boolean : True if the algorithm should use predictors to construct a validation set on which to search for the best discretization scheme using the provided criterion (default: TRUE). The coefficients of the final logistic regression are then estimated on the training and validation sets together.
#' @param test Boolean : True if the algorithm should use predictors to construct a test set on which to calculate the provided criterion using the best discretization scheme (chosen thanks to the provided criterion on either the test set (if true) or the training set (otherwise)) (default: TRUE).
#' @param criterion The criterion ('gini','aic','bic') to use to choose the best discretization scheme among the generated ones (default: 'gini'). Note: it is best to use 'gini' only when test is set to TRUE and 'aic' or 'bic' when it is not. When using 'aic' or 'bic' with a test set, the likelihood is returned as there is no need to penalize for generalization purposes.
#' @param iter The number of iterations to do in the SEM protocol (default: 1000).
#' @param m_start The maximum number of resulting categories for each variable wanted (default: 20).
#' @param reg_type The model linking a continuous feature to its quantized version: multinomial logistic regression ('poly', default; fitted by a dedicated penalized Newton solver) or ordered logistic regression ('polr', \code{\link[MASS]{polr}}; falls back to 'poly' when it cannot be fitted).
#' @param interact Boolean : True (default) if interaction detection is wanted (Warning: may be very memory/time-consuming).
#' @param proportions The proportions wanted for the test and validation sets, in this order. Not used when both test and validation are false. Only the first is used when there is only one of either test or validation that is set to TRUE. Produces an error when the sum is greater to one. Default: list(0.2,0.2) so that the training set has 0.6 of the input observations.
#' @param verbose Print information while fitting (default: FALSE).
#' @param patience Optional early stopping: stop when the criterion has not improved for this many consecutive iterations (default: NULL, run all \code{iter} iterations). The SEM chain typically reaches a plateau long before the default \code{iter}; see the vignette.
#' @param fast Deprecated and ignored (sampling is now always vectorized).
#' @concept SEM Gibbs discretization
#' @author Adrien Ehrhardt.
#' @seealso \code{\link[stats]{glm}}, \code{\link[MASS]{polr}}
#' @details
#' This function finds the most appropriate discretization scheme for logistic regression. When provided with a continuous variable \eqn{X}, it tries to convert it to a categorical variable \eqn{Q} which values uniquely correspond to intervals of the continuous variable \eqn{X}.
#' When provided with a categorical variable \eqn{X}, it tries to find the best grouping of its values and subsequently creates categorical variable \eqn{Q}. The goal is to perform supervised learning with logistic regression so that you have to specify a target variable \eqn{Y} denoted by \code{labels}.
#' The "discretization" process, i.e. the transformation of \eqn{X} to \eqn{Q} is done so as to achieve the best logistic regression model \eqn{p(y|e;\theta)}. It can be interpreted as a special case feature engineering algorithm.
#' When \code{interact = TRUE}, the set of pairwise interactions is explored by a Metropolis-Hastings sampler: at each iteration one pair is proposed for addition or removal (with a probability that favors pairs for which a logistic regression on the raw features finds an interaction) and accepted according to the BIC of the logistic regression with and without it.
#' The target of this sampler is the BIC approximation of the posterior of the set of interactions, with a Bernoulli prior given by the pairwise screening, and the proposal is corrected so that it is sampled exactly. The sampler of the thesis (BIC not halved, no proposal correction, unclamped prior) can be restored for comparison with \code{options(glmdisc.mh_scale = 1, glmdisc.mh_hastings = FALSE, glmdisc.mh_clamp = 0)}.
#' Subsequently, its outputs are: the optimal discretization scheme and the logistic regression model associated with it. We also provide the parameters that were provided to the function and the evolution of the criterion with respect to the algorithm's iterations.
#' @importFrom stats predict
#' @importFrom graphics plot
#' @export
#' @references
#' Celeux, G., Chauveau, D., Diebolt, J. (1995), On Stochastic Versions of the EM Algorithm. [Research Report] RR-2514, INRIA. 1995. <inria-00074164>
#'
#' Agresti, A. (2002) \emph{Categorical Data}. Second edition. Wiley.
#' @examples
#' # Simulation of a discretized logit model
#' set.seed(1)
#' x <- matrix(runif(300), nrow = 100, ncol = 3)
#' cuts <- seq(0, 1, length.out = 4)
#' xd <- apply(x, 2, function(col) as.numeric(cut(col, cuts)))
#' theta <- t(matrix(c(0, 0, 0, 2, 2, 2, -2, -2, -2), ncol = 3, nrow = 3))
#' log_odd <- rowSums(t(sapply(seq_along(xd[, 1]), function(row_id) {
#'   sapply(
#'     seq_along(xd[row_id, ]),
#'     function(element) theta[xd[row_id, element], element]
#'   )
#' })))
#' y <- rbinom(100, 1, 1 / (1 + exp(-log_odd)))
#'
#' sem_disc <- glmdisc(x, y,
#'   iter = 50, m_start = 4, test = FALSE,
#'   validation = FALSE, criterion = "aic"
#' )
#' print(sem_disc)
glmdisc <- function(predictors,
                    labels,
                    interact = TRUE,
                    validation = TRUE,
                    test = TRUE,
                    criterion = "gini",
                    iter = 1000,
                    m_start = 20,
                    reg_type = "poly",
                    proportions = c(0.2, 0.2),
                    verbose = FALSE,
                    fast = FALSE,
                    patience = NULL) {
  first_argument_checks(criterion, labels, predictors, validation, verbose, reg_type, iter, m_start, patience)
  if (isTRUE(fast)) warning("`fast` is deprecated and ignored: sampling is now always vectorized.", call. = FALSE)

  prepared <- prepare_features(predictors)
  X <- prepared$X
  types <- prepared$types
  n <- nrow(X)
  d <- ncol(X)
  second_argument_checks(d, interact)
  labels <- as.numeric(labels)

  # Training, validation (criterion used to choose the best iteration) and test
  # (performance of the chosen model) sets.
  split <- split_dataset(n, proportions, test, validation)

  fitted <- sem_engine(X, types, labels, split, criterion, as.integer(iter), as.integer(m_start), reg_type, interact, verbose, patience)
  best <- fitted$best
  links <- add_level_names(best$links, X, types, split$train)

  # Final logistic regression on the best quantization (a regular glm object)
  model_names <- make.names(colnames(X), unique = TRUE)
  quantized <- quantized_frame(links, X, model_names)
  varying <- which(lengths(lapply(links, `[[`, "out_levels")) > 1L)
  terms <- c(
    model_names[varying],
    paste(model_names[best$pairs[, 1L]], model_names[best$pairs[, 2L]], sep = ":")
  )
  quantized$labels <- labels
  formula_best <- stats::reformulate(terms, response = "labels")
  # The quantization was chosen with the training and validation sets; the coefficients of
  # the final model use both (the test set, if any, is left untouched).
  fit_rows <- split$train | split$validation
  best_glm <- suppressWarnings(stats::glm(formula_best, family = stats::binomial(link = "logit"), data = quantized[fit_rows, , drop = FALSE]))
  formula_best <- stats::reformulate(
    c(colnames(X)[varying], paste(colnames(X)[best$pairs[, 1L]], colnames(X)[best$pairs[, 2L]], sep = ":")),
    response = "labels"
  )

  # Performance on the test set, else on the validation set, else on the training set
  reported <- if (any(split$test)) split$test else if (any(split$validation)) split$validation else split$train
  performance <- normalizedGini(
    labels[reported],
    as.numeric(suppressWarnings(stats::predict(best_glm, newdata = quantized[reported, , drop = FALSE], type = "response")))
  )

  disc_data <- as.data.frame(discretize_link(links, X[reported, , drop = FALSE]), stringsAsFactors = TRUE)
  colnames(disc_data) <- colnames(X)
  disc_data$labels <- labels[reported]
  cont_data <- X[reported, , drop = FALSE]
  cont_data$labels <- labels[reported]

  methods::new(
    Class = "glmdisc",
    parameters = list(
      test = test, validation = validation, criterion = criterion, iter = iter, m_start = m_start,
      reg_type = reg_type, types_data = types, interact = interact,
      factor_levels = lapply(X, function(column) if (is.factor(column)) levels(column) else NULL),
      model_names = model_names
    ),
    best.disc = list(
      bestLogisticRegression = best_glm,
      bestLinkFunction = links,
      formulaOfBestLogisticRegression = formula_best
    ),
    performance = list(performance = performance, criterionEvolution = fitted$history),
    disc.data = disc_data,
    cont.data = cont_data
  )
}

# Masks of the training, validation and test observations.
split_dataset <- function(n, proportions, test, validation) {
  sets <- cut_dataset(n, proportions, test = test, validation = validation)
  mask <- function(index) if (is.null(index)) rep(FALSE, n) else seq_len(n) %in% index
  if (test && validation) {
    list(train = mask(sets[[1L]]), test = mask(sets[[2L]]), validation = mask(sets[[3L]]))
  } else if (test) {
    list(train = mask(sets[[1L]]), test = mask(sets[[2L]]), validation = mask(NULL))
  } else if (validation) {
    list(train = mask(sets[[1L]]), test = mask(NULL), validation = mask(sets[[2L]]))
  } else {
    list(train = mask(sets[[1L]]), test = mask(NULL), validation = mask(NULL))
  }
}
