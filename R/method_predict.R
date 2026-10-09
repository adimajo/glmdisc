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
#' predict(sem_disc, data.frame(x))
predict.glmdisc <- function(object, predictors) {
  if (ncol(predictors) != length(object@parameters$types_data)) {
    stop(simpleError("Not the same number of columns."))
  }

  types_data <- sapply(predictors[1, ], class)

  if (!all(types_data == object@parameters$types_data)) {
    stop(simpleError("Not the same data types."))
  }

  data_disc <- tryCatch(
    as.data.frame(discretize_link(object@best.disc[[2]], predictors, object@parameters$m_start), stringsAsFactors = TRUE),
    error = function(e) {
      stop(simpleError("Unseen (during training) levels of some categorical feature."))
    }
  )

  # Observations whose quantized value was not seen during training cannot be scored: they get NA.
  encoder <- object@parameters$encoder
  unseen <- rep(FALSE, nrow(data_disc))
  for (var in encoder$facVars) {
    training_levels <- unlist(unname(encoder$lvls[var]))
    value <- as.character(data_disc[[var]])
    new_level <- !(value %in% training_levels)
    if (any(new_level)) {
      warning(
        "Level(s) ", paste(unique(value[new_level]), collapse = ", "), " of feature ", var,
        " were not seen in the training set: the corresponding predictions are NA."
      )
      value[new_level] <- training_levels[1]
      unseen <- unseen | new_level
    }
    data_disc[[var]] <- factor(value, levels = training_levels)
  }

  data <- predict(object = encoder, newdata = data_disc)

  prediction <- rep(NA_real_, nrow(data_disc))
  prediction[!unseen] <- predictlogisticRegression(data[!unseen, , drop = FALSE], object@best.disc[[1]]$coefficients)
  prediction
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

# Levels not in the training set but in the test set are removed
# if (class(data_disc) == "character") {
#   for (var in 1:length(object@parameters$encoder$facVars)) {
#     if (length(levels(predictors[, names(which(types_data == "factor"))[var]])[!(levels(predictors[, names(which(types_data == "factor"))[var]]) %in% unlist(unname(object@parameters$encoder$lvls[var])))]) > 0) {
#       predictors <- predictors[-which(predictors[, names(which(types_data == "factor"))[var]] == levels(predictors[, names(which(types_data == "factor"))[var]])[!(levels(predictors[, names(which(types_data == "factor"))[var]]) %in% unlist(unname(object@parameters$encoder$lvls[var])))]), ]
#       warning("Level(s) ", paste(levels(predictors[, names(which(types_data == "factor"))[var]])[!(levels(predictors[, names(which(types_data == "factor"))[var]]) %in% unlist(unname(object@parameters$encoder$lvls[var])))], collapse = ", "), " of feature ", var, " were removed from test set.")
#       predictors <- predictors %>% dplyr::mutate_at(dplyr::vars(names(which(types_data == "factor"))), dplyr::funs(factor))
#     }
#   }
# }
