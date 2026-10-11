# Preparation of the predictors: classification of the columns into numeric / factor
# features and consistent handling of factor levels between training and new data.

# Label used for missing values of factor features.
NA_LEVEL <- "<NA>"

# Turn a column into a factor with the given levels; missing values become NA_LEVEL.
as_feature_factor <- function(column, levels = NULL) {
  value <- as.character(column)
  value[is.na(value)] <- NA_LEVEL
  if (is.null(levels)) {
    base <- if (is.factor(column)) levels(droplevels(column)) else sort(unique(as.character(column[!is.na(column)])))
    levels <- if (any(value == NA_LEVEL)) c(base, NA_LEVEL) else base
  }
  factor(value, levels = levels)
}

# Predictors given to glmdisc(): a data.frame whose columns are numeric or factor.
prepare_features <- function(predictors) {
  X <- as.data.frame(predictors, stringsAsFactors = FALSE)
  types <- vapply(X, function(column) {
    if (is.numeric(column)) {
      "numeric"
    } else if (is.factor(column) || is.character(column) || is.logical(column)) {
      "factor"
    } else {
      NA_character_
    }
  }, character(1L), USE.NAMES = FALSE)
  if (anyNA(types)) {
    stop(simpleError("Unsupported data types. Columns of predictors must be numeric or factor."))
  }
  for (j in which(types == "factor")) {
    X[[j]] <- as_feature_factor(X[[j]])
  }
  list(X = X, types = types)
}

# New data given to predict() / discretize(): same columns and types as in training;
# factor values must have been seen during training.
prepare_new_features <- function(object, predictors) {
  types <- object@parameters$types_data
  X <- as.data.frame(predictors, stringsAsFactors = FALSE)
  if (ncol(X) != length(types)) {
    stop(simpleError("Not the same number of columns."))
  }
  new_types <- vapply(X, function(column) {
    if (is.numeric(column)) "numeric" else if (is.factor(column) || is.character(column) || is.logical(column)) "factor" else NA_character_
  }, character(1L), USE.NAMES = FALSE)
  if (!all(new_types == types, na.rm = FALSE) || anyNA(new_types)) {
    stop(simpleError("Not the same data types."))
  }
  for (j in which(types == "factor")) {
    training_levels <- object@parameters$factor_levels[[j]]
    value <- as.character(X[[j]])
    value[is.na(value)] <- NA_LEVEL
    if (!all(value %in% training_levels)) {
      stop(simpleError("Unseen (during training) levels of some categorical feature."))
    }
    X[[j]] <- factor(value, levels = training_levels)
  }
  X
}
