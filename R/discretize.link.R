# Discretize a prepared data.frame of predictors with a list of fitted links (one per
# feature, as stored in the glmdisc object). Returns an n x d character matrix with the
# name of the level (as shown to the user) of each observation for each feature.
discretize_link <- function(link, df) {
  n <- nrow(df)
  d <- ncol(df)
  out <- matrix("", n, d)
  for (j in seq_len(d)) {
    lk <- link[[j]]
    labels <- if (lk$type == "constant") {
      rep(lk$S, n)
    } else {
      link_map(lk, if (lk$type == "numeric") df[[j]] else as.integer(df[[j]]))
    }
    out[, j] <- unname(lk$relabel[as.character(labels)])
  }
  out
}

# Give every level of every feature a user-facing name, stored in `relabel` (label ->
# name) and `out_levels` (names, in display order). Levels of numeric features are
# ordered by the mean of the feature (missing values last); those of factor features by
# label.
add_level_names <- function(links, X, types, train) {
  for (j in seq_along(links)) {
    lk <- links[[j]]
    if (lk$type == "constant") {
      lk$relabel <- stats::setNames("1", as.character(lk$S))
      lk$out_levels <- "1"
    } else if (lk$type == "numeric") {
      labels <- link_map(lk, X[[j]])
      observed <- train & !is.na(X[[j]])
      averages <- tapply(X[[j]][observed], factor(labels[observed], levels = lk$S), mean)
      ordered_labels <- lk$S[order(averages[as.character(lk$S)], na.last = TRUE)]
      names_out <- as.character(seq_along(ordered_labels))
      names_out[ordered_labels == lk$na_label] <- "NA"
      lk$relabel <- stats::setNames(names_out, as.character(ordered_labels))
      lk$out_levels <- names_out
    } else {
      names_out <- as.character(seq_along(lk$S))
      lk$relabel <- stats::setNames(names_out, as.character(lk$S))
      lk$out_levels <- names_out
    }
    links[[j]] <- lk
  }
  links
}

# The quantized data as the data.frame the final logistic regression was fitted on: one
# factor per feature (levels in display order), named with `model_names`.
quantized_frame <- function(links, X, model_names) {
  quantized <- as.data.frame(discretize_link(links, X), stringsAsFactors = FALSE)
  for (j in seq_along(links)) {
    quantized[[j]] <- factor(quantized[[j]], levels = links[[j]]$out_levels)
  }
  colnames(quantized) <- model_names
  quantized
}
