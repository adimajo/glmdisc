methods::setMethod(
  "show",
  methods::signature(object = "glmdisc"),
  function(object) {
    methods::show(object@best.disc[[1]])
  }
)


#' Print for the glmdisc class
#' @rdname print
#' @method print glmdisc
#' @export
#' @param x S4 glmdisc object.
#' @param ... Other objects passed to print
#' @return The logistic regression fitted on the discretized features (a \code{\link[stats]{glm}} object), invisibly.
print.glmdisc <- function(x, ...) {
  print(x@best.disc[[1]])
}


#' Summary for the glmdisc class
#' @rdname summary
#' @method summary glmdisc
#' @export
#' @param object S4 glmdisc object.
#' @param ... Other objects passed to summary
#' @return An object of class \code{summary.glm}, the summary of the logistic regression fitted on the discretized features.
summary.glmdisc <- function(object, ...) {
  summary(object@best.disc[[1]])
}


simple_roc <- function(labels, scores) {
  labels <- labels[order(scores, decreasing = TRUE)]
  labels <- as.numeric(as.character(labels))
  data.frame(TPR = cumsum(labels) / sum(labels), FPR = cumsum(!labels) / sum(!labels), labels)
}
