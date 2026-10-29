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
#' @examples
#' set.seed(1)
#' x <- matrix(runif(300), nrow = 100, ncol = 3)
#' y <- rbinom(100, 1, plogis(2 * (x[, 1] > 0.5) - 1))
#' sem_disc <- glmdisc(x, y,
#'   iter = 30, m_start = 4, test = FALSE,
#'   validation = FALSE, criterion = "aic", interact = FALSE
#' )
#' print(sem_disc)
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
#' @examples
#' set.seed(1)
#' x <- matrix(runif(300), nrow = 100, ncol = 3)
#' y <- rbinom(100, 1, plogis(2 * (x[, 1] > 0.5) - 1))
#' sem_disc <- glmdisc(x, y,
#'   iter = 30, m_start = 4, test = FALSE,
#'   validation = FALSE, criterion = "aic", interact = FALSE
#' )
#' summary(sem_disc)
summary.glmdisc <- function(object, ...) {
  summary(object@best.disc[[1]])
}


simple_roc <- function(labels, scores) {
  labels <- labels[order(scores, decreasing = TRUE)]
  labels <- as.numeric(as.character(labels))
  data.frame(TPR = cumsum(labels) / sum(labels), FPR = cumsum(!labels) / sum(!labels), labels)
}
