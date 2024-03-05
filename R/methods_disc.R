methods::setMethod(
  "show",
  methods::signature(object = "glmdisc"),
  function(object) {
    methods::show(object@best.disc[[1]])
  }
)


#' Print for the glmdisc class.
#' @rdname print
#' @method print glmdisc
#' @export
#' @param x S4 glmdisc object.
#' @param ... Other objects passed to print
print.glmdisc <- function(x, ...) {
  print(x@best.disc[[1]])
}


#' Summary for the glmdisc class.
#' @rdname summary
#' @method summary glmdisc
#' @export
#' @param object S4 glmdisc object.
#' @param ... Other objects passed to summary
summary.glmdisc <- function(object, ...) {
  summary(object@best.disc[[1]])
}


simple_roc <- function(labels, scores) {
  labels <- labels[order(scores, decreasing = TRUE)]
  labels <- as.numeric(as.character(labels))
  data.frame(TPR = cumsum(labels) / sum(labels), FPR = cumsum(!labels) / sum(!labels), labels)
}


#' Wrapper for \code{\link[caret]{contr.ltfr}}
#'
#' Simply redirects the call to \code{\link[caret]{contr.ltfr}}, this is done
#' to avoid errors when the caret namespace is not attached.
#'
#' @param ... \code{\link[caret]{contr.ltfr}} parameters.
#' @export
#' @keywords internal
#' @examples
#' contr.ltfr(2)
contr.ltfr <- function(...) {
  caret::contr.ltfr(...)
}
