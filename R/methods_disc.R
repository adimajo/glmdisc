methods::setMethod("show",
                   methods::signature(object = "glmdisc"),
                   function(object) {
                        methods::show(object@best.disc[[1]])
                   })


print.glmdisc <- function(object) {
     print(object@best.disc[[1]])
}


summary.glmdisc <- function(object) {
     summary(object@best.disc[[1]])
}


simple_roc <- function(labels, scores){
     labels <- labels[order(scores, decreasing=TRUE)]
     data.frame(TPR=cumsum(labels)/sum(labels), FPR=cumsum(!labels)/sum(!labels), labels)
}






