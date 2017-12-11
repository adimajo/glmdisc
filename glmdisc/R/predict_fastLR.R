


predict_fastLR <- function(model,x) {
     1/(1+exp(-(rowSums(sapply(1:ncol(x),function(col) x[,col]*model$coefficients[col])))))
}
