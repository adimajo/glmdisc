

elementwise.all.equal <- Vectorize(function(x,y) {isTRUE(all.equal(x,y))})


predict_fastLR <- function(model,x) {
     S = rowSums(sapply(1:ncol(x),function(col) x[,col]*model$coefficients[1:ncol(x)][col]))
     S_exp = 1/(1+exp(-S))
     S_exp <- ifelse(elementwise.all.equal(S_exp,0),S_exp+.Machine$double.eps,S_exp)
     ifelse(elementwise.all.equal(S_exp,1),S_exp-.Machine$double.eps,S_exp)
}
