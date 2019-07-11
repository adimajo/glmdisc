context("test-method_predict")

test_that("predict works with continuous data", {
     set.seed(1)
     x = matrix(runif(300), nrow = 100, ncol = 3)
     cuts = seq(0,1,length.out= 4)
     xd = apply(x,2, function(col) as.numeric(cut(col,cuts)))
     theta = t(matrix(c(0,0,0,2,2,2,-2,-2,-2),ncol=3,nrow=3))
     log_odd = rowSums(t(sapply(seq_along(xd[,1]), function(row_id) sapply(seq_along(xd[row_id,]), function(element) theta[xd[row_id,element],element]))))
     y = rbinom(100,1,1/(1+exp(-log_odd)))
     sem_disc <- glmdisc(x,y,iter=50,m_start=4,test=FALSE,validation=FALSE,criterion="aic")
     pred_sem = predict(sem_disc, data.frame(x))
     
     expect_length(pred_sem, 100)
     expect_true(all(pred_sem<=1), TRUE)
     expect_true(all(pred_sem>=0), TRUE)
})
