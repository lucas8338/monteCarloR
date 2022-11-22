# test data build myself

load("data/testVerifyScm.rda")

trues<- testVerifyScm$trueScm
falses<- testVerifyScm$falseScm

for ( i in 1:(length(trues)) ){
  x<- trues[[i]]
  y<- trues[[i]]
  result<- scm_verifyScm(x)
  testthat::expect_equal(result,y)
}

for ( i in 1:(length(falses)) ){
  x<- falses[[i]]
  testthat::expect_error(scm_verifyScm(x))
}