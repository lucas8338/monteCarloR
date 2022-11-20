load("data/testMatrix.rda")

initState<-c(A=1,B=0,C=0)

for ( i in 1:5 ){
  initState<- scm_transitionProbability(testMatrix,initState)
}

testthat::expect_equal(initState,c(A=0.185120,B=0.132000,C=0.682880))