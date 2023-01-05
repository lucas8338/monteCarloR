load("data/testMatrixFactors.rda")

data<- testMatrixFactors

result<- matrix_createMultivariateMultipleFromExogsCom(data[[1]],exogs = list(data[[2]],data[[3]],data[[4]]))

result<- result['1 & 2 & 3',]

testthat::expect_equivalent(result,c(0,0,14,0,1))