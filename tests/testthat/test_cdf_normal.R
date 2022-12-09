# data and results taken from: https://keisan.casio.com/exec/system/1180573188

Xs<- list(c(50,60,10),c(55,60,10),c(77,70,10),c(77,70,12),c(0.3,1,1),c(-0.3,1,1))
Ys<- list(0.15865    ,0.30853    ,0.75803    ,0.72016    ,0.24196   ,0.0968)

for ( i in 1:(length(Xs)) ){
  params<- Xs[[i]]
  x<- params[[1]]
  mean<- params[[2]]
  stddev<- params[[3]]
  testthat::expect_equal( cdf_normal(x,mean,stddev), Ys[[i]] ,tolerance = 0.00001)
}