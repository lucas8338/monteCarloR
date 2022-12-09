# data takenf from: https://keisan.casio.com/exec/system/1180573188

Xs<- list(c(50,60,10),c(55,60,10),c(65,70,12),c(0.3,1,1),c(-0.3,1,1))
Ys<- list(0.02419,0.03520,0.03048,0.31225,0.17136)

for ( i in 1:(length(Xs)) ){
  params<- Xs[[i]]
  x<- params[[1]]
  mean<- params[[2]]
  stddev<- params[[3]]
  testthat::expect_equal( pdf_normal(x,mean,stddev), Ys[[i]], tolerance = 0.0001 )
}