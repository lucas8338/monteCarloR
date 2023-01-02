# results taken from: https://keisan.casio.com/exec/system/1180573449

Xs<- list(0.123,  0.111,  0.333,  0.48647 ,13,-1.7)
Ys<- list(0.13809,0.12473,0.36231,0.50852 ,1,-0.98379 )

for ( i in 1:(length(Xs)) ){
  testthat::expect_equal(math_erf(Xs[[i]]),Ys[[i]],tolerance=0.00001)
}