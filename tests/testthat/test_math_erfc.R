# data and results taken from: https://keisan.casio.com/exec/system/1180573449

Xs<- list(0.777,0.666,0.123,2,-2,0.99)
Ys<- list(0.27183,0.34626,0.86190,0.00467,1.99532,0.16149)

for ( i in 1:(length(Xs)) ){
  testthat::expect_equal(math_erfc(Xs[[i]]),Ys[[i]],tolerance=0.00001)
}

# aditional test for math_erf and math_erfc
# when erf(x) - erfc(x) shoud be equal to 1.
for ( i in 1:(length(Xs)) ){
  testthat::expect_equal(math_erf(Xs[[i]])+math_erfc(Xs[[i]]),1)
}