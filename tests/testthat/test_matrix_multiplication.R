# y came from https://matrix.reshish.com/multCalculation.php
load("data/testMatrix.rda")
data<- list(
  x = testMatrix,
  y= data.frame('A'=c(0.22,0.26,0.16),'B'=c(0.14,0.18,0.12),'C'=c(0.64,0.56,0.72))
)

x<- data[['x']]
y<- data[['y']]
rownames(y)<- rownames(x)
colnames(y)<- colnames(x)
class(y)<- 'data.frame'
testthat::expect_equal(matrix_multiplication(x,x),y)



