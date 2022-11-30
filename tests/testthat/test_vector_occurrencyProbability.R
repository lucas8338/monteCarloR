# x1, x2 and results cames from: https://doi.org/10.1007/s42519-021-00179-y pag.: 15.
x1<- as.factor(c(3, 4, 2, 2, 1, 2, 3, 4, 2, 2, 3, 4))
y1<- c('1'=1/12,'2'=5/12,'3'=1/4,'4'=1/4)
x2<- as.factor(c(3, 1, 1, 1, 1, 3, 2, 2, 4, 1, 2, 2))
y2<- c('1'=5/12,'2'=1/3,'3'=1/6,'4'=1/12)

testthat::expect_equal(vector_occurrencyProbability(x1),y1)
testthat::expect_equal(vector_occurrencyProbability(x2),y2)

