x.1<- c(1,2,1,2,3,4,6,4,3,2,1,4,2)
y.1<- as.factor(c('[1, 2]','[1, 2]','[1, 2]','[1, 2]','[3, 3]','[4, 6]','[4, 6]','[4, 6]','[3, 3]','[1, 2]','[1, 2]','[4, 6]','[1, 2]'))

testthat::expect_equal(vector_discretize(x.1,mode='n.factors',options = list('n'=3)),y.1)

x.2<- x.1
y.2<- as.factor(c('[1, 2)','[2, 3)','[1, 2)','[2, 3)','[3, 4)','[4, 5)','6','[4, 5)','[3, 4)','[2, 3)','[1, 2)','[4, 5)','[2, 3)'))

testthat::expect_equivalent(vector_discretize(x.2,mode = 'fixed.minMaxStep',options = list('min'=1,'max'=6,'step'=1)),y.2)