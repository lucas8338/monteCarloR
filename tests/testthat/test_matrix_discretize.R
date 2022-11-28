data<-list(
  x=data.frame(first=1:100,second=rep(1:10,10),third=rep(c('monkey','ant','cow','eagle'),25)),
  y=data.frame(first=c(rep('x<=25',25),rep('x<=50',25),rep('x<=75',25),rep('x<=100',25)),second=rep(c(rep("x<=5",5),rep("x<=10",5)),10),third=rep(c('monkey','ant','cow','eagle'),25))
)
data[['y']][['first']] <- as.factor(data[['y']][['first']])
data[['y']][['second']]<- as.factor(data[['y']][['second']])

x<- data[['x']]
y<- data[['y']]
testthat::expect_equal(matrix_discretize(x,4,n.custom = list("second"=2)),y)