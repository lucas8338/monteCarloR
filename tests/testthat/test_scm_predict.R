load("data/testMatrix2.rda")

initialState<- c(S=1,R=0)

# values for 'y' taken fom the from:
# Markov Chains
# From Theory to Implementation and Experimentation, chapter 4, pag.48.
y<- data.frame(matrix(nrow = 5,ncol = 3))
y[1,]<-c('R',0.2, 0.8)
y[2,]<-c('S',0.54, 0.46)
y[3,]<-c('R',0.3955, 0.6045)
y[4,]<-c('R',0.4569125, 0.5430875)
y[5,]<-c('R',0.4308121875, 0.5691878125)

colnames(y)<- c('state','S','R')
y[,c('S','R')]<- c(as.numeric(y[['S']]),as.numeric(y[['R']]))

testthat::expect_equal(scm_predict(testMatrix2,state=initialState,horizon=5),y)
