load("data/testMatrix2.rda")

x<- testMatrix2
# values for 'y' taken fom the from:
# Markov Chains
# From Theory to Implementation and Experimentation, chapter 4, pag.48.
y<- c('S'=0.439194058168945,'R'=0.560805941831055)

result<- scm_nStepTransitionProbability(x,state = 'S',7)

testthat::expect_equal(result,y)
