# the result of this tests came from the book:
# Markov Chains, From Theory to Implementation and Experimentation, Paul A. Gagniuc, chapter: 4.
load("data/testMatrix2.rda")
data<- list(
  x= testMatrix2,
  y= c(S=0.43859649122807,R=0.56140350877193)
)

x<- data[['x']]
y<- data[['y']]

testthat::expect_equal(scm_steadyReady(x),y)