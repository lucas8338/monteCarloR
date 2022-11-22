# the results came from the book:
# Markov Chains, From Theory to Implementation and Experimentation, Paul A. Gagniuc, pag.: 104.
# but in the book the the author uses a floating point precision of 1, so all values
# in there are rounded, but here i'm using non rounded values, with a bigger
# floating point precision

load("data/testMatrix3.rda")
data<- list(
  x= testMatrix3,
  y= c(A=0.3963,B=0.5063,C=0.0913)
)

x<- data[['x']]
y<- data[['y']]

x<- scm_fixEigenvectorLowerThanOne(x)

testthat::expect_equal(scm_statesTimes(x),y,tolerance = 0.01)