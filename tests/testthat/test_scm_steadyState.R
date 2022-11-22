# the result of this tests came from the book:
# Markov Chains, From Theory to Implementation and Experimentation, Paul A. Gagniuc, chapter: 4.
load("data/testMatrix2.rda")
data<- list(
  x= testMatrix2,
  y= c(S=0.43859649122807,R=0.56140350877193)
)

x<- data[['x']]
y<- data[['y']]

testthat::expect_equal(scm_steadyState(x),y)

# for for when the eigenvalues are different than one
load("data/testMatrix3.rda")
x<- testMatrix3
y<- c(A=0.3773585,B=0.4981132,C=0.1245283)

testthat::expect_error(scm_verifyScm(x))

x<- scm_fixEigenvectorLowerThanOne(x)

testthat::expect_equal(scm_steadyState(x),y)