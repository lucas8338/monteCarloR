# as the metropolis-hastings algorithm has a expected comportament, is in low sample the distribution
# of the returns be different than the stable distribution and for large runs the distribution of the returns
# be near equal the stable distribution i can use that for thest the function.
load(file = "data/testMatrix2.rda")
data<- list(
  x = testMatrix2,
  y = c(S=0.43859649122807,R=0.56140350877193)
)

x <- data$x
y <- data$y

returns<- mcmc_metropolisHastings(testMatrix2,'S',10000)
returns.probs<- c(S=length(which(returns=='S'))/length(returns),R=length(which(returns== "R"))/length(returns))

# as is a mcmc the the tolerance is 2 percent from the correct results
testthat::expect_equal(returns.probs,y,tolerance = 0.02)