load("data/testVec.rda")

# x and y taken from: Markov Chains, From Theory to Implementation and Experimentation, Paul A. Gagniuc, pags.: 73-75.
# but one value 'P[T|S]' was modified, from 4 to 5. so for row 'T' will there a difference from this.
x<- testVec$y.conditionalOccurrenceMatrix
y<- testVec$y.scm

result<- matrix_transitionProbabilities(x)

class(result)<- "data.frame"

testthat::expect_equal(result,y)