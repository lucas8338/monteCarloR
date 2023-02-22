load("data/testMatrixFactors.rda")

dataAll<- testMatrixFactors

states<- "exog1=1 & exog2=2 & exog3=3"

result<- slw_whichRowsHappens(dataAll, states)

correctAnswer<- c(6, 16, 17, 19, 23, 29, 47, 50, 52, 63, 77, 79, 85, 92, 99)

testthat::expect_equivalent(result, correctAnswer)

states<- "exog2=3 & exog1=3"

result<- slw_whichRowsHappens(dataAll, states)

correctAnswer<- c(14, 20, 37)

testthat::expect_equivalent(result, correctAnswer)

states<- "exog3=1"

correctAnswer<- c(8, 13, 18, 36, 42, 45, 48, 56, 57, 60, 80, 81, 82, 88, 89, 91)

result<- slw_whichRowsHappens(dataAll, states)

testthat::expect_equivalent(result, correctAnswer)