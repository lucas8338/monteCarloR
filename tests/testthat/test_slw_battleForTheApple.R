load("data/testMatrixFactors.rda")

levels<- model_mmc_ShinnigamiLeftWing(testMatrixFactors$endog, testMatrixFactors[,-which(colnames(testMatrixFactors)=='endog')], levels = 2)
probs<- levels[[1]] %>% matrix_transitionProbabilities()
# in this case in problem i have exog2=2 & exog3=3: 0.82 to be 3
# and when exog1=4 & exog2=2 i have 1.0 (100%) to be 4,
# 'of course in the real life you will not find exactly similar to it (100%), but near, like 0.9 and 0.89.
# so the question is: 'which to trust? 0.82 to be 3 or 1.0 to be 4', this is what the slw_battleForTheAplle should to answer.
hip1<- probs[,3, drop=FALSE]
hip2<- probs[,4, drop=FALSE]
result<- slw_battleForTheApple(testMatrixFactors, hip1, hip2, 1, 'endog', Inf, maxDefensors = 20, maxAttackers = 20) %>% suppressWarnings()

testthat::expect_equivalent(result['exog1=1 & exog2=2 & exog1=1 & exog3=5','4'], 2) # manual
testthat::expect_true(colnames(result)[[3]] == 'defensor.index')
testthat::expect_true(colnames(result)[[4]] == 'attacker.index')

# this test is incomplete, a pratical test there only one, cause the function 'slw_battleForTheApple' needs a large
# amount of data (large data.frame) in the parameter: 'dataAll'.