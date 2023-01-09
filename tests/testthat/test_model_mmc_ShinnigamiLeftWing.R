load("data/testMatrixFactors.rda")

result<- model_mmc_ShinnigamiLeftWing(testMatrixFactors[[1]], testMatrixFactors[,-1], 2:3, 1)

# the tests results bellow were taken using the functions 'matrix_createMultivariateFromExogCom' and
# 'matrix_createMultivariateMultipleFromExogsCom', some parts were manualy verified too.

########################################################################################################################
#| test level1
########################################################################################################################

testthat::expect_equivalent(result[[ 'level1' ]]['exog1=1',], c(4,7,14,10,6))
testthat::expect_equivalent(result[[ 'level1' ]]['exog1=3',], c(3,5,0,3,4))
testthat::expect_equivalent(result[[ 'level1' ]]['exog1=5',], c(1,4,0,0,4)) # manual

testthat::expect_equivalent(result[[ 'level1' ]]['exog2=2',], c(3,7,14,4,5))
testthat::expect_equivalent(result[[ 'level1' ]]['exog2=5',], c(4,8,0,4,3))

testthat::expect_equivalent(result[[ 'level1' ]]['exog3=5',], c(3,6,0,5,5))

#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
########################################################################################################################
#| test level2
########################################################################################################################

testthat::expect_equivalent(result[[ 'level2' ]]['exog1=2 & exog2=2',], c(2,1,0,1,3))
testthat::expect_equivalent(result[[ 'level2' ]]['exog1=5 & exog2=1',], c(1,0,0,0,1))
testthat::expect_equivalent(result[[ 'level2' ]]['exog1=1 & exog2=2',], c(0,2,14,2,1)) # manual

#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
########################################################################################################################
#| test level3
########################################################################################################################

testthat::expect_equivalent(result[[ 'level3' ]]['exog1=1 & exog2=2 & exog3=3',], c(0,0,14,0,1)) # manual
testthat::expect_equivalent(result[[ 'level3' ]]['exog1=3 & exog2=2 & exog3=1',], c(0,2,0,0,0)) # manual
testthat::expect_equivalent(result[[ 'level3' ]]['exog1=1 & exog2=5 & exog3=5',], c(0,1,0,1,0)) # manual
testthat::expect_equivalent(result[[ 'level3' ]]['exog1=2 & exog2=2 & exog3=2',], c(1,0,0,1,2))
testthat::expect_equivalent(result[[ 'level3' ]]['exog1=1 & exog2=5 & exog3=4',], c(0,0,0,1,2))
