load("data/testMatrixFactors.rda")

result<- model_mmc_ShinnigamiLeftWing(testMatrixFactors[[1]], testMatrixFactors[,-1], 1:3, 1)

testthat::expect_equivalent(names(result), c('level1','level2','level3'))

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

#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
########################################################################################################################
#| test if wanted only level 1
########################################################################################################################

result2<- model_mmc_ShinnigamiLeftWing(testMatrixFactors[[1]], testMatrixFactors[,-1], 1, 1)

testthat::expect_equivalent(names(result2), c('level1'))

testthat::expect_equivalent(result2[[ 'level1' ]]['exog1=1',], c(4,7,14,10,6))
testthat::expect_equivalent(result2[[ 'level1' ]]['exog1=3',], c(3,5,0,3,4))
testthat::expect_equivalent(result2[[ 'level1' ]]['exog1=5',], c(1,4,0,0,4)) # manual

testthat::expect_equivalent(result2[[ 'level1' ]]['exog2=2',], c(3,7,14,4,5))
testthat::expect_equivalent(result2[[ 'level1' ]]['exog2=5',], c(4,8,0,4,3))

testthat::expect_equivalent(result2[[ 'level1' ]]['exog3=5',], c(3,6,0,5,5))

#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
########################################################################################################################
#| test if wanted only level 3
########################################################################################################################

result3<- model_mmc_ShinnigamiLeftWing(testMatrixFactors[[1]], testMatrixFactors[,-1], 3, 1)

testthat::expect_equivalent(names(result3), c('level3'))

testthat::expect_equivalent(result3[[ 'level3' ]]['exog1=1 & exog2=2 & exog3=3',], c(0,0,14,0,1)) # manual
testthat::expect_equivalent(result3[[ 'level3' ]]['exog1=3 & exog2=2 & exog3=1',], c(0,2,0,0,0)) # manual
testthat::expect_equivalent(result3[[ 'level3' ]]['exog1=1 & exog2=5 & exog3=5',], c(0,1,0,1,0)) # manual
testthat::expect_equivalent(result3[[ 'level3' ]]['exog1=2 & exog2=2 & exog3=2',], c(1,0,0,1,2))
testthat::expect_equivalent(result3[[ 'level3' ]]['exog1=1 & exog2=5 & exog3=4',], c(0,0,0,1,2))

#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
########################################################################################################################
#| test if wanted only expecific levels
########################################################################################################################

result4<- model_mmc_ShinnigamiLeftWing(testMatrixFactors[[1]], testMatrixFactors[,-1], c(1,3), 1)

testthat::expect_equivalent(names(result4), c('level1','level3'))

testthat::expect_equivalent(result4[[ 'level1' ]]['exog1=1',], c(4,7,14,10,6))
testthat::expect_equivalent(result4[[ 'level1' ]]['exog1=3',], c(3,5,0,3,4))
testthat::expect_equivalent(result4[[ 'level1' ]]['exog1=5',], c(1,4,0,0,4)) # manual

testthat::expect_equivalent(result4[[ 'level1' ]]['exog2=2',], c(3,7,14,4,5))
testthat::expect_equivalent(result4[[ 'level1' ]]['exog2=5',], c(4,8,0,4,3))

testthat::expect_equivalent(result4[[ 'level1' ]]['exog3=5',], c(3,6,0,5,5))

testthat::expect_equivalent(result4[[ 'level3' ]]['exog1=1 & exog2=2 & exog3=3',], c(0,0,14,0,1)) # manual
testthat::expect_equivalent(result4[[ 'level3' ]]['exog1=3 & exog2=2 & exog3=1',], c(0,2,0,0,0)) # manual
testthat::expect_equivalent(result4[[ 'level3' ]]['exog1=1 & exog2=5 & exog3=5',], c(0,1,0,1,0)) # manual
testthat::expect_equivalent(result4[[ 'level3' ]]['exog1=2 & exog2=2 & exog3=2',], c(1,0,0,1,2))
testthat::expect_equivalent(result4[[ 'level3' ]]['exog1=1 & exog2=5 & exog3=4',], c(0,0,0,1,2))

#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
########################################################################################################################
#| test if wanted expecific levels except 1
########################################################################################################################

result5<- model_mmc_ShinnigamiLeftWing(testMatrixFactors[[1]], testMatrixFactors[,-1], c(2,3), 1)

testthat::expect_equivalent(names(result5), c('level2','level3'))

testthat::expect_equivalent(result5[[ 'level2' ]]['exog1=2 & exog2=2',], c(2,1,0,1,3))
testthat::expect_equivalent(result5[[ 'level2' ]]['exog1=5 & exog2=1',], c(1,0,0,0,1))
testthat::expect_equivalent(result5[[ 'level2' ]]['exog1=1 & exog2=2',], c(0,2,14,2,1)) # manual

testthat::expect_equivalent(result5[[ 'level3' ]]['exog1=1 & exog2=2 & exog3=3',], c(0,0,14,0,1)) # manual
testthat::expect_equivalent(result5[[ 'level3' ]]['exog1=3 & exog2=2 & exog3=1',], c(0,2,0,0,0)) # manual
testthat::expect_equivalent(result5[[ 'level3' ]]['exog1=1 & exog2=5 & exog3=5',], c(0,1,0,1,0)) # manual
testthat::expect_equivalent(result5[[ 'level3' ]]['exog1=2 & exog2=2 & exog3=2',], c(1,0,0,1,2))
testthat::expect_equivalent(result5[[ 'level3' ]]['exog1=1 & exog2=5 & exog3=4',], c(0,0,0,1,2))

#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
########################################################################################################################
#| test if want in non ordened order
########################################################################################################################
result6<- model_mmc_ShinnigamiLeftWing(testMatrixFactors[[1]], testMatrixFactors[,-1], c(3,1,2), 1)

testthat::expect_true((c('level1','level2','level3') %in% names(result6)) %>% all() == TRUE)

testthat::expect_equivalent(result6[[ 'level1' ]]['exog1=1',], c(4,7,14,10,6))
testthat::expect_equivalent(result6[[ 'level1' ]]['exog1=3',], c(3,5,0,3,4))
testthat::expect_equivalent(result6[[ 'level1' ]]['exog1=5',], c(1,4,0,0,4)) # manual

testthat::expect_equivalent(result6[[ 'level1' ]]['exog2=2',], c(3,7,14,4,5))
testthat::expect_equivalent(result6[[ 'level1' ]]['exog2=5',], c(4,8,0,4,3))

testthat::expect_equivalent(result6[[ 'level1' ]]['exog3=5',], c(3,6,0,5,5))

testthat::expect_equivalent(result6[[ 'level2' ]]['exog1=2 & exog2=2',], c(2,1,0,1,3))
testthat::expect_equivalent(result6[[ 'level2' ]]['exog1=5 & exog2=1',], c(1,0,0,0,1))
testthat::expect_equivalent(result6[[ 'level2' ]]['exog1=1 & exog2=2',], c(0,2,14,2,1)) # manual

testthat::expect_equivalent(result6[[ 'level3' ]]['exog1=1 & exog2=2 & exog3=3',], c(0,0,14,0,1)) # manual
testthat::expect_equivalent(result6[[ 'level3' ]]['exog1=3 & exog2=2 & exog3=1',], c(0,2,0,0,0)) # manual
testthat::expect_equivalent(result6[[ 'level3' ]]['exog1=1 & exog2=5 & exog3=5',], c(0,1,0,1,0)) # manual
testthat::expect_equivalent(result6[[ 'level3' ]]['exog1=2 & exog2=2 & exog3=2',], c(1,0,0,1,2))
testthat::expect_equivalent(result6[[ 'level3' ]]['exog1=1 & exog2=5 & exog3=4',], c(0,0,0,1,2))
