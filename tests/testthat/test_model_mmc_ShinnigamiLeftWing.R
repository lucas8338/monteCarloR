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

#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
########################################################################################################################
#| bellow will test when using chunking
########################################################################################################################

# the place to save the files
pathDir<- paste0(Sys.getenv("TEMP"), '/test_model_mmc_ShinnigamiLeftWing')
# delete the dicrectory
unlink(pathDir, recursive = TRUE)
# create the directory
dir.create(pathDir, showWarnings = FALSE)
result<- model_mmc_ShinnigamiLeftWing(testMatrixFactors[[1]], testMatrixFactors[,-1], 1:3, 1, options.chunk = TRUE, options.chunkDir = pathDir, options.chunkNIter = 1, options.chunkContinue = FALSE)

# check the structure of files
# for level1
testthat::expect_true(file.exists(glue::glue("{pathDir}/level1/dist.part1.fst")))
# for level2
testthat::expect_true(file.exists(glue::glue("{pathDir}/level2/dist.part1.fst")))
testthat::expect_true(file.exists(glue::glue("{pathDir}/level2/dist.part2.fst")))
testthat::expect_true(file.exists(glue::glue("{pathDir}/level2/dist.part3.fst")))
# for level3
testthat::expect_true(file.exists(glue::glue("{pathDir}/level3/dist.part1.fst")))

# will test for each file individualy

# for the the first file of level1
fileDf<- fst::read_fst(glue::glue("{pathDir}/level1/dist.part1.fst"))
rownames(fileDf)<- fileDf$rownames
fileDf$rownames<- NULL

testthat::expect_equivalent(fileDf['exog1=1',], c(4,7,14,10,6))
testthat::expect_equivalent(fileDf['exog1=3',], c(3,5,0,3,4))
testthat::expect_equivalent(fileDf['exog1=5',], c(1,4,0,0,4)) # manual

testthat::expect_equivalent(fileDf['exog2=2',], c(3,7,14,4,5))
testthat::expect_equivalent(fileDf['exog2=5',], c(4,8,0,4,3))

testthat::expect_equivalent(fileDf['exog3=5',], c(3,6,0,5,5))

# test for level2

fileDf1<- fst::read_fst(glue::glue("{pathDir}/level2/dist.part1.fst"))
fileDf2<- fst::read_fst(glue::glue("{pathDir}/level2/dist.part2.fst"))
fileDf3<- fst::read_fst(glue::glue("{pathDir}/level2/dist.part3.fst"))
fileDf<- dplyr::bind_rows(fileDf1, fileDf2, fileDf3)
rownames(fileDf)<- fileDf$rownames
fileDf$rownames<- NULL

testthat::expect_equivalent(fileDf['exog1=2 & exog2=2',], c(2,1,0,1,3))
testthat::expect_equivalent(fileDf['exog1=5 & exog2=1',], c(1,0,0,0,1))
testthat::expect_equivalent(fileDf['exog1=1 & exog2=2',], c(0,2,14,2,1)) # manual

# test for level3
fileDf<- fst::read_fst(glue::glue("{pathDir}/level3/dist.part1.fst"))
rownames(fileDf)<- fileDf$rownames
fileDf$rownames<- NULL

testthat::expect_equivalent(fileDf['exog1=1 & exog2=2 & exog3=3',], c(0,0,14,0,1)) # manual
testthat::expect_equivalent(fileDf['exog1=3 & exog2=2 & exog3=1',], c(0,2,0,0,0)) # manual
testthat::expect_equivalent(fileDf['exog1=1 & exog2=5 & exog3=5',], c(0,1,0,1,0)) # manual
testthat::expect_equivalent(fileDf['exog1=2 & exog2=2 & exog3=2',], c(1,0,0,1,2))
testthat::expect_equivalent(fileDf['exog1=1 & exog2=5 & exog3=4',], c(0,0,0,1,2))
