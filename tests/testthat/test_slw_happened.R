load("data/testMatrixFactors.rda")

coms<- model_mmc_ShinnigamiLeftWing(testMatrixFactors[,'endog'], testMatrixFactors[,which(colnames(testMatrixFactors)!='endog')], levels = 1:3)

result<- slw_happened(testMatrixFactors[nrow(testMatrixFactors),], rownames(coms[['level3']]))

testthat::expect_true(length(valids)==1)

testthat::expect_equivalent(rownames(coms[['level3']])[[result]], 'exog1=5 & exog2=4 & exog3=5')

# tests for level2

result<- slw_happened(testMatrixFactors[nrow(testMatrixFactors),], rownames(coms[['level2']]))

testthat::expect_true(length(result)==3)

# tests for level1

result<- slw_happened(testMatrixFactors[nrow(testMatrixFactors),], rownames(coms[['level1']]))

testthat::expect_true(length(result)==3)

testthat::expect_true('exog1=5' %in% rownames(coms[['level1']]))
testthat::expect_true('exog2=4' %in% rownames(coms[['level1']]))
testthat::expect_true('exog3=5' %in% rownames(coms[['level1']]))
