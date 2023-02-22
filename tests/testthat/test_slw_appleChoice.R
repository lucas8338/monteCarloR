load("data/ethusdWithExogsFactorsAndSlwStates.rda")

dataAll<- dataAll
current<- dataAll[nrow(dataAll),]
tPlusX<- 32
minNumberOccurrences<- 100

happened<- slw_happened(current, rownames(levels[['level2']]), levels = 2)

valids<- levels[['level2']][which(rowSums(levels[['level2']])>=minNumberOccurrences), ]

valids.prob<- matrix_transitionProbabilities(valids)
# the states is the top states from the 'fell' coolumn
resultFell<- slw_appleChoice(dataAll, current, "smacVolumeClose160=]-924.3725, -781.23625] & adx_adx24=]41.691752906545, 70.8152025000835]", 'label', tPlusX)

testthat::expect_equivalent(nrow(resultFell[['Fell']]) + nrow(resultFell[['Rose']]), 109)
testthat::expect_equivalent(nrow(resultFell[['Fell']]), 93)
testthat::expect_equivalent(nrow(resultFell[['Rose']]), 16)
testthat::expect_equivalent(resultFell[['duplicates']][['Fell']], 13)
testthat::expect_equivalent(resultFell[['duplicates']][['Rose']], 2)

# this test is a bit incomplete, is very, very, hard to test the function cause this function
# is a bit complex, there a lot of logics to this function works, the function
# was rewriten to be more readable, to avoid bugs. i believe today 22 feb 2023 there not
# bugs in this function, and all their result are correctly
