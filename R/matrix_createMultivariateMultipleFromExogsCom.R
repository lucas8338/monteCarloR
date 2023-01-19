#' @title matrix_createMultivariateMultipleFromExogsCom
#' @description create a conditional occurrence matrix (com) from multiple exogs, this should
#' to accept one 'endog' factor vector and multiple 'exogs' factors vectors. the com returned
#' is the com of states of the exogs happening together causes endog.
#' @param endog a factor vector, is the endog (cause).
#' @param exogs a list with factors vectors, the (givens).
#' @param tPlusX a integer is the leading to be applied to the endog 1 means: ('actual exogs can bredics the next endog').
#' @param combinations.max the number of combinations to be used, if all combinations is wanted, set it to Inf.
#' @param combinations.randomize a logical if should to randomize the indexes of combinations, this way the combinations
#' will be random.
#' @param combinations.function a function which takes the exogs (a list with factor vectors) and return a data.frame
#' with the combinations, each column of each row is a level. expand.grid does it.
#' @param options.nThread the number of threads to be used by foreach.
#' @param options.threadType the type of thread of the foreach.
#' @return a data.frame with number of occurrences.
#' @import dplyr
#' @export
matrix_createMultivariateMultipleFromExogsCom <- function(endog,exogs,tPlusX=1L, combinations.max=Inf, combinations.randomize=TRUE, combinations.function=expand.grid){
  # bellow will create the variable 'exogs.levels' that contains the levels of each data
  # in exogs.
  exogs.levels<- vector('list', length(exogs)) # prealocate vector
  for ( i in 1:(length(exogs)) ){
    exogs.levels[[ i ]]<- exogs[[ i ]] %>% levels()
  }

  # exogs.combinations must contain a data.frame with the combinations
  # each column of each row must contain a state of aa exog.
  # one example of this should work is the function 'expand.grid'.
  exogs.combinations<- combinations.function(exogs.levels)

  rownames(exogs.combinations)<- NULL

  # configure the number of wanted indexes
  if ( combinations.max > nrow(exogs.combinations) ){
    combinations.max<- nrow(exogs.combinations)
  }

  # randomize the indexes
  if ( combinations.randomize==TRUE ){
    randomizedIndexes<- sample(1:(nrow(exogs.combinations)), replace = FALSE)
    exogs.combinations<- exogs.combinations[ randomizedIndexes ,]
  }

  # take only the number of wanted indexes
  exogs.combinations<- exogs.combinations[ 1:combinations.max ,]

  ########################################################################################################################
  #| bellow will take each combination (or a number of combinations) and calculate the com of them, and append
  #| the distribution of occurrences of each combination to a data.frame. the names of each combination (rownames) of the
  #| data.frame must follow this standard: {state1} & {state2} & {state3}...
  ########################################################################################################################

  # times is a data.frame containing the exogs and a leaded version of the endog.
  times<- data.frame(exogs, 'endog' = dplyr::lead(endog, n=tPlusX))

  result<- vector('list', nrow(exogs.combinations)) # prealocate a vector for results
  for( i in 1:(nrow(exogs.combinations)) ){
    ans<- data.frame( matrix(nrow = 0, ncol = 0) ) # a bit prealocation.
    combination<- exogs.combinations[i,]
    idx<- combination %>% unlist() %>% paste(., collapse = ' & ')
    for ( .colLevel in levels(endog) ){
      # will return a data.frame with TRUE or FALSE on each column.
      trueFalseDf<- times==c(combination, .colLevel) # bool
      # bellow will do the summation of the rows with true.
      trueFalseSummedVector<- trueFalseDf %>% rowSums()
      # to check if all values were true the sum of the values of the row need to be equal to number of columns.
      ans[idx, .colLevel]<- which(trueFalseSummedVector == ncol(times)) %>% length()
    }
    result[[i]]<- ans
  }

  # return the result
  dplyr::bind_rows(result)
  }
