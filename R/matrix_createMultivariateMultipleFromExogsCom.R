#' @title matrix_createMultivariateMultipleFromExogsCom
#' @description create a conditional occurrence matrix (com) from multiple exogs, this should
#' to accept one 'endog' factor vector and multiple 'exogs' factors vectors. the com returned
#' is the com of states of the exogs happening together causes endog.
#' @param endog a factor vector, is the endog (cause).
#' @param exogs a list with factors vectors, the (givens).
#' @param tPlusX a integer is the leading to be applied to the endog 1 means: ('actual exogs can bredics the next endog').
#' @param combinationsFunction a function which takes the exogs (a list with factor vectors) and return a data.frame
#' with the combinations, each column of each row is a level. expand.grid does it.
#' @param options.nThread the number of threads to be used by foreach.
#' @param options.threadType the type of thread of the foreach.
#' @return a data.frame with number of occurrences.
matrix_createMultivariateMultipleFromExogsCom <- function(endog,exogs,tPlusX=1L, combinationsFunction= function(data){
  combinations<- expand.grid(data)
  combinations<- combinations[ sample(1:(nrow(combinations))) ,]
  rownames(combinations)<- NULL
  combinations
},options.nThread= parallel::detectCores(), options.threadType=ifelse(Sys.info()['sysname']=='Windows','PSOCK','FORK')){
  # create processes for parallelization
  cl<- parallel::makeCluster(options.nThread,options.threadType)
  doSNOW::registerDoSNOW(cl)
  on.exit(parallel::stopCluster(cl))

  # bellow will create the variable 'exogs.levels' that contains the levels of each data
  # in exogs.
  exogs.levels<- list()
  for ( i in 1:(length(exogs)) ){
    exogs.levels[[ i ]]<- exogs[[ i ]] %>% levels()
  }

  # exogs.combinations must contain a data.frame with the combinations
  # each column of each row must contain a state of aa exog.
  # one example of this should work is the function 'expand.grid'.
  exogs.combinations<- combinationsFunction(exogs.levels)

  ########################################################################################################################
  #| bellow will take each combination (or a number of combinations) and calculate the com of them, and append
  #| the distribution of occurrences of each combination to a data.frame. the names of each combination (rownames) of the
  #| data.frame must follow this standard: {state1} & {state2} & {state3}...
  ########################################################################################################################

  # times is a data.frame containing the exogs and a leaded version of the endog.
  times<- data.frame(exogs, 'endog' = dplyr::lead(endog, n=tPlusX))
  
  result<- foreach::foreach( i=1:(nrow(exogs.combinations)), .packages = 'dplyr', .inorder = FALSE )%dopar%{
    ans<- data.frame(matrix(nrow = 0, ncol = 0))
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
    ans
  }

  # return the result
  dplyr::bind_rows(result)
  }
