#' @title matrix_createMultivariateMultipleFromExogsCom
#' @description create a conditional occurrence matrix (com) from multiple exogs, this should
#' to accept one 'endog' factor vector and multiple 'exogs' factors vectors. the com returned
#' is the com of states of the exogs happening together causes endog.
#' @param endog a factor vector, is the endog (cause).
#' @param exogs a list with factors vectors, the (givens).
#' @param tPlusX a integer is the leading to be applied to the endog 1 means: ('actual exogs can bredics the next endog').
#' @return a data.frame with number of occurrences.
#' @import dplyr
#' @export
matrix_createMultivariateMultipleFromExogsCom <- function(endog,exogs,tPlusX=1L){
  ########################################################################################################################
  #| bellow will take each combination (or a number of combinations) and calculate the com of them, and append
  #| the distribution of occurrences of each combination to a data.frame. the names of each combination (rownames) of the
  #| data.frame must follow this standard: {state1} & {state2} & {state3}...
  ########################################################################################################################

  # times is a data.frame containing the exogs and a leaded version of the endog.
  times<- data.frame(exogs, 'endog' = dplyr::lead(endog, n=tPlusX))

  # the plyr::ddply will calculate the number of occurrence of each row
  occurrences<- plyr::ddply(times, colnames(times), nrow)

  # create a variable containing only the states
  ans<- occurrences[,1:(ncol(times)-1)]

  # for each state in endog will iterate and will create the column of the state and set the value of numer of
  # occurrence for that state.
  for ( i in 1:(length(levels(endog))) ){
    # contain the actual state (level)
    .level<- levels(endog)[[i]]
    # bellow will create the new column and will set the values, the 'which' function is used a lot.
    ans[ which(occurrences[['endog']]==.level), .level ]<- occurrences[['V1']][ which(occurrences[['endog']]==.level) ]
  }

  # this will take the repaeted first n columns in the ans (coluns which were in the exogs)
  # then will  summarize it (the remaining columns will be summed with the duplicated of these first columns).
  ans<- ans %>% dplyr::group_by_at(colnames(ans)[1:(length(exogs))]) %>% dplyr::summarise_at(colnames(ans)[(ncol(times)):(ncol(ans))],.funs=sum,na.rm=TRUE)

  # will generate the names for the rownames, will select the columns which arent the results, and will use paste.
  ansNames<- do.call(paste, c(ans[, 1:(length(exogs))], sep=' & '))

  # bellow ans will contain only the number of occurrences, cause the states will be the rownames
  ans<- ans[, (ncol(times)):(ncol(ans))]

  # because the dplyr functions the dplyr functions return a
  ans<- as.data.frame(ans)

  # set the new rownames
  rownames(ans)<- ansNames

  ans
  }
