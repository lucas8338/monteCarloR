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

  uniques<- unique(times)

  # calculate the number of occurrence of each row or uniques in times.
  occurrencesCount<- util_countNOccurrencesOfEachRowString(uniques, times)

  occurrences<- cbind(uniques, 'count'=occurrencesCount)

  # create a variable containing only the states
  ans<- occurrences[,seq_len(ncol(times)-1)]

  # for each state in endog will iterate and will create the column of the state and set the value of numer of
  # occurrence for that state.
  # a variable to store the levels of endog.
  endogLevels<- levels(endog)
  for ( i in seq_along(endogLevels) ){
    # contain the actual state (level)
    .level<- endogLevels[[i]]
    # bellow will create the new column and will set the values, the 'which' function is used a lot.
    ans[ which(occurrences[['endog']]==.level), .level ]<- occurrences[['count']][ which(occurrences[['endog']]==.level) ]
  }

  # the group_by_at will group the data based on the values on a colname (search in web for group_by)
  # and the results returned have duplicated rows (based on the firsts columns not the columns with the number of occurrences)
  # and NAs.
  # for example you can have something like it:
  # ex1 | ex2 | ex3 | 1 | 2 | 3 | 4 | 5 // columns
  #  1  |  2  |  3  | NA| NA| 14| NA| NA
  #  1  |  2  |  3  | 1 | NA| 1 | NA| 1
  # in the example above look you have two columns with repeated exogs, this is how will come from
  # the group_by_at, then you need to remove the NA and sum these repeated columns (exogs repeated)
  # cause you want the example above become:
  # ex1 | ex2 | ex3 | 1 | 2 | 3 | 4 | 5 // columns
  #  1     2     3  | 1 | 0 | 15| 0 | 1
  # you summed the columns and has replaced NA by 0. this is what 'summarize_at' will do.:
  # the summarize_at will take the repaeted first n columns in the ans (coluns which were in the exogs)
  # then will  summarize it (the remaining columns will be summed with the duplicated of these first columns).
  ans<- ans %>% dplyr::group_by_at(colnames(ans)[seq_len(length(exogs))]) %>% dplyr::summarise_at(., colnames(ans)[(ncol(times)):(ncol(ans))],.funs=sum,na.rm=TRUE)

  # will generate the names for the rownames, will select the columns which arent the results, and will use paste.
  ansNames<- do.call(paste, c(ans[, seq_len(length(exogs))], sep=' & '))

  # bellow ans will contain only the number of occurrences, cause the states will be the rownames
  ans<- ans[, (ncol(times)):(ncol(ans))]

  # because the dplyr functions the dplyr functions return a
  ans<- as.data.frame(ans)

  # set the new rownames
  rownames(ans)<- ansNames

  ans
  }
