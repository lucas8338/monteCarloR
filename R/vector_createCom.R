#' @title vector_createCom
#' @description this function creates a matrix with the number of conditional occurrences. the main purpose
#' of the usage of this function is to create a stochastic matrix (markov chain matrix).
#' 'Com' means: Conditional Occurrence matrix.
#' @param vec a vector of factors.
#' @param tPlusX the difference of step of the next value. is globaly used t_n+1.
#' @return a data.frame with equal number of rows and columns and equal names in colnames and rownames with
#' number of occurences.
#' @references
#' Markov Chains, From Theory to Implementation and Experimentation, Paul A. Gagniuc, chapter: 3.
#' @export
vector_createCom<- function(vec,tPlusX=1L){
  stopifnot("'vec' cannot be non factor"=is.factor(vec))
  stopifnot("cannot have NA in 'vec'"=any(is.na(vec))==FALSE)

  result<- data.frame(matrix(nrow = length(levels(vec)),ncol = length(levels(vec))))
  rownames(result)<- levels(vec)
  colnames(result)<- levels(vec)

  times<- data.frame(original=vec,leaded=dplyr::lead(vec,n=tPlusX))

  for ( .rowName in rownames(result) ){
    for ( .colName in colnames(result) ){
      result[.rowName,.colName]<- length(which(times[,'original']==.rowName & times[,'leaded']==.colName))
    }
  }

  result

}