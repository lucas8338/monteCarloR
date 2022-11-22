#' @title vector_createCom
#' @description this function creates a matrix with the number of conditional occurrences. the main purpose
#' of the usage of this function is to create a stochastic matrix (markov chain matrix).
#' 'Com' means: Conditional Occurrence matrix.
#' @param vec a vector of factors.
#' @param tPlusx the difference of step of the next value. is globaly used t_n+1.
#' @return a data.frame with equal number of rows and columns and equal names in colnames and rownames with
#' number of occurences.
#' @references
#' Markov Chains, From Theory to Implementation and Experimentation, Paul A. Gagniuc, chapter: 3.
#' @export
vector_createCom<- function(vec,tPlusx=1L){
  stopifnot("'vec' cannot be non factor"=is.factor(vec))
  stopifnot("cannot have NA in 'vec'"=any(is.na(vec))==FALSE)

  n.levels<- length(levels(vec))
  result<- data.frame(matrix(nrow = n.levels,ncol = n.levels))

  rownames(result)<- levels(vec)

  colnames(result)<- levels(vec)

  fvec<- dplyr::lead(vec,n=tPlusx)

  dfvec<- data.frame(vec,fvec)

  dfvec<- tidyr::drop_na(dfvec)

  pg <- progress::progress_bar$new(total=n.levels,format=libGetDataR::util.progress_format())
  for ( .level in levels(vec) ){
    pg$tick()
    # this loop will add only the probability to be itsel "P[A|A] (probability from 'A' to 'A')"
    result[.level,.level]<- length(which(dfvec[,'vec']==.level & dfvec[,'fvec']==.level))
    for ( .outLevel in levels(vec)[which(levels(vec)!=.level)] ){
      # this loop will add only the columns " P[A|X_j] (probability from 'A' to 'X_j', j=1:length(factorsExcept'A')) "
      result[.level,.outLevel]<- length(which(dfvec[,'vec']==.level & dfvec[,'fvec']==.outLevel))
    }
  }

  stopifnot("the number of rows cannot be different of the number of columns"=nrow(vec)==ncol(vec))
  stopifnot("cannot have NA in the result"=any(is.na(result))==FALSE)

  # for possible future usage
  class(result)<- c("com","data.frame")

  result

}