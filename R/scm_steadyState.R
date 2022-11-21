#' @title scm_steadyState
#' @description find the steadyState probabilities (stable distribution) for
#' a right stochastic matrix. Rows with eigenvector that dont end on exactly one will be
#' fixed using the formula:
#' rowLowerValue= 1-(A-L)
#' A: sum of all values in the row
#' L: the lowest value in the row.
#' @param scm a data.frame with the stochastic matrix
#' @param n.max a integer the maximum number of steps for simulating.
#' @return a numeric vector with names and probabilities for each state.
#' @references
#' Markov Chains, From Theory to Implementation and Experimentation, Paul A. Gagniuc, chapter: 4.
#' @export
scm_steadyState<- function(scm,n.max=1000){
  stopifnot("scm must be a square matrix"=nrow(scm)==ncol(scm))
  stopifnot("cant have NA in the matrix"=any(is.na(scm))==FALSE)
  stopifnot("the eigenvectors of the stochastic matrix cannot be different than one (if near one will be fixed)."=all(rowSums(scm)>=0.98))

  # get the eigenvectors (sum of rows)
  eigenvectors<- rowSums(scm)
  # fix if the eigenvector is lower than one
  if ( any(eigenvectors<1) ){
    # iter for each "name" in of the indexes are lower than one
    for ( .rowName in names(eigenvectors[which(eigenvectors<1)]) ){
      # get the name of the column with the lowerst value
      columnNameLowerValue<- names(which.min(scm[.rowName,]))
      # set the new value for the eigenvector value be equal 1, this will be done
      # subtracting 1 - sum of all values of the row - the lowest non zero value (this new value).
      # nv= 1-(A-L)
      # A: sum of all values in the row
      # L: the value of the lowest value in the row
      scm[.rowName,columnNameLowerValue]<- 1-(sum(scm[.rowName,])-scm[.rowName,columnNameLowerValue])
    }
  }


  simulations<- scm_predict(scm,state=scm[1,],horizon = n.max,steadyState = TRUE)

  stopifnot("the simulations need to end before n.max"=nrow(simulations)<n.max)

  result<- simulations[nrow(simulations),2:(ncol(simulations))]

  class(result)<- 'numeric'

  result
}