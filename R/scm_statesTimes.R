#' @title scm_statesTimes
#' @description calculate the average time (percent) the system will stay at state.
#' @param scm a data.frame with a right stochastic matrix
#' @return a numeric vector with names and values with the percentage of time spent on each state.
#' @references
#' Markov Chains, From Theory to Implementation and Experimentation, Paul A. Gagniuc, chapter: 7.3.
#' @export
scm_statesTimes<- function (scm){
  multipliedMatrix<- matrix_multiplication(scm,scm)
  result<- c()
  for ( .column in colnames(multipliedMatrix) ){
    result[[.column]]<- sum(multipliedMatrix[,.column])*(1/3)
  }
  result
  class(result)<- 'numeric'
  result
}