#' @title scm_nStepTransitionProbability
#' @description calculate the probabilities n-step ahead given the probabilities
#' using a stochastic matrix.
#' @param scm the stochastic matrix with probabilities
#' @param state the initial probabilities
#' @param n the horizon
#' @references
#' #' Markov Chains, From Theory to Implementation and Experimentation, Paul A. Gagniuc, chapter: 4.
#' @export
scm_nStepTransitionProbability<- function(scm,state,n){
  stopifnot("'n' cant be lower than one"=n>=1)
  stopifnot("'n' cant be a non positive natural number."=n%%1==0)

  result<- scm_transitionProbability(scm,state)
  if ( n>1 ){
    # n-1 cause above one step is already done.
    for ( i in 1:(n-1) ){
      result<- scm_transitionProbability(scm,result)
    }
  }
  result
}