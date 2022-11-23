#' @title scm_steadyState
#' @description find the steadyState probabilities (stable distribution) for
#' a right stochastic matrix.
#' @param scm a data.frame with the stochastic matrix
#' @param n.max a integer the maximum number of steps for simulating.
#' @return a numeric vector with names and probabilities for each state.
#' @references
#' Markov Chains, From Theory to Implementation and Experimentation, Paul A. Gagniuc, chapter: 4.
#' @export
scm_steadyState<- function(scm,n.max=1000){
  # learned should not to verify the scm cause this can be used
  # for another types of scm

  simulations<- scm_predict(scm,state=scm[1,],horizon = n.max,steadyState = TRUE)

  stopifnot("the simulations need to end before n.max"=nrow(simulations)<n.max)

  result<- simulations[nrow(simulations),2:(ncol(simulations))]

  class(result)<- 'numeric'

  result
}