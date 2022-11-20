#' @title scm_steadyReady
#' @description find the steadyReady probabilities (stable distribution) for
#' a right stochastic matrix
#' @param scm a data.frame with the stochastic matrix
#' @param n.max a integer the maximum number of steps for simulating.
#' @return a numeric vector with names and probabilities for each state.
#' @references
#' Markov Chains, From Theory to Implementation and Experimentation, Paul A. Gagniuc, chapter: 4.
#' @export
scm_steadyReady<- function(scm,n.max=1000){
  stopifnot("scm must be a square matrix"=nrow(scm)==ncol(scm))
  stopifnot("cant have NA in the matrix"=any(is.na(scm))==FALSE)

  simulations<- scm_predict(scm,state=scm[1,],horizon = n.max,steadyReady = TRUE)

  stopifnot("the simulations need to end before n.max"=nrow(simulations)<n.max)

  result<- simulations[nrow(simulations),2:(ncol(simulations))]

  class(result)<- 'numeric'

  result
}