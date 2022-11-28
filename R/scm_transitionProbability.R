#' @title scm_transitionProbability
#' @description calculate the one step transition probability.
#' @param scm the stochastic matrix.
#' @param state a numeric vector containing names with the priori probabilities.
#' @return a numeric vector containing the probabilities
#' @references
#' Markov Chains, From Theory to Implementation and Experimentation, Paul A. Gagniuc, chapter: 4.
#' @export
scm_transitionProbability<- function(scm,state){
  # learned should not to verify the scm cause this can be used
  # for another types of scm

  # accepts a character 'state name' of scm as value for state,
  # will take the probabilities from 'scm'.
  if ( is.character(state) ){
    state<- scm[state,]
  }

  result<- c()

  for ( .name in names(state) ){
    result[[.name]]<- sum(state*scm[[.name]])
  }

  names(result)<- names(state)

  stopifnot("cant have NA in the result"=any(is.na(result))==FALSE)

  class(result)<- 'numeric'

  result

}
