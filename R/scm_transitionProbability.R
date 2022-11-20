#' @title scm_transitionProbability
#' @description return the probability transition for the next state
#' @param scm the stochastic matrix.
#' @param state a numeric vector containing names with the priori probabilities.
#' @return a numeric vector containing the probabilities
#' @references
#' Markov Chains, From Theory to Implementation and Experimentation, Paul A. Gagniuc, chapter: 4.
#' @export
scm_transitionProbability<- function(scm,state){
  stopifnot("state must have the names of the state"=is.null(names(state))==FALSE)
  stopifnot("the order o names in state cant be different than rownames(scm)"=names(state)==rownames(scm))
  stopifnot("the length of state need to be the same than nrow(scm)"=length(state)==nrow(scm))
  stopifnot("cant have NA in the state"=length(which(is.na(state)))==0)
  stopifnot("cant have a rowname that is out the states of matrix"=all(names(state)%in%rownames(scm)))
  stopifnot("the scm need to be a square matrix"=nrow(scm)==ncol(scm))

  result<- c()

  for ( .name in names(state) ){
    result[[.name]]<- sum(state*scm[[.name]])
  }

  names(result)<- names(state)

  stopifnot("cant have NA in the result"=any(is.na(result))==FALSE)

  class(result)<- 'numeric'

  result

}
