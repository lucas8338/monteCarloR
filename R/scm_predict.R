#' @title scm_predict
#' @description do a prediction/simulation using n-step transition probabilities.
#' @param scm the stochastic matrix.
#' @param state the initial state. a numeric vector containing the names of the states and probabilities.
#' @param horizon a numeric is how much simulate.
#' @param steadyState whether the function should stop when found the steadyState probabilities (stable distribution).
#' @return a data.frame with the columns: choosen state, and the probabilities of each state.
#' @references
#' Markov Chains, From Theory to Implementation and Experimentation, Paul A. Gagniuc, chapter: 4.
#' @export
scm_predict<- function(scm,state,horizon,steadyState=TRUE){
  result<- data.frame(matrix(ncol = length(names(state))+1))
  colnames(result)<- c('state',names(state))
  for ( i in 1:horizon ){
    nstate<- scm_transitionProbability(scm,state = state)
    # check for readyState
    if ( steadyState==TRUE ){
      if ( all(nstate==state) ){
        break
      }
    }
    state<- nstate
    result[i,'state']<- names(state)[which.max(state)]
    result[i,names(state)]<- state
  }
  result
}


