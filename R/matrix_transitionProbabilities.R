#' @title matrix_transitionProbabilities
#' @description calculate the transition probabilities from a com (Conditional Occurrence Matrix).
#' basicaly what this will do id to divide each value for the sum of the values of their row.
#' @param com the Conditional Occurrence Matrix.
#' @return a data.frame with probabilities. instead number of occurrences.
#' @references
#' Markov Chains, From Theory to Implementation and Experimentation, Paul A. Gagniuc, chapter: 3.
#' @export
matrix_transitionProbabilities<- function(com){
  # tscm means: Temporary scm
  tscm<- com

  # takes return a vector with the sum of the all values, of the each row.
  rs<- rowSums(com)

  # divite by total to get probabilities.
  tscm<- tscm/rs

  # replace NA by zero
  tscm[is.na(tscm)]<- 0

  tscm
}