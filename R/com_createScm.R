#' @title com_createScm
#' @description create a scm (StoChastic Matrix) from a com (Conditional Occurrence Matrix).
#' basicaly what this will do id to divide each value for the sum of the values of their row.
#' @param com the Conditional Occurrence Matrix.
#' @return a data.frame with probabilities. a stochastic matrix.
#' @references
#' Markov Chains, From Theory to Implementation and Experimentation, Paul A. Gagniuc, chapter: 3.
#' @export
com_createScm<- function(com){
  # tscm means: Temporary scm
  tscm<- com

  # takes return a vector with the sum of the all values, of the each row.
  rs<- rowSums(com)

  # divite by total to get probabilities.
  tscm<- tscm/rs

  # for possible future usage
  class(tscm)<- c("scm","data.frame")

  tscm
}