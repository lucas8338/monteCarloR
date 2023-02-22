#' @title slw_whichRowsHappens
#' @description returns the number of rows which states happened.
#' @param dataAll the complete data.frame
#' @param states a character with the pattern of shinnigamiLeftWing rownames
#' @return a numeric vector
#' @import dplyr
#' @export
slw_whichRowsHappens<- function(dataAll, states){
  states.vectorized<- slw_stateToVector(states)
  states.vectorized.names<- names(states.vectorized)
  rowsHappened<- apply(dataAll, 1, function(d){all( states.vectorized == d[states.vectorized.names] )}) %>% which()
  rowsHappened
}