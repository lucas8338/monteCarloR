#' @title vector_occurrencyProbability
#' @description a very simple probability of vector to came that value (factor).
#' it is basicaly nOccurrencyOfThatFactor/lengthOfVector.
#' @param vec the data vector.
#' @return a named numeric vector with probabilities
vector_occurrencyProbability<- function(vec){
  result<- c()

  for ( .level in levels(vec) ){
    result[[.level]]<- length(vec[which(vec==.level)]) / length(vec)
  }

  class(result)<- 'numeric'

  result
}