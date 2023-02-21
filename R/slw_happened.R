#' @title slw_happened
#' @description check if/which rownames states (shinnigamiLeftWing rowname) happened in the current data.frame
#' (a data.frame with one row)
slw_happened<- function(current, states){
  stopifnot("the nrow(current) cant be different than 1"=nrow(current)==1)
  # variable will store the result
  result<- c()
  # main loop to work if length(states) > 1
  for ( i in seq_along(states) ){
    # actual state as vector
    state<- slw_stateToVector(states[[i]])
    # check if the current happened
    if ( all(current[, names(state)] == state) ){
      result[[ length(result) + 1 ]]<- i
    }
  }
  # return the result as numeric.
  as.numeric(result)
}
