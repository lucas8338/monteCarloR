#' @title slw_stateToVector
#' @description tranform a state standard of shinnigamiLeftWing to a named vector of character
#' @param data the state character
#' @return a character vector
#' @export
slw_stateToVector<- function(data){
  splitted<- strsplit(data, '=| & ') %>% unlist()
  # get only the names
  names<- splitted[seq(1, length(splitted), 2)]
  # get only the values
  values<- splitted[seq(2, length(splitted), 2)]
  # assign the names to values
  names(values)<- names
  values
}