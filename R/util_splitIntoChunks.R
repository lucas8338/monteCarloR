#' @title util_splitIntoChunks
#' @description function will split data into chunks
#' @param data a vector
#' @param n the number of chunks
#' @return a list with values
#' @references
#' https://www.geeksforgeeks.org/split-vector-into-chunks-in-r/
#' @import dplyr
util_splitIntoChunks<- function(data,n){
  split(data, cut(seq_along(data),n,labels = FALSE)) %>% unname()
}
