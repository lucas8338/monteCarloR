#' @title util_countNOccurrencesOfEachRowString
#' @description count how much times a row of string happened in the given data.frame.
#' this work only with strings (character).
#' @param uniques: a data.frame with uniques values to be counted.
#' @param data: the complete data.frame where to count.
#' @return a numeric vecto these values are the number of occurrences for each row in uniques
#' happened in data.
#' @useDynLib monteCarloR
#' @export
util_countNOccurrencesOfEachRowString<- function(uniques, data){
  p_uniques<- as.character(unlist(uniques))
  p_data<- as.character(unlist(data))
  p_nrow_uniques<- as.integer(nrow(uniques))
  p_nrow_data<- as.integer(nrow(data))
  p_ncol<- ncol(uniques)
  p_out<- as.integer(rep(0, p_nrow_uniques))
  result<- .C("RC_countNOccurrencesOfEachRowString",
              p_uniques,
              p_data,
              p_nrow_uniques,
              p_nrow_data,
              p_ncol,
              p_out
  )
  result<- result[[6]]
  result
}