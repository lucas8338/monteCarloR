#' @title matrix_power
#' @description do 'power' (exponentiation) of matrix
#' @param .matrix the matrix to be 'powered'
#' @param n the exponential.
#' @return a data.frame with the powered matrix.
#' @references
#' #' Markov Chains, From Theory to Implementation and Experimentation, Paul A. Gagniuc, pags.: 103-104.
#' @export
matrix_power<- function(.matrix,n){
  # check whether the number is a integer (without decimal).
  stopifnot("for now this function cant do power by number with decimals (i dont know even this is possible)."=n%%1==0)

  # if the n=1 power by 1 is itself.
  if ( n==1 ){
    return(.matrix)
  }

  result<- .matrix
  # is 'n-1' cause the loop starts on one.
  for ( i in 1:(n-1) ){
    # is needed to multiply the original matrix by the previouly multiplied
    # this way we have the 'power' functionality
    result<- matrix_multiplication(.matrix,result)
  }
  result
}