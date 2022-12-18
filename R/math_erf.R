#' @title math_erf
#' @description the erf ( ERror Function ) also called ( gauss error function ).
#' @param x a double for the function.
#' @return a double.
#' @export
math_erf<- function (x){
  pracma::erf(x)
}