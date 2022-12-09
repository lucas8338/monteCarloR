#' @title mat_erf
#' @description the erf ( ERror Function ) also called ( gauss error function ).
#' @param x a double for the function.
#' @return a double.
#' @export
mat_erf<- function (x){
  pracma::erf(x)
}