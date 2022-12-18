#' @title math_erfc
#' @description the complementary error function ( erfc ).
#' @param x a double for the function.
#' @return a double
#' @export
math_erfc<- function(x){
  pracma::erfc(x)
}