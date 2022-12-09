#' @title mat_erfc
#' @description the complementary error function ( erfc ).
#' @param x a double for the function.
#' @return a double
#' @export
mat_erfc<- function(x){
  pracma::erfc(x)
}