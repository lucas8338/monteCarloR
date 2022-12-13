#' @title cdf_normal
#' @description the cumulative density function ( cdf ) of normal distribution.
#' @param x a double.
#' @param mean a double is the mean ( crest ).
#' @param stddev a double the standard deviation.
#' @return a double
#' @export
cdf_normal<- function(x,mean=0,stddev=1){
  1/2 * ( 1 + mat_erf( (x-mean)/(stddev*sqrt(2)) ) )
}