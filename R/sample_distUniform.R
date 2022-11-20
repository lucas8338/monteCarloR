#' @title sample_distUniform
#' @description get a sample from a uniform distribution
#' @param n means "numbers" is the number of samples to get.
#' @param min the minium return value.
#' @param max the maximum return value.
#' @return a double.
#' @export
sample_distUniform<- function(n=1,min=0,max=1){
  dqrng::dqrunif(n=n,min=min,max=max)
}