#' @title sample_distDiscrete
#' @description take a sample from a discrete distribution.
#' @param n a integer with the number of samples.
#' @param dist a numeric vector with names, names contains the results
#' and the values contains the probabilities
#' @return a character vector.
#' @export
sample_distDiscrete<- function(n=1,dist){
  stopifnot("cant have NA in dist"=any(is.na(dist))==FALSE)
  sample(names(dist),size=n,replace = TRUE,prob=dist)
}