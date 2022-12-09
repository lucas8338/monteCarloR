#' @title pdf_normal
#' @description the normal distribution probability density function ( pdf ).
#' @param x the target
#' @param mean the mean of the distribution ( crest ).
#' @param stddev the standard deviation of the distribution.
#' @return a double.
#' @references
#' https://web.stanford.edu/class/archive/cs/cs109/cs109.1178/lectureHandouts/110-normal-distribution.pdf
#' @export
pdf_normal<- function(x,mean=1,stddev=1){
  e<- exp(1)
  part1<- 1.0/(stddev*sqrt(2.0*pi))
  part2<- e^(-(1.0/2.0)*((x-mean)/stddev)^2)
  result<- part1 * part2
  result
}