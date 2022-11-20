#' @title scm_multiply
#' @description do matrix multiplication
#' @param scm1 data.frame the first scm
#' @param scm2 data.frame the second scm
#' @return a multiplied stochastic matrix
#' @references
#' https://www.khanacademy.org/math/precalculus/x9e81a4f98389efdf:matrices/x9e81a4f98389efdf:multiplying-matrices-by-matrices/v/matrix-multiplication-intro
#' @export
scm_multiply<- function(scm1,scm2){
  # verify if data.frames are compatibles (equals)
  stopifnot("matrices of different dimensions cant be multiplied"=nrow(scm1)==nrow(scm2) && ncol(scm1)==ncol(scm2))
  # verify if them are square
  stopifnot("this dont work with retangular matrix"=nrow(scm1)==ncol(scm1))

  result <- data.frame(matrix(nrow = max(c(nrow(scm1),nrow(scm2))),ncol = max(c(ncol(scm1),ncol(scm2)))))

  for ( .col in 1:(ncol(scm1)) ){
    for ( .row in 1:(nrow(scm1)) ){
      result[.row,.col]<- sum(scm1[.row,] * scm2[,.col])
    }
  }

  colnames(result)<- colnames(scm1)
  rownames(result)<- rownames(scm1)

  class(result)<- c("scm","data.frame")

  result
}