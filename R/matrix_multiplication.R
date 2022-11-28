#' @title matrix_multiplication
#' @description do multiplication of two matrices.
#' @param matrix1 the first matrix
#' @param matrix2 the second matrix
#' @return a data.frame of multiplied matrices
#' @references
#' https://online.stat.psu.edu/statprogram/reviews/matrix-algebra/arithmetic#:~:text=To%20perform%20matrix%20multiplication%2C%20the,columns%20of%20the%20second%20matrix.
#' https://en.wikipedia.org/wiki/Matrix_multiplication
#' https://matrix.reshish.com/multCalculation.php
#' Markov Chains, From Theory to Implementation and Experimentation, Paul A. Gagniuc, pags.: 103-104.
#' @export
matrix_multiplication<- function(matrix1,matrix2){
  stopifnot("ncol of 'matrix1' cant be different than nrow 'matrix2'"=ncol(matrix1)==nrow(matrix2))
  # The resulting matrix, known as the matrix product, has the number of rows of the first and the number of columns of
  # the second matrix. source: wikipedia.
  result<- as.matrix(matrix1) %*% as.matrix(matrix2)

  result<- as.data.frame(result)

  result
}