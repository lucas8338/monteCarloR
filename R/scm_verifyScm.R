#' @title scm_verifyScm
#' @description this function verifies if the scm is ready for usage.
#' @param scm a data.frame with a stochastic matrix.
#' @return the same input. but if the scm is not valid will raise an error.
#' @references
#' Markov Chains, From Theory to Implementation and Experimentation, Paul A. Gagniuc, chapter: 3.
#' @export
scm_verifyScm<- function(scm){
  stopifnot("cant have NA's in the 'scm'."=any(is.na(scm))==FALSE)
  stopifnot("all rownames need to be in cownames vice versa."=all(rownames(scm) %in% colnames(scm)) && all(colnames(scm) %in% rownames(scm)))
  stopifnot("the stochastic matrix need to be a square stochastic matris (number of rows need to be equal to number of columns)"=nrow(scm)==ncol(scm))
  # obviously this row verifies too if all values in a data.frame are numeric.
  stopifnot("all eigenvectors (sum of values in a row) cant be different than one."=all(rowSums(scm)==1.0))
  scm
}
