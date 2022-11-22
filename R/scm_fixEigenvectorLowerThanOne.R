#' @title scm_fixEigenvectorLowerThanOne
#' @description will fix values that where their eigenvector (sum of all values in a row) is
#' lower than one, this can happen for example in the division, the division
#' by a number can return a repeating decimal (primes are very common).
#' Rows with eigenvector that dont end on exactly one will be
#' fixed using the formula:
#' lowerstValueOfRow= 1-(A-L)
#' A: sum of all values in the row
#' L: the lowerst value in the row.
#' @param scm a data.frame with a stochastic matrix
#' @param maximumDifference the maximum difference from one to accept to fix these values.
#' will raise an error if the difference is highter than this param value.
#' @return a scm (stochastic matrix) with fixed values now the rum of each
#' row will be equal to one.
#' @export
scm_fixEigenvectorLowerThanOne<- function(scm,maximumDifference=0.02){

  # get the eigenvectors (sum of rows)
  eigenvectors<- rowSums(scm)

  # fix if the eigenvector is lower than one
  # iter for each "name" in of the indexes are lower than one
  for ( .rowName in names(eigenvectors[which(eigenvectors<1)]) ){
    # get the name of the column with the lowerst value
    columnNameLowerValue<- names(which.min(scm[.rowName,]))
    # check if the difference of value to fix to one is bigger than the maximum acceptable.
    stopifnot(1-sum(scm[.rowName,])<=maximumDifference)
    # set the new value for the eigenvector value be equal 1, this will be done
    # subtracting 1 - sum of all values of the row - the lowest non zero value (this new value).
    # nv= 1-(A-L)
    # A: sum of all values in the row
    # L: the value of the lowest value in the row
    scm[.rowName,columnNameLowerValue]<- 1-(sum(scm[.rowName,])-scm[.rowName,columnNameLowerValue])
  }

  scm

}