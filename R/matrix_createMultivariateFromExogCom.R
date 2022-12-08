#' @title matrix_createMultivariateFromExogCom
#' @description this is calculate the conditional number of occorrences from levels of a sequence (exog)
#' cause another one (endog) C(endog|exog) occurrence of endog given exog.
#' @param endog a factor vector.
#' @param exog a factor vector, the levels need to be the same as in endog.
#' @return a data.frame with the number of occurrences.
#' @export
matrix_createMultivariateFromExogCom<- function(endog,exog,tPlusX=1L){
  # from rows causes columns
  result<- data.frame(matrix(nrow = length(levels(exog)),ncol = length(levels(endog))))
  rownames(result)<- levels(exog)
  colnames(result)<- levels(endog)
  # the column endog will be leaded (slided to up / backward in the time) at
  # actual time endog is given the future 'tPlusX' data.
  times<- data.frame('exog'=exog,'endog'=dplyr::lead(endog,n=tPlusX))
  
  for ( .rowName in rownames(result) ){
    for ( .colName in colnames(result) ){
      result[.rowName,.colName]<- length(which(times[['exog']]==.rowName & times[['endog']]==.colName))
    }
  }
  result

}