#' @title df_discretize
#' @description transform a data.frame with continuous values into a data.frame
#' with 'x<=VALUE' value, only numeric columns will be transformed.
#' @param data a data.frame.
#' @param n a integer with the maximum number of discrete values.
#' @param n.custom takes a list with the name of the column and the number of chunk.
#' @import dplyr
#' @export
df_discretize<-function(data,n,n.custom=NULL){
  for ( colname in colnames(data) ){
    uniques<- unique(data[[colname]])
    if ( libGetDataR::check.is_characterNumeric(uniques) ){
      uniques<- uniques %>% as.numeric() %>% sort()
    }else{
      uniques<- uniques %>% sort()
    }
    if ( length(uniques)>1 && is.numeric(data[[colname]]) ){
      chunks<- util_splitIntoChunks(uniques,ifelse(colname%in%names(n.custom),n.custom[[colname]],n))
      for ( part in chunks ){
        lastPartValue<- part[[length(part)]]
        data[[colname]][which(suppressWarnings(as.numeric(data[[colname]]))<=lastPartValue)]<- glue::glue("x<={lastPartValue}")
      }
      data[[colname]]<- as.factor(data[[colname]])
    }
  }
  data
}