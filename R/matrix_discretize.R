#' @title matrix_discretize
#' @description transform a data.frame with continuous values into a data.frame
#' with 'x<=VALUE' value, only numeric columns will be transformed.
#' @param data a data.frame.
#' @param n a integer with the maximum number of discrete values.
#' @param n.custom takes a list with the name of the column and the number of chunk.
#' @import dplyr
#' @export
matrix_discretize<-function(data,n,n.custom=NULL){
  # select only numeric columns from data
  data<- dplyr::select_if(data,is.numeric)

  pg<- progress::progress_bar$new(total=length(colnames(data)),format=libGetDataR::util.progress_format())
  for ( colname in colnames(data) ){
    pg$tick()
    uniques<- unique(data[[colname]]) %>% sort()
    n<- ifelse(colname%in%names(n.custom),n.custom[[colname]],n)
    breaks<- vector_splitIntoChunks(uniques,n)
    
    pgi<- progress::progress_bar$new(total=length(breaks),format=libGetDataR::util.progress_format())
    for ( .break in breaks ){
      pgi$tick()
      .min<- min(.break)
      .max<- max(.break)
      data[which(data[,colname] %in% .break),colname]<- glue::glue("[{.min},{.max}]")
    }
    data[[colname]]<- as.factor(data[[colname]])
  }
  data
}