#' @title vector_discretize
#' @description transform a numeric vector into a discrete
#' @param data a numeric vector.
#' @param mode how to do the discretization? can be: "all","n.factors","fixed.minMaxStep"
#' @param options a list with the param for that mode. each mode there itself parameters.
#' @section mode= "all":
#' is the default mode it just transform the data into factor return(as.factor(data))
#' this therent params.
#' @section mode= "n.factors":
#' split the data into numbers of factors.
#' n: a integer is how much factors to split
#' @section mode= "fixed.minMaxStep":
#' equaly spaced factors, from a min to a max each step. this will use the 'seq' function to generate a sequence
#' which these parameters.
#' the factors of this mode will be: [min, max).
#' min: a double is the minimum value.
#' max: a double is the maximum value.
#' step: a double is the 'step' for the sequence, this will be the 'by' of the function 'seq'.
#' @return a factor vector.
#' @import dplyr
#' @export
vector_discretize<-function(data,mode='all',options=list()){
  if ( mode=='all' ){
    data<- as.factor(data)
    return(data)
  }

  if ( mode=='n.factors' ){
    n<- options[['n']]
    uniques<- unique(data) %>% sort()
    chunks<- vector_splitIntoChunks(uniques,n=n)
    for ( chunk in chunks ){
      .min<- min(chunk)
      .max<- max(chunk)
      data[which(data %in% chunk)]<- glue::glue("[{.min}, {.max}]")
    }
    data<- as.factor(data)
    return(data)
  }

  if ( mode=='fixed.minMaxStep' ){
    min<- options[['min']]
    max<- options[['max']]
    step<- options[['step']]
    stopifnot(min(data)>=min)
    stopifnot(max(data)<=max)

    .levels<- seq(from = min,to = max,by = step)
    # a vector which will store all possible levels
    allLevels<- c()

    for ( i in 2:length(.levels) ){
      .min<- .levels[[ i-1 ]]
      .max<- .levels[[ i ]]
      .lvStr<- glue::glue("[{.min}, {.max})")
      allLevels[[length(allLevels)+1]]<- .lvStr
      data[which(data>=.min & data<.max)]<- .lvStr
    }
    # add a missing level is the max
    allLevels[[length(allLevels)+1]]<- as.character(max)
    data<- as.factor(data)
    # add the all laves to the end of levels of data
    attr(data,'levels')<- append(levels(data),allLevels[which(allLevels %in% levels(data)==FALSE)])
    return(data)
  }

}
