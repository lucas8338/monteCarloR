#' @title slw_appleChoice
#' @description counts the similatiries between a row of a data.frame and rows of a data.frame which that states
#' happened (current).
#' @param dataAll a data.frame containing all datas.
#' @param current a data.frame with one row containing actual row data (the colnames of this data.frame needs to be qual
#' to the dataAll data.frame).
#' @param states a character (string) containing the states (following the rownames standard in shinnigamiLeftWing)
#' @param labelColumn the name of the column containing the labels.
#' @param tPlusX the predict horizon (as in shinnigamiLeftWing).
#' @return a list with the states names and a duplicates numeric containing the number of duplicates in each label state.
#' @import dplyr
#' @export
slw_appleChoice<- function(dataAll, current, states, labelColumn, tPlusX){
  stopifnot("colnames of 'current' cant be different from the colnames in 'dataAll'"=colnames(current) == colnames(dataAll))
  # contains all possible col levels
  levels<- levels(dataAll[, labelColumn])
  # transform 'dataAll' and 'current' factors columns into a character
  for ( i in seq_along(colnames(dataAll)) ){
    dataAll[[i]]<- as.character(dataAll[[i]])
    current[[i]]<- as.character(current[[i]])
  }
  # transform tthe 'states' character into a named vector
  states.vector<- slw_stateToVector(states)
  # columns which not in states
  reducedColnames<- colnames(dataAll)[ which(colnames(dataAll) %in% names(states.vector) == FALSE) ]
  # a numeric vector containing which rows the 'states' hapenned
  rowsHappened<- slw_whichRowsHappens(dataAll, states)
  # a variable to store the results
  result<- list()
  # iterate over levels
  for ( i in seq_along(levels) ){
    # store the actual level of thsis iteration
    actualLevel<- levels[[i]]
    # get which in the future of dataAll the label is correct (prediction was correct).
    result[[ actualLevel ]]<- rowsHappened[which(dataAll[rowsHappened + tPlusX, labelColumn] == actualLevel)]
    # result[[ actualLevel ]] uses the indexes to take the data.frames with reduced colnames.
    result[[ actualLevel ]]<- dataAll[result[[ actualLevel ]], reducedColnames]
    # which values in current happened on each value of result[[ actualLevel ]] data.frame?.
    for ( j in seq_along(rownames(result[[ actualLevel ]])) ){
      result[[ actualLevel ]][j, ]<- result[[ actualLevel ]][j, reducedColnames] == current[, reducedColnames]
    }
    # sums of values which are true in rows (is needed to convert them to local first cause them are characters).
    result[[ actualLevel ]][[ 'trueSums' ]]<- apply(result[[ actualLevel ]], 1, function(d){sum(as.logical(d), na.rm = TRUE)})
  }
  # prealoc the 'duplicates' in result which will store the number of duplicates over above iterations
  result[['duplicates']]<- integer(length(result))
  # set names to result[['duplicates']]
  names(result[['duplicates']])<- levels
  # iterate over each value in result (if is not the duplicates)
  for ( i in seq_along(which(names(result) != 'duplicates')) ){
    result[['duplicates']][[ names(result)[[i]] ]]<- duplicated(result[[i]]) %>% which() %>% length()
  }
  # return the result
  result
}
