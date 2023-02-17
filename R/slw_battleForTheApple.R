#' @title slw_battleForTheApple
#' @description do the states of the output of the function shinnigamiLeftWing (slw) to battle the actual
#' hipoteses (null hipoteses) vs a oposite hipoteses (alternative hipoteses).
#' is to solve the problem of the outputs of shinnigamiLeftWing function.
#' @section problem:
#' the shinnigamiLeftWing has a problem which two near equal probabilities in their results are near
#' opose equaly, for example: you have two possible columns 'fell' and 'rose', so in the results of the function you get
#' something near, 0.9 fall and 0.89 rose, so the question is: "how to solve this ploblem".
#' @section solution:
#' the solution which this function tries to apply is to get these states which occurer with high opose simultaneous
#' probability and check in the complete data when this happened in the past. for example: if the slw says that when
#' A=2 & B=5: 0.9 fell 0.1 rose, and, C=1 & D=3: 0.11 fell 0.89 rose, just to check when these states happened together
#' is to get the probability of: A=2 & B=5 & C=1 & D=3, with it you will have a level 4 result, with you can use
#' it as a tiebreaker.
#' @section rite:
#' this function will do these steps in their work to solve the slw problem: 1 - get the null and alternative hipoteses,
#' the null hipoteses will be the column which has the maximum value (maximum probability) and the alternative is the
#' remaining, them are data.frames with one column; 2 - order both the data.frames (null hipoteses and alternative hipote
#' sis) decreasingment; 3 - get in 'dataAll' the rows which bot states happen together and count of these occurrences
#' which was the result them.
#' @param dataAll a data.frame containing the raw states data which the function will search in using data1 and data2 rownames.
#' @param data1 data.frame with one column (you can do it using drop=FALSE in a data.frame selector)
#' @param data2 data.frame with one column (you can do it using drop=FALSE in a data.frame selector)
#' @param tPlusX the distance in the future to predict, THIS MUST BE THE SAME WAS USED IN THE shinnigamiLeftWing function.
#' @param labelColumn a character is the name of the column of 'dataAll' which contains the answer.
#' @param minOccurrences the minimum number of occurrences to stop, for example: if two states not occurs together or
#' occurs with a low number of times the function will try with the next value of the 'alternative hipoteses' against the
#' 'null hipoteses', always will be 'alternative vs null' not the inverse. at end of max number of attackers
#' the functil will change the defensor (rowname of null hipoteses) to the next value and will reset the index of the attacker
#' (alternative hipotesis). \cr
#' SET IT TO A HIGH VALUE OR INF TO DISABLE IT and run for all values in data1 and data2.
#' @param maxDefensors a integer is the maximum number of values in the 'null hipoteses'. \cr
#' SET IT TO A HIGH VALUE OR INF TO DISABLE IT.
#' @param maxAttackers a integer is the maximum number of values in the 'alternative hipoteses'. \cr
#' SET IT TO A HIGH VALUE OR INF TO DISABLE IT.
#' @return a data.frame with these columns: {column of data1} {column of data2} defensor.index attacker.index \cr
#' defensor.index: is the 'rank' of the data.frame containing the probability of the null hipotesis (highest probability). \cr
#' attacker.index: is the 'rank' of the data.frame containing the probability of the alternative hipotesis. \cr
#' for example: if the defensor.index=1 this means that the defensor will be the value of the highest probability in the
#' null hipotesis, and if this value is: defensor.index=2 this means the value used is the secong highest. the same with
#' the 'attacker.index'.
#' @import dplyr
#' @export
slw_battleForTheApple<- function(dataAll, data1, data2, tPlusX, labelColumn='label', minOccurrences=3, maxDefensors=10, maxAttackers=10){

  result<- data.frame(matrix(nrow = 0, ncol = 4))
  colnames(result)<- c(colnames(data1), colnames(data2), 'defensor.index', 'attacker.index')
  # store the colnames of result, cause the colnames will not change anymore
  result.colnames<- colnames(result)

  # assign with data will be the null hipoteses and with will be the alternative
  if ( max(data1) > max(data2) ){
    hip.null<- data1
    hip.alt<- data2
  }else{
    hip.null<- data2
    hip.alt<- data1
  }

  # order the hip.null decreasing
  hip.null<- hip.null[order(hip.null[,1], decreasing = TRUE),, drop=FALSE]
  # order the hip.alt decreasing
  hip.alt<- hip.alt[order(hip.alt[,1], decreasing = TRUE),, drop=FALSE]

  bothOcurred<- function(data, state1, state2){
    # description return the indexes 'rows numbers' which two states happened together
    # data the data where to search
    # state1 a named character vector
    # state2 a named character vector
    # return a numeric vectow with the indexes 'rows numbers'
    state1.names<- names(state1)
    state2.names<- names(state2)
    # check if the state is in the row, look the 'd' variable is accessed by the names 'd'[stateX.names]
    # this way will check if the NAMES and the values are equal
    .logical<- apply(data, 1, function(d){all( all((state1 == d[state1.names])) & all((state2 == d[state2.names])) )})
    which(.logical)
  }

  hip.null.names<- rownames(hip.null)
  hip.null.names.length<- length(hip.null.names)
  hip.alt.names<- rownames(hip.alt)
  hip.alt.names.length<- length(hip.alt.names)
  # store the index of the actual row of the hip.null data.frame
  defensor.index<- 1
  # store the index of the actual row of the hip.alt data.frame
  attacker.index<- 1
  while (TRUE){
    defensor<- hip.null.names[[defensor.index]]
    attacker<- hip.alt.names[[attacker.index]]
    # name of defensor as a named vector
    defensor.vec<- slw_stateToVector(defensor)
    # name of attacker as a named vector
    attacker.vec<- slw_stateToVector(attacker)
    # number of rows which both states occurred
    occurrencesIndexes<- bothOcurred(dataAll, attacker.vec, defensor.vec) + tPlusX

    newName<- paste0(attacker, ' & ', defensor)

    # bellow the loop will store the number of occurrences of each column of result
    # to the result data.frame
    for ( j in seq_along(colnames(result)) ){
      # store the equivalent to column 1
      result[newName, 1]<- which(dataAll[occurrencesIndexes, labelColumn] == result.colnames[[1]]) %>% length()
      # store the equivalent to column 2
      result[newName, 2]<- which(dataAll[occurrencesIndexes, labelColumn] == result.colnames[[2]]) %>% length()
      # store the indexes in the result
      result[newName, c('defensor.index', 'attacker.index')]<- as.integer(c(defensor.index, attacker.index))
    }

    # if the number of occurrences is lower than a minimum aceptable
    # try with the next attacker, the max attackers are limited.
    if ( length(occurrencesIndexes) < minOccurrences && attacker.index < maxAttackers && attacker.index < hip.alt.names.length ){
      attacker.index<- attacker.index + 1
    }else if ( attacker.index == maxAttackers || attacker.index == hip.alt.names.length ){
      # if after try with all attacker there's not a minimum necessaries occurrences with
      # the defensor, so will try with another defensor
      defensor.index<- defensor.index + 1
      attacker.index<- 1
    }else{
      break
    }
    # will stop if the number of defensor reach to a maximum
    if ( defensor.index > maxDefensors || defensor.index > hip.null.names.length ){
      warning("the maximum number of defensors are reached without hit the minOccurrences")
      break
    }
  }
  # return
  result
}
