#' @title slw_happened
#' @description check if/which rownames states (shinnigamiLeftWing rowname) happened in the current data.frame
#' (a data.frame with one row)
#' @param current a data.frame with one row.
#' @param states a character vector containing at least one character.
#' @param levels a integer vector containing the levels to return. this should to be the same level than the states cause
#' if for example, the states contains characters of level3 but you choose level1, so any value with any column like
#' in that state will be returned.\cr
#' example: states= "exog1=2 & exog2=5 & exog3=2", levels=1; in this case a row which contains only exog1=2... is a valid.
#' @return the positions in 'states' vector which happens in 'current'.
#' @import dplyr
#' @export
slw_happened<- function(current, states, levels){
  stopifnot("the nrow(current) cant be different than 1"=nrow(current)==1)
  # current to the standard
  current<- paste0(colnames(current), '=', apply(current, 2, as.character))
  # prealoc a data.frame to store the detections
  detected<- data.frame(matrix(nrow = length(states), ncol = length(current)))
  # a progress bar
  pb<- progress::progress_bar$new(format=":percent (:current/:total) at :tick_rate/s | eta: :eta | elapsed: :elapsedfull", total=length(current))
  # the main loop
  for ( i in seq_along(current) ){
    # update the progress bar
    pb$tick()
    # do the detection and add it to the 'detected' data.frame
    detected[, i]<- stringr::str_detect(states, current[[i]])
  }
  # terminate the progress bar
  pb$terminate()
  # filter the valids by the index of wanted levels
  valids<- which(rowSums(detected) %in% levels)
  # return the valids
  valids
}
