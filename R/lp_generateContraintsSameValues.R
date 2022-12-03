#' @title lp_generateConstraintsSameValues
#' @description generate a matrix of contraints for linear programmin (lpSolve) to some variables be equal in values
#' ( x1=x2, x2=x3, x3=x4 )
#' @section how this will do that?:
#' this will use multiplying for -1 and 1, to get these functionality, linear programming automaticaly will sum
#' the contraints, so if i write: "-1,1=0" this mean: "-1*x + 1*y=0", when two values are equal when multipliying
#' -1 of one part plus 1 of another one, the result is equal to 0, i will use this logic to construct this function.
#' @param obj.coef the coeficients (constants) of the problem to linear problem. this is a vector.
#' @param obj.coef.levels a vector of factors containing number, when the number is 0 this mean do nothing, when the number
#' is 1 this mean level one, and all variable in obj.coef that has the same index will be place in the same
#' level ( be equal ):
#' for example:
#' 1) you have this obj.coef: c(100,200,80,30,40,90)
#' do you want the values: 100,80 and 40 be equal. AND 200 and 90 be equall too, so 30 is free.
#' you can input this in obj.coef.levels: c(1,2,1,0,1,2).
#' the index of values 100,80 and 40 are level one
#' the index of values 200 and 90 are level two
#' and 30 is level zero (no level).
#' @return a data.frame, this is needed to converto to matrix using as.matrix to pass to package 'lpSolve'.
#' @export
lp_generateConstraintsSameValues<- function(obj.coef,obj.coef.levels){
  result<- data.frame(matrix(0,nrow = 0,ncol=length(obj.coef)))

  for ( .level in levels(obj.coef.levels)[which(levels(obj.coef.levels)!='0')] ){
    .index<- which(obj.coef.levels==.level)
    for ( i in 2:length(.index) ){
      # generate a row with all zeroes, to setting the values
      .zeros<- rep(0,length(obj.coef))
      .zeros[[ .index[[i-1]] ]]<- -1
      .zeros[[ .index[[i]] ]]<- 1
      result[nrow(result)+1,] <- .zeros
    }
  }

  result
}