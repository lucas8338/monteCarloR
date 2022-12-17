#' @title model_mmc_MTDProbit
#' @description a multivariate markov chains model implemented from their paper MTD-Probit.
#' @param data a data.frame with factors.
#' @param j the index of the endog column.
#' @param l the number of order.
#' @param nj0 a initial value (as said in the paper it can be zero).
#' @param type it can be 'dependent' or 'indepenent', dependent is if the levels of all columns in the data are equals.
#' 'independent' is if the them are different.
#' @param fiFunc is a function which takes a vector with probabilities and returns a cdf (cumulative distribution function)
#' value for each the the default is the standard normal distribution (mean = 0, stddev = 1), but as said in the paper it can be
#' replaceb by any cdf that works with continum values and works with state space Reals (Real numbers, math).
#' @param s the the number of sequences in the data (this will only used to the exog).
#' @param t the present time in the data (will be used to take the previous values).
#' @references
#' A New Model for Multivariate Markov Chains
#' JOÃO NICOLAU
#' doi: 10.1111/sjos.12087
#'
#' The Profitability in the FTSE 100 Index: A New Markov Chain Approach
#' Flavio Ivo Riedlinger, João Nicolau
#' doi: 10.1007/s10690-019-09282-4
#' @import dplyr
#' @export
model_mmc_MTDProbit<- function(data, j, l, nj0, type, fiFunc = function(d){cdf_normal(d,mean=0,stddev=1)}, s = ncol(data), t = nrow(data)){
  # this implementation will have two ways to find the values of njs (the lambda), the first way
  # is when the levels of all datas are equal, for this, the formula (6) can be used
  # but when the levels of the datas are different the optimization problem
  # becomes the formula (7a) and (7b)

  stopifnot(type=='dependent' || type=='independent')

  ########################################################################################################################
  #| estimate Pjst
  ########################################################################################################################

  Pjst<- list()

  for ( .s in 1:s ){
    for ( .l in 1:l ){
      idx<- glue::glue("j={j},s={.s},l={.l}")
      endog<- data[[ j ]]
      exog<- data[[ .s ]]
      prob<- matrix_createMultivariateFromExogCom(endog,exog,tPlusX = .l) %>% matrix_transitionProbabilities()
      prob<- prob[ which( rownames(prob)==exog[[ t-.l ]] ) ,]
      probNames<- colnames(prob)
      prob<- prob %>% as.numeric()
      names(prob)<- probNames
      Pjst[[ idx ]]<- prob
    }
  }

  #/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
  ########################################################################################################################
  #| main mtd-probit function
  ########################################################################################################################

  mtdProbitMain<- function(nj0,njs,applyFi=TRUE){
    # this is the main function of the model mtd-probit formula (5) in the paper, it takes a initial value (can be zero)
    # and the value of njs (is equivalent of lambdas in others models).
    # the 'applyFi' argument is if or not to apply the fiFunc to the result of the dividend. (this will be used by
    # the independent logLikelihood function).
    stopifnot(length(njs)==length(Pjst))
    toDoSummation<- list()
    for ( i in 1:(length(Pjst)) ){
      toDoSummation[[ i ]]<- njs[[ i ]] * Pjst[[ i ]]
    }
    result<- nj0 + Reduce('+',toDoSummation)
    if ( applyFi == TRUE ){
      result<- fiFunc(result)
    }else{
      result<- result
    }

    # bellow will do the division, the objective of the division is to tranform the result which are densities
    # (sum of values are different than one) into probabilities to do this i will to divide each value of the upper part
    # (result (divident)) by the summatory of it.
    divisor<- sum(result)
    for ( i in 1:(length(result)) ){
      result[[ i ]]<- result[[ i ]] / divisor
    }

    result
  }

  #/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
  ########################################################################################################################
  #| log likelihood
  ########################################################################################################################

  logL<- function(nj0,njs){
    # this is the log likelihood (LL) formula (6) in the paper.this function is to find the values of the njs but it is
    # to be used only when the levels of all data are equals.
    prob<- mtdProbitMain(nj0,njs)
    prob<- prob %>% log() %>% sum()
    prob
  }

  #/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
  ########################################################################################################################
  #| independent logL
  ########################################################################################################################

  ilogL<- function(nj0,njs){
    # this is the formula (7a) in the paper.
    # as in the paper says this function needs to be used to find the values of njs when the number of levels
    # (the number of rows of the endog and the exog are different ("the author must be saying about the conditional matrix
    # where rows are levels of exog and columns are levels of endog and rows cause columns"))
    probWOfi<- mtdProbitMain(nj0,njs,FALSE)
    probWfi<- mtdProbitMain(nj0,njs,TRUE)
    prob<- probWOfi - probWfi
    # at the start i had a doubt about the "vertical bar" i dont knew if that is a module of a vector "norm", or
    # the auther was saying that is the abs(), as have a summatory at the start of the formula the return of that
    # "vertical bar" could not to be a unique value, but it would need to return a vector, the only way to this happen
    # is if the "vertical bar" means abs() of each value then squared them and this question in 'stackexchange':
    # https://math.stackexchange.com/questions/1971492/modulus-of-vectors-squared. put a end in the indecision.
    prob<- abs(prob)^2
    prob<- sum(prob)
    prob
  }

  #/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
  ########################################################################################################################
  #| estimates the values of njs
  ########################################################################################################################

  opt.func.dependent<- function(njs){
    # for dependent the objective is maximize the log likelihood.
    loss<- logL(nj0,njs)
    # as the objective is to maximize this function and nloptr package only works with minimization
    # to solve this i need to multiply the return of this function by -1, this way the 'minimization is reversed'
    # working as maximization.
    -loss
  }

  opt.func.independent<- function(njs){
    # for independent the objective is to minimize the function.
    loss<- ilogL(nj0,njs)
    loss
  }

  suppressWarnings(
    opt.solution<- nloptr::nloptr(x0=rep(1,length(Pjst)),
                     eval_f = ifelse(type=='dependent',opt.func.dependent,opt.func.independent),
                     opts = list('algorithm'='NLOPT_LN_COBYLA','maxeval'=Inf)
    )
  )

  njs<- opt.solution$solution

  #/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
  ########################################################################################################################
  #| do calculations and return the result
  ########################################################################################################################

  result<- mtdProbitMain(nj0,njs)

  result

}