#' @title model_mc_MTD
#' @description Mixture Transition Distribution model. a model for high order markov chain.
#' @param data a vector of factors.
#' @param l the order.
#' @param t is the actual time, (dont need to be changed).
#' @references
#' The Mixture Transition Distribution Model for High-Order Markov Chains and Non-Gaussian Time Series.
#' André Berchtold and Adrian E. Raftery
#' Statistical Science 2002, Vol. 17, No. 3, 328–356
#' @import dplyr
#' @export
model_mc_MTD<- function(data,l,t=length(data)){
  ########################################################################################################################
  #| estimates probabilities
  ########################################################################################################################

  probs<- list()

  for ( g in 1:l ){
    idx<- glue::glue("g={g}")
    prob<- vector_createCom(data,g) %>% matrix_transitionProbabilities()
    prob<- prob[which(rownames(prob)==data[[ t-g ]]), ] %>% as.numeric()
    names(prob)<- levels(data)
    probs[[ idx ]]<- prob
  }

  #/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
  ########################################################################################################################
  #| the main function
  ########################################################################################################################

  mtdMain<- function(lambdas){
    # this function takes a vector with the values of lambdas and
    # does a linear combination of probs using values of lambdas as constants.
    # this is the function (7) in the paper."
    stopifnot("cant have prob without a lambda"=length(lambdas)==length(probs))
    toDoSummation<- list()
    for ( i in 1:(length(lambdas)) ){
      toDoSummation[[ i ]]<- lambdas[[ i ]] * probs[[ i ]]
    }
    result<- Reduce('+',toDoSummation)
    result
  }

  #/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
  ########################################################################################################################
  #| mtd log likelihood function
  ########################################################################################################################

  mtdLogLikelihood<- function(lambdas){
    # a function to calculate the log like lihood to estimates the lambda values.
    # as in the function (28) in the paper.
    prob<- mtdMain(lambdas)
    ll<- log(prob) %>% sum()
    ll
  }

  #/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
  ########################################################################################################################
  #| estimates lambdas
  ########################################################################################################################
  # the objective to find the values of lambdas is to find values that multiplied by the probabilities maximizes
  # the logLikelihood

  opt.func<- function(lambdas){
    loss<- mtdLogLikelihood(lambdas)
    # the loss returned is multiplied by -1 ( negative ) cause the nlopt function only minimizes, so the way
    # to tranform a minimization problem into a maximization is inverting the loss, (multiplying by -1)
    -loss
  }

  # the constraint function formula (9) in the paper ( the nlopt minimizes this function ).
  opt.const<- function(lambdas){
    abs(sum(lambdas)-1)
  }

  opt.solution<- nloptr::nloptr(x0=rep(1,length(probs)),
                 eval_f = opt.func,
                 eval_g_ineq = opt.const,
                 lb = rep(0,length(probs)),
                 ub = rep(1+1e-3,length(probs)),
                 opts = list('algorithm'='NLOPT_LN_COBYLA','maxeval'=Inf)
                 )

  lambdas<- opt.solution$solution

  stopifnot( "the sum of lambdas need to be equal to one (with a very low tolerance)."=abs(sum(lambdas)-1)<=1e-3 )

  #/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
  ########################################################################################################################
  #| do calculations and return the result
  ########################################################################################################################

  result<- mtdMain(lambdas)
  # constrant as in the formula (8) in the paper.
  stopifnot("the sum of the result cant be different than one (with a very low tolerance) or this is not a probability."=abs(sum(result)-1)<=1e-3)
  result
}