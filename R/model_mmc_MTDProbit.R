#' @title model_mmc_MTDProbit
#' @description implementation of a multivariate markov chains MTD-Probit model from their paper.
#' @param data a data.frame with sequences.
#' @param j the number of the column in the data is the endog (who will be returned the probabilities).
#' @param l the number of order.
#' @param nj0 a initial value (as said in the paper it can be zero: a constant term nj0 is introduced in the Pfij
#' specification, and in this way, the proposed specification involves one additional parameter in comparison with the
#' MTD case; although it can be set to zero, nj0 generally improves the fit (i.e. allows the probability Pfij to be
#' closer to Pj).
#' @param fiFunc can be any probability function, the default is the cumulative distribution function of a standard normal
#' distribution. (cdf of normal function with mean=0 and stddev=1). but as said in the paper: fi can be replaced by
#' another distribution function of any continuous random variable with state space of Real.
#' @param s the number of sequences.
#' @param t the actual time.
#' @param options ia a list with parameters for the model, these parameters are:
#' \itemize{
#' \item{opt.opts: }{parameters to be passed to the parameter 'opts' of the function nloptr::nloptr}
#' }
#' @return a numeric vector with values (probabilities).
#' @references
#' A New Model for Multivariate Markov Chains
#' JOÃO NICOLAU
#' doi: 10.1111/sjos.12087
#' @import dplyr
#' @import utils
#' @export
model_mmc_MTDProbit<- function(data,j,l,nj0=0,fiFunc=function(d){cdf_normal(d,mean = 0,stddev = 1)},s=ncol(data),t=nrow(data), options= list()){
  ########################################################################################################################
  #| configuration of arguments and parameters
  ########################################################################################################################

  defaultOptions<- list("opt.opts"=list("algorithm"="NLOPT_LN_SBPLX","maxeval"=1000))

  options<- modifyList(defaultOptions,options)

  #/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
  ########################################################################################################################
  #| estimates nii (transation occurrences)
  ########################################################################################################################

  nii<- list()
  for ( .s in 1:s ){
    for ( .l in 1:l ){
      idx<- glue::glue("j={j},s={.s},l={.l}")
      endog<- data[[ j ]]
      exog<- data[[ .s ]]
      exog_state<- exog[[ t-.l ]] %>% as.character()
      nt<- matrix_createMultivariateFromExogCom(endog,exog,.l)[exog_state,]
      ntNames<- colnames(nt)
      nt<- nt %>% as.numeric()
      names(nt)<- ntNames
      nii[[ idx ]]<- nt
    }
  }

  #/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
  ########################################################################################################################
  #| estimates Pjsl
  ########################################################################################################################

  Pjsl<- list()
  for ( .name in names(nii) ){
    idx<- .name
    Pjsl[[ idx ]]<- nii[[ .name ]] / sum(nii[[ .name ]])
  }

  #/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
  ########################################################################################################################
  #| main function MTD-Probit.
  ########################################################################################################################

  mtdProbitMain<- function(nj0,njs){
    # this is the main function of the mtd probit the formula (5) in the paper.
    # nj0 is the nj0 in the formula is a single value.
    # njs are the parameters to be found by optimization.

    stopifnot(length(njs)==length(Pjsl))

    toSum<- list()
    for ( i in 1:(length(Pjsl)) ){
      toSum[[ i ]]<- njs[[ i ]] * Pjsl[[ i ]]
    }
    result<- nj0 + Reduce('+',toSum)
    result<- fiFunc(result)

    divisor<- sum(result)
    result<- result / divisor

    result
  }

  #/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
  ########################################################################################################################
  #| logL function
  ########################################################################################################################

  logL<- function(nj0,njs){
    # this is the default optimization problem, formula (6) in the paper.
    # nj0 and njs will be both passed to the mtdProbitMain.

    logPfij<- mtdProbitMain(nj0,njs) %>% log()
    toSum<- list()
    for ( i in 1:(length(nii)) ){
      toSum[[ i ]]<- nii[[ i ]] * logPfij
    }
    result<- Reduce('+',toSum)

    result
  }

  #/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
  ########################################################################################################################
  #| estimate njs
  ########################################################################################################################

  opt.func<- function(njs){
    # function to be passed to optimizer when type='dependent'.
    loss<- logL(nj0,njs)
    # the return of this function is multiplied by -1 cause the nlopt package only works with minimization
    # and dont work with maximization, a maximization can be solved by the inverse of the negative of the loss.
    # argmin -loss == argmax loss.
    # uptim this function (with sum of the distribution) is equivalent to using the maxLik package without the sum.
    -sum(loss)
  }

  opt.solution<- nloptr::nloptr(x0= rep(1,length(Pjsl)),
                        eval_f= opt.func,
                        opts = options$opt.opts
  )

  njs<- opt.solution$solution

  #/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
  ########################################################################################################################
  #| run main function then return results
  ########################################################################################################################

  result<- list()

  result[[ 'distribution' ]]<- mtdProbitMain(nj0,njs)
  result[[ 'opt' ]]<- opt.solution

  result
}