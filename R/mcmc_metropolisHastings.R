#' @title mcmc_metropolisHastings
#' @description the metropolis-hastings algorithm, a
#' improvement for metropolis algorithm (metropolis: 1953; hastings: 1970).
#' @param scm a data.frame with a stochastic matrix.
#' @param x_0 a character the initial state.
#' @param n the horizon (num steps) of the sampling.
#' @param dist the stable distribution (pi function).
#' @return a character vector containing the results of sampling
#' @section expected comportment:
#' is expected in low samples the distribution of the results are different than the stable distribution (steadyState)
#' and for long long run the distribution of the results are near equal the stable distribution.
#' @references
#' https://en.wikipedia.org/wiki/Metropolis–Hastings_algorithm
#' https://stephens999.github.io/fiveMinuteStats/MH_intro.html
#' @import dplyr
#' @export
mcmc_metropolisHastings<- function(scm,x_0,n,dist=scm_steadyState(scm)){
  result<- c()

  # initialize the x_t
  x_t<- x_0

  # main loop, look the first value in the array (result is empty) will be always accepted
  # by the acceptance_probability, so at end the length(result) will be equal n ( length(result)==n ).
  # this was tested.
  for ( i in 1:n ){
    # draw a proposed state
    # this is a character.
    # sample y from Q(y|xt). Think of y as a “proposed” value for xt+1.
    proposed<- sample_distDiscrete(n=1,scm[x_t,]) # check
    # formula to generate the acceptance_probability
    # this is a double between [0,1]
    # A=min(1,π(y)Q(xt|y)/π(xt)Q(y|xt)).
    acceptance_probability<- min(c(1, (dist[proposed]*scm[proposed,x_t])/(dist[x_t]*scm[x_t,proposed]) )) # check
    # random number to test if the proposed is accepted or not
    # with probability A “accept” the proposed value, and set xt+1=y. Otherwise set xt+1=xt
    acceptance_rn<- sample_distUniform(n=1) # check
    if ( acceptance_rn<=acceptance_probability ){
      result<- append(result,proposed)
    }else{
      result<- append(result,result[length(result)])
    }
    # set the last value of result to be the new x_t
    x_t<- result[length(result)]
  }

  result

}
