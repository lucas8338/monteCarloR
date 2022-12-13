#' @title model_mmc_ching2008
#' @description a multivariate markov chains model from chin et all 2008.
#' @param n is the order.
#' @param norm.type is the type of norm, it can be any type in the base::norm, from the paper ther recommendations is:
#' '1': (norm-1 (taxicab norm))always give the most robust answer.
#' 'I': (norm-infinity) avoids gross discrepancies with the data.
#' '2': (norm-euclidian) if the errors are known to be normally distributed then is the best choice.
#' @param s is the number of sequences.
#' @param j is a iterator (vector) for the number of sequences (s).
#' @param r is the previous time in the data. (t-1).
#' @param m is the number of levels.
#' @references
#' Higher-order multivariate Markov chains and their applications
#' Wai-Ki Ching, Michael K. Ng, Eric S. Fung
#' doi: 10.1016/j.laa.2007.05.021
#' @import dplyr
#' @export
model_mmc_ching2008<- function(data,n,norm.type ='1',s = ncol(data),j=1:s,r = nrow(data)-1,m = length(levels(data[[1]])),options.mco.popsize = s*4,options.mco.generations = 100){
  # verifies if all levels in all sequence are equals
  for ( i in 2:ncol(data) ){
    stopifnot("levels cant be different"= all(levels(data[[i]])==levels(data[[i-1]])) )
  }

  #/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
  ######################################################################################################################
  #| estimate Pjkhs: pags.: 501-502.
  ######################################################################################################################

  Pjkhs<- list()
  for ( .j in j ){
    for ( k in 1:s ){
      for ( h in 1:n ){
        idx<- glue::glue("j={.j},k={k},h={h}")
        Pjkhs[[ idx ]]<- matrix_createMultivariateFromExogCom(data[[.j]], data[[k]], h) %>% matrix_transitionProbabilities()
      }
    }
  }
  # verified in debugging

  #/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
  ######################################################################################################################
  #| estimate Xkhs: pag. 496.
  ######################################################################################################################

  Xkhs<- list()
  for ( k in 1:s ){
    for ( h in 1:n ){
      idx<- glue::glue("k={k},h={h}")
      X<- rep(0,m)
      names(X)<- levels(data[[k]])
      X[[ as.character(data[r-h+1,k]) ]]<- 1
      Xkhs[[ idx ]]<- X
    }
  }
  # verified in debugging

  #/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
  ######################################################################################################################
  #| estimates Xjs: pag. 500, 502.
  ######################################################################################################################

  Xjs<- list()
  for ( .j in j ){
    idx<- glue::glue("j={.j}")
    Xjs[[ idx ]]<- vector_occurrencyProbability(data[[.j]])
  }
  # remember vectors are seen as vertical vectors by default.
  # this is the motive in multiplication of a vector by a data.frame the multiplication is done
  # for each row, cause the vector is seen as vertical vector.

  # verified in debugging

  #/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
  ######################################################################################################################
  #| function to do the optimization, as formula (10) in the paper. for each j.
  ######################################################################################################################

  func10InPaper<- function(vec){
    stopifnot(length(vec)==(s^2)*n)
    results<- list()
    for ( .j in j ){
      ksum<- list()
      for ( k in 1:s ){
        # values for h summatory
        hsum<- list()
        for ( h in 1:n ){
          ##############################################################################################################
          #| declare variables
          ##############################################################################################################
          # will use the first value of vec then remove that value, this way that value will never to be multiplied
          # again.
          lambda<- vec[[1]]
          vec<- vec[2:length(vec)]

          P<- Pjkhs[[ glue::glue("j={.j},k={k},h={h}") ]]
          Xk<- Xjs[[ glue::glue("j={k}") ]]
          Xj<- Xjs[[ glue::glue("j={.j}") ]]

          #/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
          ##############################################################################################################
          #| do calculation
          ##############################################################################################################

          calc<- lambda * P
          calc<- matrix_multiplication(calc,Xk)
          calc<- calc - Xj
          hsum[[ glue::glue("j={.j},k={k},h={h}") ]]<- calc
        }
        ksum[[ glue::glue("j={.j},k={k}") ]]<- Reduce("+",hsum)
      }
      results[[ glue::glue("j={.j}") ]]<- Reduce("+",ksum)
    }
    # applies the norm.
    results<- lapply(results,function(d){norm(as.matrix(d),norm.type)})
    results
  }


  #/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
  ######################################################################################################################
  #| find lambdas
  ######################################################################################################################

  opt.func<- function(vec){
    results<- func10InPaper(vec)
    losses<- as.numeric(results) %>% as.matrix()
    losses
  }

  # the function bellow is a constrant for package mco to make sure
  # the sum of all params in each j is equal to one as in the formula (10) in the paper.
  opt.const<- function(vec){
    losses<- list()
    for ( .j in j ){
      startIdx<- s*n*(.j-1)+1
      endIdx<- s*n*.j
      losses[[.j]]<- -abs(sum(vec[ startIdx:endIdx ]) - 1)
    }
    losses<- as.numeric(losses) %>% as.matrix()
    as.matrix(losses)
  }

  opt.solution<- mco::nsga2(fn= opt.func,
             idim = length(Pjkhs),
             odim = s,
             constraints = opt.const,
             cdim = s,
             lower.bounds = rep(0,length(Pjkhs)),
             upper.bounds = rep(s*n+1e-2,length(Pjkhs)),
                            popsize = options.mco.popsize,
                            generations = options.mco.generations
  )

  opt.bestIdx<- which.min(rowSums(opt.solution$value))

  opt.bestParams<- opt.solution$par[opt.bestIdx,]

  lambdas<- opt.bestParams

  #/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
  ######################################################################################################################
  #| verifies if lambda is valid due constraints in formula (10) in the paper.
  ######################################################################################################################
  tolerance<- 0.01
  losses<- list()
    for ( .j in j ){
      startIdx<- s*n*(.j-1)+1
      endIdx<- s*n*.j
      losses[[.j]]<- -abs(sum(lambdas[ startIdx:endIdx ]) - 1)
    }
  losses<- as.numeric(losses)
  stopifnot("sum of lambdas of all constraints of each j need to be equal to one"=all(abs(losses)-1<=tolerance))

  #/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
  ######################################################################################################################
  #| the main formula (6).
  ######################################################################################################################

  func6InPaper<- function(vec){
    stopifnot(length(vec)==(s^2)*n)
    results<- list()
    for ( .j in j ){
      ksum<- list()
      for ( k in 1:s ){
        # values for h summatory
        hsum<- list()
        for ( h in 1:n ){
          ##############################################################################################################
          #| declare variables
          ##############################################################################################################
          # will use the first value of vec then remove that value, this way that value will never to be multiplied
          # again.
          lambda<- vec[[1]]
          vec<- vec[2:length(vec)]

          P<- Pjkhs[[ glue::glue("j={.j},k={k},h={h}") ]]
          Xkr<- Xkhs[[ glue::glue("k={k},h={h}") ]]

          #/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
          ##############################################################################################################
          #| do calculation
          ##############################################################################################################

          calc<- lambda * P
          calc<- matrix_multiplication(calc,Xkr)
          hsum[[ glue::glue("j={.j},k={k},h={h}") ]]<- calc
        }
        ksum[[ glue::glue("j={.j},k={k}") ]]<- Reduce("+",hsum)
      }
      results[[ glue::glue("j={.j}") ]]<- Reduce("+",ksum)
    }
    results
  }

  #/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
  ######################################################################################################################
  #| run the main formula then return the probabilities for each column of data data.frame.
  ######################################################################################################################

  results<- func6InPaper(lambdas)

  results
}