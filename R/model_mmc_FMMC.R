#' @title model_mmc_FMMC
#' @description implements the model FM-MC from their paper.
#' @param data a data.frame containing columns, each data.frame needs to have the sames factors levels.
#' @param k the index of the endog column.
#' @param r is the order or the model. if how much past data probabilities to consider.
#' @param n is the actual time in the data. (taken automaticaly).
#' @param m is the number of datas. (taken automaticaly).
#' @param s is the number of states. (taken automaticaly).
#' @param options takes a list with values for additional options, these options there'nt no correlation with the model
#' itself but are paramether for things used to implement, for example the n.threads.
#' @return return a numeric vector with the probabilities for X_n+1 (the next state). the names of the vector
#' are the names of the levels of factor.
#' @references
#' A Gradual Facilitate High‑Order Multivariate Markov Chains
#' Model with Application to the Changes of Exchange Rates
#' in Egypt: New Approach
#' A. M. Elshehawey
#' https://doi.org/10.1007/s42519-021-00179-y
#' @import foreach
#' @export
model_mmc_FMMC<- function(data,k,r,n=nrow(data),m=ncol(data),s=length(levels(data[[k]])),options=list( 'generations'=100,'n.threads'=min(c(parallel::detectCores(),r)) )){
  # check if the levels over all data are equals
  for ( i in 2:(ncol(data)) ){
    stopifnot("the levels of datas cant be different."= all( levels(data[[i]]) %in% levels(data[[i-1]]) ) )
  }

  stopifnot(1<=k)
  # constraint created by me avouid r be bigger than n.
  stopifnot(r <= n)

  cl<- parallel::makeCluster(spec = parallel::detectCores(),type = 'PSOCK')
  doSNOW::registerDoSNOW(cl)
  on.exit( parallel::stopCluster(cl) )

  # calculate states occurrency probability
  # folowing the example at pag.: 15. chapter: 5.1.
  # then improving to with with: 'n-i+1'.
  Xs<- list()
  for ( .colName in colnames(data) ){
    Xs[[.colName]]<-
      foreach::foreach ( i= 1:r ,.packages = c('dplyr'),.export = c('vector_occurrencyProbability','get'))%dopar%{
      selection<- n-i+1
      # was used the 'occurrencyProbability' which is a simple number of occurrences (nocurrence/lenght(data))
      # instead a conditional probability (which is most common in markov chains) cause this way was used
      # in the exemple section in the paper and in all others two papers (references [25], [26] of the paper.)
      # the second motive is this function is supose to be used in stochastic stationary timeseries, so the probabilities
      # are not constrained for a especific state, for example, the returns of a stock data, the return Xn can be 0.001
      # the probability of Xn+1 is not constrained, it can be -infinite to infinite states, so it can be any state.
      Xi<- vector_occurrencyProbability(data[1:(selection),.colName]) %>% t() %>% as.data.frame()
      attr(Xi,'iValue')<- selection
      Xi
    }
    # the loop bellow will transform the names of indexes of Xs[[.colName]] into characters
    # with the number of lag, this way i can acces these variables by a character.
    for ( i in 1:(length(Xs[[.colName]])) ){
      # will to use 1 fixed cause at last line i'm droping the first index, so as are indexes
      # the index [[2]] becomes [[1]] again, so i'll always to use [[1]]
      idx<- 1
      iValue<- attr(Xs[[.colName]][[ idx ]],'iValue')
      Xs[[.colName]][[as.character(iValue)]]<- Xs[[.colName]][[ idx ]]
      Xs[[.colName]]<- Xs[[.colName]][ -idx ]
    }
  }
  invisible(gc())

  # calculate the Ps for 'k' index
  Ps11<-
    foreach::foreach(i=1:r,.export = c('vector_createCom','matrix_transitionProbabilities'))%dopar%{
    com<- vector_createCom(data[[k]],tPlusX = i)
    scm<- matrix_transitionProbabilities(com)
    attr(scm,'iValue')<- i
    scm
  }
  # set the values of Ps11 to a list with the name of colnames(data)[[k]]
  Ps<- list()
  Ps[[ colnames(data)[[k]] ]]<- Ps11

  # stop the cluster cause it will not to be used then remove the actual on.exit
  parallel::stopCluster(cl)
  # a empty on.exit remove the existing, it is in their the documentation.
  on.exit()
  invisible(gc())

  # the first summatory of equation (2.3) in the paper.
  partOne<- list()
  for ( i in 1:r ){
    P<- Ps[[k]][[i]]
    X<- Xs[[k]][[as.character(n-i+1)]]
    # check if the i of P is realy the correct (cause can happen errors during parallelization)
    stopifnot(attributes(P)[['iValue']]==i)
    # the order of multiplication of two matrixes are X * P cause in the paper the author uses a
    # columnar matrix (columns to rows) and here i'm using a 'row to column' matrix, so in matrix
    # multiplication the order matter, and i tested the correct order for me is X then P ( X * P )
    # so or i invert the multiplication or invert where to apply the transpose. choose invert the multiplication.
    calc<- matrix_multiplication(X,P)
    partOne<- append(partOne,list(calc))
  }

  # the second and the third summatory in the equation (2.3) in the paper.
  # i'm using the equation without the identity matrix.
  partTwo<- list()
  for ( j in 1:m ){
    # dont allow j be equal to k
    if ( j==k ){next}
    for ( i in 1:r ){
      X<- Xs[[j]][[as.character(n-i+1)]]
      partTwo<- append(partTwo,list(X))
    }
  }

  partAll<- c(partOne,partTwo)

  partAll.matrix<- dplyr::bind_rows(partAll)

  # the function to optimize
  # bellow will calculate the distance from the X(k) as in the papaer: minimize||B*y ~ y||
  # the objective of this optimization is to minimize the difference between the new data and the X(k)n
  # to do this i need do add some contraints to the linear programming model,
  # to learn what i did read:
  # https://math.stackexchange.com/questions/1954992/linear-programming-minimizing-absolute-values-and-formulate-in-lp
  opt.func<- function(vec){
    # multiply every value of vec to rows of partAll.matrix
    result<- partAll.matrix * vec
    sums<- colSums(result)
    differences.1<- sums - Xs[[k]][[as.character(n)]]
    differences.2<- Xs[[k]][[as.character(n)]] - sums
    loss<- data.frame(matrix(nrow = 1,ncol = length(sums)))
    for ( i in 1:(ncol(loss)) ){
      loss[1,i]<- max(c(differences.1[[i]],differences.2[[i]]))
    }
    as.matrix(loss)
  }

  opt.const<- function(vec){
    .sum<- sum(vec)
    max(c(.sum - 1, 1 - .sum))
  }

  solution<- mco::nsga2(fn=opt.func,
             idim = s,
             odim = s,
             constraints = opt.const,
             cdim = 1,
             lower.bounds = rep(0,s),
             upper.bounds = rep(1,s),
             generations = options[['generations']]
  )

  # check if there'nt optmum
  if ( all(solution$pareto.optimal)==FALSE ){
    stop("HAD NO OPTIMUM SOLUTION, try to increasing the options$generations")
  }

  opt.bestResult<- solution$par[which.min(rowSums(solution$value)),]

  result<- colSums(partAll.matrix*opt.bestResult)

  if ( sum(result)>1 ){
    warning("the sum of result ( sum(result) ) is bigger than one, is better this value be lower or equal to 1, try to increase the options$generations number")
  }

  result
}
