#' @title model_mmc_FMMC
#' @description implements the model FM-MC from their paper. this model is indtended to work only for
#' sequences are positively correlated.
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
model_mmc_FMMC<- function(data,k,r,n=nrow(data),m=ncol(data),s=length(levels(data[[k]])),options=list( 'popsize'=4*s, 'generations'=100 )){
  # check if the levels over all data are equals
  for ( i in 2:(ncol(data)) ){
    stopifnot("the levels of datas cant be different."= all( levels(data[[i]]) %in% levels(data[[i-1]]) ) )
  }

  stopifnot(1<=k)
  # constraint created by me avouid r be bigger than n.
  stopifnot(r <= n)

  # calculate states occurrency probability
  # folowing the example at pag.: 15. chapter: 5.1.
  # then improving to with with: 'n-i+1'.
  Xs<- list()
  for ( .colName in colnames(data) ){
    Xs[[.colName]]<- list()
    for ( i in 1:r ){
      selection<- n-i+1
      # was used the 'occurrencyProbability' which is a simple number of occurrences (nocurrence/lenght(data))
      # instead a conditional probability (which is most common in markov chains) cause this way was used
      # in the exemple section in the paper and in all others two papers (references [25], [26] of the paper.)
      # the second motive is this function is supose to be used in stochastic stationary timeseries, so the probabilities
      # are not constrained for a especific state, for example, the returns of a stock data, the return Xn can be 0.001
      # the probability of Xn+1 is not constrained, it can be -infinite to infinite states, so it can be any state.
      Xs[[.colName]][[as.character(selection)]]<- vector_occurrencyProbability(data[1:(selection),.colName]) %>% t() %>% as.data.frame()
    }
  }

  # calculate the Ps for 'k' index
  Ps<- list()
  for ( i in 1:r ){
    com<- vector_createCom(data[[k]],tPlusX = i)
    scm<- matrix_transitionProbabilities(com)
    Ps[[ colnames(data)[[k]] ]][[ as.character(i) ]]<- scm
  }

  # the first summatory of equation (2.3) in the paper.
  partOne<- list()
  for ( i in 1:r ){
    P<- Ps[[ colnames(data)[[k]] ]][[ as.character(i) ]]
    X<- Xs[[k]][[ as.character(n-i+1) ]]
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
      X<- Xs[[j]][[ as.character(n-i+1) ]]
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
    # will create a vector with size of nrow(partAll.matrix)
    # to do the multiplication. as in the paper only the partOne must
    # be multiplied by the params, so i'll multiplier the partTwo by 1.
    params<- c(vec,rep(1,length(partTwo)))
    # multiply every value of vec to rows of partAll.matrix
    result<- partAll.matrix * vec
    sums<- colSums(result)
    differences<- abs(sums - Xs[[k]][[as.character(n)]])
    as.matrix(differences)
  }

  opt.const<- function(vec){
    # i'll use negative abs cause in the mco::nsga2 uses for the constraint function
    # the objective is to get a values equal o bigger than 0 ( it was accepted by the constraint ).
    # so the objective of the constraint in mco::nsga2 is to get the loss of this function
    # at least equal to zero, so for example: if you want x >= 5 you can just to use: a function
    # which returns x - 5.
    -abs(sum(vec)-1)
  }

  solution<- mco::nsga2(fn=opt.func,
             idim = length(partOne),
             odim = s,
             constraints = opt.const,
             cdim = 1,
             lower.bounds = rep(0,length(partOne)),
             upper.bounds = rep(1,length(partOne)),
             popsize = options$popsize,
             generations = options$generations
  )

  opt.bestResult<- solution$par[which.min(rowSums(solution$value)),]

  # check if the sum of parameter are equal to one
  maximumAcceptableDiff<- 0.01
  stopifnot("the sum of all parameters cant be different than one ( with a acceptable margin. )."=abs(sum(opt.bestResult)-1)<=maximumAcceptableDiff)

  result<- colSums(partAll.matrix*opt.bestResult)

  result
}
