#' @title model_mmc_FMMC
#' @description implements the model FM-MC from their paper.
#' @references
#' A Gradual Facilitate High‑Order Multivariate Markov Chains
#' Model with Application to the Changes of Exchange Rates
#' in Egypt: New Approach
#' A. M. Elshehawey
#' https://doi.org/10.1007/s42519-021-00179-y
#' @import foreach
#' @export
model_mmc_FMMC<- function(data,k,r,n=nrow(data),m=ncol(data),s=length(levels(data[[k]])),options=list('generations'=100)){
  cl<- parallel::makeCluster(spec = parallel::detectCores(),type = 'PSOCK')
  doSNOW::registerDoSNOW(cl)

  # calculate states occurrency probability
  # folowing the example at pag.: 15. chapter: 5.1.
  Xs<- list()
  for ( .colName in colnames(data) ){
    occurrencyProbability<- vector_occurrencyProbability(data[[.colName]]) %>% as.data.frame()
    occurrencyProbabilityTransposed<- t(occurrencyProbability) %>% as.data.frame()
    Xs[[.colName]]<- occurrencyProbabilityTransposed
  }

  # calculate the Ps for 'k' index
  Ps11<- foreach::foreach(i=1:r,.export = c('vector_createCom','matrix_transitionProbabilities'))%dopar%{
    com<- vector_createCom(data[[k]],tPlusX = i)
    scm<- matrix_transitionProbabilities(com)
    attributes(scm)[['iValue']]<- i
    scm
  }
  Ps<- list()
  Ps[[colnames(data)[[k]]]]<- Ps11


  partOne<- list()
  for ( i in 1:r ){
    P<- Ps[[k]][[i]]
    X<- Xs[[k]]
    # check if the i of P is realy the correct (cause can happen errors during parallelization)
    stopifnot(attributes(P)[['iValue']]==i)
    # the order of multiplication of two matrixes are X * P cause in the paper the author uses a
    # columnar matrix (columns to rows) and here i'm using a 'row to column' matrix, so in matrix
    # multiplication the order matter, and i tested the correct order for me is X then P ( X * P )
    # so or i invert the multiplication or invert where to apply the transpose. choose invert the multiplication.
    calc<- matrix_multiplication(X,P)
    partOne<- append(partOne,list(calc))
  }

  partTwo<- list()
  for ( j in 1:m ){
    # dont allow j be equal to k
    if ( j==k ){next}
    for ( i in 1:r ){
      X<- Xs[[j]]
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
    differences.1<- sums - Xs[[k]]
    differences.2<- Xs[[k]] - sums
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

  # check the therent optmum

  opt.bestResult<- solution$par[which.min(rowSums(solution$value)),]

  result<- colSums(partAll.matrix*opt.bestResult)

  if ( sum(result)>1 ){
    warning("the sum of result ( sum(result) ) is bigger than one, is better this value be lower or equal to 1, try to increase the options$generations number")
  }

  result
}
