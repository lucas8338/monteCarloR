#' @title model_mmc_FMMC
#' @description implements the model FM-MC from their paper.
#' @references
#' A Gradual Facilitate High‑Order Multivariate Markov Chains
#' Model with Application to the Changes of Exchange Rates
#' in Egypt: New Approach
#' A. M. Elshehawey
#' https://doi.org/10.1007/s42519-021-00179-y
#' @export

library(foreach)
cl<- parallel::makeCluster(spec = parallel::detectCores(),type = 'PSOCK')
doSNOW::registerDoSNOW(cl)

load("./data/cryptoWExog.rda")

data<- cryptoWExog[,c(1,8,10)]

data[[1]]<- libGetDataR::transf.diff(data[[1]])

data<- tidyr::drop_na(data)

data<- matrix_discretize(data,n=100)

# the index of the endog
k<- 1

# n is the actual time, in this case i'm using the index
n<- nrow(data)

# m is the number of datas
m<- ncol(data)
# check for:
# Assume that there are m ≥ 2. pag: 5.
stopifnot(ncol(data)>=2)

# the amount of order
r<- 8

# calculate states occurrency probability
# folowing the example at pag.: 15. chapter: 5.1.
Xs<- list()
for ( .colName in colnames(data) ){
  Xs[[.colName]]<- vector_occurrencyProbability(data[[.colName]])
}

# calculate the Ps for 'k' index
Ps11<- foreach::foreach(i=1:r)%dopar%{
  com<- vector_createCom(data[[k]],tPlusX = i)
  scm<- matrix_transitionProbabilities(com)
  attributes(scm)[['iValue']]<- i
  scm
}
Ps<- list()
Ps[[colnames(data)[[k]]]]<- Ps11


partOne<- list()
for ( i in 1:r ){
  # my doubt here is about the multiplication, caus P is a matrix, and X is a unique value,
  # i dont know if this is correct i believe yes, cause the page: 16 there an example.
  P<- Ps[[k]][[i]]
  X<- Xs[[k]][[as.character(data[n-i+1,k])]]
  # check if the i of P is realy the correct (cause can happen errors during parallelization)
  stopifnot(attributes(P)[['iValue']]==i)
  calc<- P * X
  partOne<- append(partOne,list(calc))
}


partTwo<- list()
for ( j in 1:m ){
  if ( j==k ){next}
  for ( i in 1:r ){
    X<- Xs[[j]][[as.character(data[n-i+1,j])]]
    partTwo<- append(partTwo,list(X))
  }
}

Ck<- c(partOne,partTwo)