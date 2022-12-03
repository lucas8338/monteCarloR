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

data<- cryptoWExog[,c(1,2:10)]

data[[1]]<- libGetDataR::transf.diff(data[[1]])

data<- tidyr::drop_na(data)

data<- matrix_discretize(data,n=100)

# data to test, providen by the paper
data<- data.frame('S1'=as.factor(c(3,4,2,2,1,2,3,4,2,2,3,4)),'S2'=as.factor(c(3,1,1,1,1,3,2,2,4,1,2,2)))
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
r<- 2

# calculate states occurrency probability
# folowing the example at pag.: 15. chapter: 5.1.
Xs<- list()
for ( .colName in colnames(data) ){
  occurrencyProbability<- vector_occurrencyProbability(data[[.colName]]) %>% as.data.frame()
  occurrencyProbabilityTransposed<- t(occurrencyProbability) %>% as.data.frame()
  Xs[[.colName]]<- occurrencyProbabilityTransposed
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

s<- ncol(partAll[[1]])

.levels<- c()
for ( i in 1:(length(partAll)) ){
  .level<- rep(as.character(i),s)
  .levels<- append(.levels,.level)
}

# CkF means: CkFlatten
CkF<- dplyr::bind_cols(partAll) %>% as.numeric()
names(CkF)<- .levels

# distances
# bellow will calculate the distance from the X(k) as in the papaer: minimize||B*y ~ y||
# the objective of this is to minimize the difference between the new data and the X(k)n
# to do this i need do add some contraints to the linear programming model,
# to learn what i did read:
# https://math.stackexchange.com/questions/1954992/linear-programming-minimizing-absolute-values-and-formulate-in-lp
partAll.matrix<- dplyr::bind_rows(partAll)
differences<- rowSums(partAll.matrix) - Xs[[k]]

lp.direction<- 'max'
lp.coefs<- CkF
# initialize lp.const which will receiver the constraint matrix
lp.const<- data.frame()
# initialize lp.constDir
lp.constDir<- c()
# initialize lp.constValues which will take the values of the constraints
lp.constValues<- c()

# generate a const matrix to bind values of coefs together
variables_equalizer.const<- lp_generateConstraintsSameValues(CkF,as.factor(names(CkF)))
variables_equalizer.condtDir<- rep("==",nrow(variables_equalizer.const))
variables_equalizer.constValues<- rep(0,nrow(variables_equalizer.const))

lp.const<- rbind(lp.const,variables_equalizer.const)
lp.constDir<- append(lp.constDir,variables_equalizer.condtDir)
lp.constValues<- append(lp.constValues,variables_equalizer.constValues)

# add a const matrix which will take the first element ef each level and set to one
levelsCol.const<- data.frame(matrix(0,nrow = s,ncol = length(CkF)))
for ( i in 1:s ){
  for ( .level in unique(names(CkF)) ){
    levelsCol.const[i, grep(.level,names(CkF))[[i]] ]<- 1
  }
}

levelsCol.constDir<- rep(">=",nrow(levelsCol.const))
levelsCol.constValues<- differences

lp.const<- rbind(lp.const,levelsCol.const)
lp.constDir<- append(lp.constDir,levelsCol.constDir)
lp.constValues<- append(lp.constValues,levelsCol.constValues)

paramsSumEqualOne.const<- data.frame(matrix(1/s,nrow = 1,ncol = length(CkF)))
paramsSumEqualOne.constDir<- "=="
paramsSumEqualOne.constValues<- 1

lp.const<- rbind(lp.const,paramsSumEqualOne.const)
lp.constDir<- append(lp.constDir,paramsSumEqualOne.constDir)
lp.constValues<- append(lp.constValues,paramsSumEqualOne.constValues)

solution<- lpSolve::lp(direction = lp.direction,
                       objective.in = lp.coefs,
                       const.mat = as.matrix(lp.const),
                       const.dir = lp.constDir,
                       const.rhs = lp.constValues
)

