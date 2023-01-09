#' @title model_mmc_ShinnigamiLeftWing
#' @description calculate probabilities of multiple possibilities given the exogs factors.
#' @param endog a factor vector containig the endog (causes).
#' @param exogs a data.frame with the exogs data, this is the (given).
#' @param levels a integer vector contaning values to the levels, the min(levels) need to be >=2.
#' @param tPlusX the number of leading to apply to endog, tPlusX 1 means "does actual exogs predicts the next endog?".
#' @param levels.length the maximum of combinations to obtain at each level. Inf if all combinations are wanted.
#' @param options.nThread the number of threads to be used by parallelization.
#' @param options.nInnerThread the number of threads of the internal function. (i dont see any motive to use more than 1).
#' @param options.threadType the type of thread, on windoes this can be PSOCK but linux distros accepts FORK. see about
#' parallel package.
#' @return a list with data.frames
#' @section tip:
#' after returned the results can be used the function in the package dplyr: dplyr::bind_rows to get a unique data.frame.
#' @import dplyr
#' @import foreach
#' @export
model_mmc_ShinnigamiLeftWing<- function(endog, exogs, levels, tPlusX=1, levels.length=rep(Inf, length(levels)), options.nThread=parallel::detectCores(), options.nInnerThread=1, options.threadType=ifelse(Sys.info()['sysname']=='Windows', 'PSOCK', 'FORK')){
  stopifnot("level1 is fast to be calculated and is basic, so this will be calculated anyway, so levels need to be bigger than 2" = min(levels)>1)

  # set the outfile location, will be the temp folder.
  outfile<- stringr::str_c(Sys.getenv('TEMP'),'/ShinnigamiLeftWing_outfile.txt')
  cl<- parallel::makeCluster(options.nThread, options.threadType, outfile=outfile)
  doSNOW::registerDoSNOW(cl)
  on.exit(parallel::stopCluster(cl))

  # will store the results for each level, it will be returned.
  result<- list()

  # format for the progress bar
  format<- "progress: :percent (:current/:total) at :tick_rate/s | eta: :eta | elapsed: :elapsedfull"

  # function to initialize the progress bar
  progressBar<- function(total){
    .GlobalEnv$.total<- total
    pg<- progress::progress_bar$new(clear = FALSE,
                               total = total,
                               format = format,
                               show_after = 0
    )
    # to show the progress bar immediately
    pg$tick(0)
    pg
  }

  # function to be passed to .options.snow
  progressBarUpdate<- function(n){
    # will do the update of the progress bar, the: 'total-total/options.nThread' is because the 100%
    # will not be 100, but need to be taken the number of threads in count.
    pg$update(n/.GlobalEnv$.total)
  }

  # formated to be passed to .options.snow
  .options.snow<- list('progress' = progressBarUpdate)

  ########################################################################################################################
  #| calculate level1
  ########################################################################################################################

  print( glue::glue("Step (1/{length(levels)+1}): calculating level1...") )
  pg<- progressBar(ncol(exogs))
  result[[ 'level1' ]]<- foreach::foreach( i=1:(ncol(exogs)), .packages = 'dplyr',.export = 'matrix_createMultivariateFromExogCom', .inorder = FALSE, .options.snow = .options.snow )%dopar%{
    com<- matrix_createMultivariateFromExogCom(endog, exogs[[i]], tPlusX = tPlusX)
    rownames(com)<- paste0(glue::glue("{colnames(exogs)[[i]]}="), rownames(com))
    com
  }
  result[[ 'level1' ]]<- dplyr::bind_rows(result[[ 'level1' ]])
  # terminate the progress bar
  pg$terminate()

  #/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
  ########################################################################################################################
  #| calculate furthermore levels
  ########################################################################################################################

  for ( i in 1:(length(levels)) ){
    print( glue::glue("Step ({i+1}/{length(levels)+1}): calculating level{levels[i]}...") )
    # a variable will contain the index of this level in the result list.
    levelIdx<- glue::glue("level{levels[i]}")
    # generate combinations, each combination is a column. rows are the columns names.
    combinations<- utils::combn(colnames(exogs), m=levels[i]) %>% as.data.frame()
    print( glue::glue("estimating {ncol(combinations)} combinations...") )
    # the level length parameter (combinations.max in the inner function)
    ll<- (levels.length[i] / ncol(combinations)) %>% round(.,digits = 0)
    # initialize the progress bar
    pg<- progressBar(ncol(combinations))
    # the main loop of this function.
    result[[ levelIdx ]]<-
      foreach::foreach( j=1:(ncol(combinations)), .packages = c('dplyr','foreach'), .export = 'matrix_createMultivariateMultipleFromExogsCom', .inorder = FALSE, .options.snow=.options.snow )%dopar%{
        combination<- combinations[[ j ]]
        com<- matrix_createMultivariateMultipleFromExogsCom(endog, exogs[,combination], tPlusX = tPlusX, combinations.max = ll, options.nThread = options.nInnerThread, options.threadType = options.threadType)
        # will return a list of lists, where each indice of sublist is a character, this
        # is without the separator ' & '.
        comNames<- rownames(com) %>% stringr::str_split(.,' & ')
        # will does comNames be a vertical data.frame (nrow is bigger than ncol).
        comNames<- comNames %>% as.data.frame() %>% t() %>% as.data.frame()
        # bellow will apply the name of the column for each column of the comNames
        # to follow the standard: {colname}={state}
        for ( k in 1:(ncol(comNames)) ){
          # the variabele 'combination' is a vector with the name of each column of this combination
          # i'll acess it using a loop (j) and will use this to format tha names of columns
          # in comNames.
          idx<- glue::glue("{combination[[ k ]]}=")
          comNames[,k]<- paste0(idx, comNames[,k])
        }
        # bellow will 'concatenate' each row of the data.frame comNames, will return a vector with each value
        # of each row comcatenated as character separated (what was columns) using the separator "'space'&'space'".
        comNames<- do.call(paste,c(comNames, sep=' & '))
        rownames(com)<- comNames
        com
      }
    result[[ levelIdx ]]<- dplyr::bind_rows(result[[ levelIdx ]])
    # terminate the actual progress bar
    pg$terminate()
  }

  result
}