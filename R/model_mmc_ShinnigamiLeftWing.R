#' @title model_mmc_ShinnigamiLeftWing
#' @description calculate probabilities of multiple possibilities given the exogs factors.
#' @param endog a factor vector containig the endog (causes).
#' @param exogs a data.frame with the exogs data, this is the (given) all need to be factors.
#' @param levels a integer vector contaning values to the levels, the min(levels) need to be >=2.
#' @param tPlusX the number of leading to apply to endog, tPlusX 1 means "does actual exogs predicts the next endog?".
#' @param options.nThread the number of threads to run paralelly.
#' @param options.threadType the type of thread, on windoes this can be PSOCK but linux distros accepts FORK. see about
#' parallel package.
#' @param options.chunk a logical (TRUE/FALSE) if save the results into files instead return them, if TRUE the return
#' of this function will be a NULL.
#' @param options.chunkNIter the number of iteration of furthermore levels to save. for example: 100 means to save each
#' 100 iterations.
#' @param options.chunkDir takes a character, is a directory path to save the result of this function, subfolders with the
#' names: 'level1', 'level2' and 'level3' will be created, and inside them will be stored .fst files which contains
#' each a data.frame.
#' @param options.chunkName is a character with the first name for all files, the files are save with the standard
#' name: {options.chunkName}.part{nPart}.fst.
#' @return a list with data.frames
#' @section tip:
#' after returned the results can be used the function in the package dplyr: dplyr::bind_rows to get a unique data.frame.
#' @import dplyr
#' @import foreach
#' @export
model_mmc_ShinnigamiLeftWing<- function(endog, exogs, levels=1, tPlusX=1, options.nThread=parallel::detectCores(), options.threadType='PSOCK', options.chunk = FALSE, options.chunkNIter = 100, options.chunkDir = getwd(), options.chunkName='dist'){
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
    # will do the update of the progress bar.
    pg$update(n/.GlobalEnv$.total)
  }

  # formated to be passed to .options.snow
  .options.snow<- list('progress' = progressBarUpdate)

  # a variable to store the step to print
  step<- 0
  step.total<- length(levels)

  ########################################################################################################################
  #| calculate level1
  ########################################################################################################################

  # will run bellow only if level one is wanted
  if ( 1 %in% levels ){
    step<- step+1
    print( glue::glue("Step ({step}/{step.total}): calculating level1...") )
    pg<- progressBar(ncol(exogs))
    result[[ 'level1' ]]<- foreach::foreach( i=1:(ncol(exogs)), .packages = 'dplyr',.export = 'matrix_createMultivariateFromExogCom', .inorder = FALSE, .options.snow = .options.snow )%dopar%{
      com<- matrix_createMultivariateFromExogCom(endog, exogs[[i]], tPlusX = tPlusX)
      rownames(com)<- paste0(colnames(exogs)[[i]], '=', rownames(com))
      com
    }
    result[[ 'level1' ]]<- dplyr::bind_rows(result[[ 'level1' ]])
    # terminate the progress bar
    pg$terminate()


    # create the directory and save if options.chunk is TRUE
    if ( options.chunk == TRUE ){
      targetDir<- paste0(options.chunkDir, '/level1')
      dir.create(path = targetDir, showWarnings = FALSE)
      dist<- result[[ 'level1' ]]
      # rownames of data to the first column cause fst dont accept rownames
      dist<- cbind(data.frame('rownames'=rownames(dist)), dist)
      # write the file
      fst::write_fst(dist, path = paste0(targetDir, '/', options.chunkName, '.part1.fst'))
      rm(dist)
      invisible(gc())
    }

    # will return if is wanted only level 1
    if ( max(levels)==1 ){
      return(result)
    }

    # remove the index where value is 1
    levels<- levels[which(levels != 1)]

  }

  #/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
  ########################################################################################################################
  #| calculate furthermore levels
  ########################################################################################################################

  for ( i in 1:(length(levels)) ){
    step<- step+1
    print( glue::glue("Step ({step}/{step.total}): calculating level{levels[i]}...") )
    # a variable will contain the index of this level in the result list.
    levelIdx<- glue::glue("level{levels[i]}")
    # generate combinations, each combination is a column. rows are the columns names.
    combinations<- utils::combn(colnames(exogs), m=levels[i]) %>% as.data.frame()
    print( glue::glue("estimating {ncol(combinations)} combinations...") )
    # the level length parameter (combinations.max in the inner function)
    # initialize the progress bar
    # initialize the variable of the start of the main loop
    k.start<- 1
    # start the j iterator
    j<- 1
    if ( options.chunk == TRUE ){
      # the estimated number of chunks
      estimatedNChunks<- ceiling(ncol(combinations) / options.chunkNIter)
      # array with the endings
      chunksEnds<- rep( ceiling(ncol(combinations) / estimatedNChunks), estimatedNChunks ) %>% cumsum()
      # fix the last index
      chunksEnds[[ length(chunksEnds) ]]<- ncol(combinations)
      # the folder to save
      targetDir<- paste0(options.chunkDir, '/', levelIdx)
      # create the subfolder to target dir
      dir.create(path = targetDir, recursive = TRUE, showWarnings = FALSE)
    }
    while (TRUE){
      # set the value of k.end
      if ( options.chunk == TRUE ){
        k.end<- chunksEnds[[ j ]]
      }else{
        k.end<- ncol(combinations)
      }
      # print information about the chunk part
      if ( options.chunk == TRUE ){
        cat(paste0('running for part ',j,' of ', length(chunksEnds),':', '\n'))
      }
      # initialize the progress bar
      pg<- progressBar(length(k.start:k.end))
      dists<-
        foreach::foreach( k = k.start:k.end, .multicombine = TRUE, .inorder = FALSE, .options.snow=.options.snow )%dopar%{
          combination<- combinations[[ k ]]
          com<- matrix_createMultivariateMultipleFromExogsCom(endog, exogs[,combination], tPlusX = tPlusX)
          # will return a list of lists, where each indice of sublist is a character, this
          # is without the separator ' & '.
          comNames<- rownames(com) %>% stringr::str_split(.,' & ')
          # will does comNames be a vertical data.frame (nrow is bigger than ncol).
          comNames<- comNames %>% as.data.frame() %>% t() %>% as.data.frame()
          # bellow will apply the name of the column for each column of the comNames
          # to follow the standard: {colname}={state}
          for ( k in 1:(ncol(comNames)) ){
            comNames[[k]]<- paste0(combination[[ k ]], '=', comNames[[k]])
          }
          # bellow will 'concatenate' each row of the data.frame comNames, will return a vector with each value
          # of each row comcatenated as character separated (what was columns) using the separator "'space'&'space'".
          comNames<- do.call(paste,c(comNames, sep=' & '))
          rownames(com)<- comNames
          com
        }
      # transform dists (until here it is a list of data.frames) into a unique data.frame
      dists<- dplyr::bind_rows(dists)

      if ( options.chunk == TRUE ){
        # set the new value of k.start
        k.start<- k.end + 1
        # as fst dont accept rownames will set it as the first columnw with the name 'rownames'
        dists<- cbind(data.frame('rownames'= rownames(dists)), dists)
        # write the fst file
        fst::write_fst(dists, path = paste0(targetDir, '/', options.chunkName, '.part', j, '.fst'))
        # remove the dists
        rm(dists)
        # will restar the cluster to free memory cause removing dists dont free memory fron parallel processes
        parallel::stopCluster(cl)
        cl<- parallel::makeCluster(options.nThread, options.threadType, outfile=outfile)
        doSNOW::registerDoSNOW(cl)
        # call gc
        invisible(gc())
        # increase the value of j (will stop the loop if j is in the end)
        if ( j == length(chunksEnds) ){ break }else{ j<- j + 1 }
      }else{
        result[[ levelIdx ]]<- dists
        break
      }
    }
    # terminate the actual progress bar
    pg$terminate()
  }

  if ( options.chunk == TRUE ){
    return(NULL)
  }else{
    return(result)
  }
}