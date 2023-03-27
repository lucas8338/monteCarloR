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
#' @param options.chunkContinue bollean whether to continue (resume) to fitting, will load the files: 'combinations.rds',
#' 'chunksEnds.rds' and 'lastCompletedChunk.rds'.
#' @return a list with data.frames
#' @section tip:
#' after returned the results can be used the function in the package dplyr: dplyr::bind_rows to get a unique data.frame.
#' @import dplyr
#' @import foreach
#' @export
model_mmc_ShinnigamiLeftWing<- function(endog, exogs, levels=1, tPlusX=1, options.nThread=parallel::detectCores(), options.threadType='PSOCK', options.chunk = FALSE, options.chunkNIter = 100, options.chunkDir = getwd(), options.chunkName='dist', options.chunkContinue=TRUE){
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

  # a function to generate the combinations
  generateCombinations<- function(){
    # generate combinations, each combination is a column. rows are the columns names.
    combs<- utils::combn(colnames(exogs), m=levels[i]) %>% as.data.frame()
    # cause the functions utils::combn takes the data as the order them are, this cause
    # that very large number of combinations, the combinations at last position can never be
    # processed or take a long time to process them cause the firsts columns will be used
    # first.
    combs<- combs[, sample(seq_len(ncol(combs))), drop=FALSE]
    combs
  }
  
  # this function will to deterine the endings indexes of the 'combinations' data.frame to use to saving (chunking)
  # this will also return the the chunks ends, and to create the folder to save these endings
  createChunksEnds<- function(){
    # the estimated number of chunks
    estimatedNChunks<- ceiling(ncol(combinations) / options.chunkNIter)
    # array with the endings
    chunksEnds<- rep( ceiling(ncol(combinations) / estimatedNChunks), estimatedNChunks ) %>% cumsum()
    # fix the last index
    chunksEnds[[ length(chunksEnds) ]]<- ncol(combinations)
    # to return
    chunksEnds
  }

  for ( i in seq_along(levels) ){
    step<- step+1
    cat( paste0('Step ', '(', step, '/', step.total, '): ', 'calculating level', levels[[i]], '...', '\n') )
    # a variable will contain the index of this level in the result list.
    levelIdx<- glue::glue("level{levels[i]}")

    # the SUPOSE path to save the chunk files
    targetDir<- paste0(options.chunkDir, '/', levelIdx)
    # this file contains the created combinations
    path.combinations<- paste0(targetDir, '/combinations.rds')
    # this file contains the chunksEnds generated (the split)
    path.chunksEnds<- paste0(targetDir, '/chunksEnds.rds')
    # this file contains the index of last completed chunk 'j'
    path.lastCompletedChunk<- paste0(targetDir, '/lastCompletedChunk.rds')

    # the explication of some variables in this if-else statement:
    # combinations: are the combinations of exogs indicators to use to fitting.
    # chunksEnds: are the ends for each chunk (when to save).
    # 'j': is the number of actual part of chunk, so after each chunk be fitted j will
    # be increased.
    # 'k.start': is the number of the combination which will be used to start the fitting
    # the model will fit combinations[k.start:k.end]. so at the end of fitting this will be
    # 'k.start<- k.end + 1' cause as we saw above j will be increased by one. and we will have
    # the correct range from start to the end combination siliding together with the fitting
    # parts.
    if ( options.chunk == TRUE ){
      # create the targetDir a dir to save the chunks
      dir.create(targetDir, showWarnings = FALSE)
      # will try to continue
      if ( options.chunkContinue == TRUE && all(file.exists(c(path.combinations, path.chunksEnds, path.lastCompletedChunk))) == TRUE ){
        combinations<- readRDS(path.combinations)
        chunksEnds<- readRDS(path.chunksEnds)
        lastCompletedChunk<- readRDS(path.lastCompletedChunk)
        if ( lastCompletedChunk == length(chunksEnds) ){
          cat( paste0(levelIdx, ' was completed. moving to the next level...', '\n') )
          next
        }
        j<- lastCompletedChunk + 1
        k.start<- chunksEnds[[ j - 1 ]] + 1
        cat('files loaded. continuing fitting...\n')
      }else{
        cat('fitting from start...')
        combinations<- generateCombinations()
        saveRDS(combinations, file = path.combinations)
        chunksEnds<- createChunksEnds()
        saveRDS(chunksEnds, file = path.chunksEnds)
        j<- 1
        k.start<- 1
      }
    }else{
      combinations<- generateCombinations()
      chunksEnds<- createChunksEnds()
      j<- 1
      k.start<- 1
    }

    cat( paste0('total number of combinations: ', ncol(combinations), '\n') )
    cat( paste0('starting fitting from combination: ', k.start, '\n') )

    repeat{
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
        lastCompletedChunk<- j
        saveRDS(lastCompletedChunk, file = path.lastCompletedChunk, compress = FALSE)
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