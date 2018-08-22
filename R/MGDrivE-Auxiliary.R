###############################################################################
#       __  _____________       _       ______
#      /  |/  / ____/ __ \_____(_)   __/ ____/
#     / /|_/ / / __/ / / / ___/ / | / / __/
#    / /  / / /_/ / /_/ / /  / /| |/ / /___
#   /_/  /_/\____/_____/_/  /_/ |___/_____/
#
#   R wrappers for C++ analysis
#   Marshall Lab
#   August 2018
#
###############################################################################

#' Split and Aggregate MGDrivE Output
#' 
#' Splits all runs by patch and aggregates females by mate. \cr
#' File structure needs to be as shown below: \cr
#'  * readDirectory
#'    * repetition 1
#'      * patch 1
#'      * patch 2
#'      * patch 3
#'    * repetition 2
#'      * patch 1
#'      * patch 2
#'      * patch 3
#'    * repetition 3
#'    * repetition 4
#'    * ...
#' 
#' @usage Split_Aggregate(readDir, writeDir, remFiles)
#' 
#' @param readDir Directory of subfolders with each run inside
#' @param writeDir Directory with same structure as readDir. Default is readDir
#' @param remFiles Boolean, remove read files. Default is FALSE
#'
#' @export
Split_Aggregate_MGDrivE <- function(readDir, writeDir=readDir, simTime, numPatch,
                            genotypes, remFiles=FALSE){
  
  # list all files to be worked on and where to write
  femaleFiles <- list.files(path = readDir, pattern = "AF1", full.names = TRUE, recursive = TRUE)
  maleFiles <- list.files(path = readDir, pattern = "ADM", full.names = TRUE, recursive = TRUE)
  writeFiles <- list.dirs(path = writeDir, full.names = TRUE, recursive = FALSE)
  
  if(length(writeFiles) != length(femaleFiles) || length(writeFiles) != length(maleFiles)){
    stop("check read/write directories for the correct files and folder structure")
  }
  
  # do the actual work
  fwritetest::S_A(outputFiles = writeFiles, maleFiles = maleFiles,
                              femaleFiles = femaleFiles, simTime = simTime,
                              numPatch = numPatch, genotypes = genotypes)
  
  # remove files
  if(remFiles){
    # unlink(x = list.dirs(path = readDir, full.names = TRUE, recursive = FALSE),
    #        recursive = TRUE, force = TRUE)
    invisible(file.remove(femaleFiles, maleFiles))
  }
  
}


#' Summary Statistics for Stochastic MGDrivE
#'
#' This function reads in all repetitions for each patch and calculates either
#' the mean, quantiles, or both. User chooses the quantiles, up to 4 decimal places,
#' and enters them as a vector. (order does not matter)  \cr
#'
#'
#' Given the readDirectory, this function assumes the follow file structure: \cr
#'  * readDirectory
#'    * repetition 1
#'      * patch 1
#'      * patch 2
#'      * patch 3
#'    * repetition 2
#'      * patch 1
#'      * patch 2
#'      * patch 3
#'    * repetition 3
#'    * repetition 4
#'    * ...
#'
#' @param readDirectory Directory to find repetition folders in
#' @param writeDirectory Directory to write output
#' @param doMean Boolean, calculate mean or not. Default is TRUE
#' @param quantiles Vector of quantiles to calculate. Default is NULL
#'
#' @return Writes output to files in writeDirectory
#' @export
AnalyzeQuantiles <- function(readDirectory, writeDirectory, doMean=TRUE, quantiles=NULL,
                             simTime, numPatch, genotypes, remFiles=FALSE){
  
  #safety check
  if(!doMean && is.null(quantiles)){
    stop("User needs to specify the mean or which quantiles to calculate. ")
  }
  
  # tilde expansion for Cpp writing
  writeDirectory <- path.expand(writeDirectory)
  
  # doing quantiles?
  doQuant = !is.null(quantiles)
  
  if(!doQuant){
    quantiles = 0
  }
  
  #get directories
  repFiles = list.dirs(path = readDirectory, full.names = TRUE, recursive = FALSE)
  
  #get male/female files as vector of vectors of strings
  maleFiles <- lapply(X = repFiles, FUN = list.files, pattern = "ADM",
                      full.names = TRUE)
  
  femaleFiles <- lapply(X = repFiles, FUN = list.files, pattern = "AF1",
                        full.names = TRUE)
  
  
  
  
  
  
  
  
  
  
  
  # #generate a list of all patches to run over
  # patchList = unique(regmatches(x = patchFiles[[1]],
  #                               m = regexpr(pattern = "Patch[0-9]+",
  #                                           text = patchFiles[[1]],
  #                                           perl = TRUE)))
  # 
  # #read in a file initially to get variables and setup return array
  # testFile <- data.table::fread(input = file.path(repFiles[1], patchFiles[[1]][1]),
  #                               verbose = FALSE, showProgress = FALSE, drop = c("Time", "Patch"))
  # 
  # 
  # #bunch of constants that get used several times
  # numReps <- length(repFiles)
  # numRow <- dim(testFile)[1]
  # numCol <- dim(testFile)[2]
  # columnNames <- c("Time", names(testFile))
  # 
  # #setup input data holder
  # popDataMale <- array(data = 0, dim = c(numRow,  numReps, numCol))
  # popDataFemale <- array(data = 0, dim = c(numRow, numReps, numCol))
  # 
  # #setup vectors and constants for use in loop
  # vecLen <- length(c(TRUE, quantiles))
  # maleFileVec <- femaleFileVec <- character(length = vecLen)
  # doQuant <- !is.null(quantiles)
  # quantFormat <- formatC(x = quantiles, digits = 4, format = "f",
  #                        decimal.mark = "", big.mark = NULL)
  # 
  # eol <- if(.Platform$OS.type=="windows") "\r\n" else "\n"
  # 
  # 
  # 
  # #loop over all patches and do stats.
  # for(patch in patchList){
  #   
  #   #get male and female files, all repetitions of this patch
  #   maleFiles <- vapply(X = malePatches,
  #                       FUN = grep, pattern = patch, fixed=TRUE, value=TRUE,
  #                       FUN.VALUE = character(length = 1L))
  #   
  #   femaleFiles <- vapply(X = femalePatches,
  #                         FUN = grep, pattern = patch, fixed=TRUE, value=TRUE,
  #                         FUN.VALUE = character(length = 1L))
  #   
  #   
  #   #Read in all repetitions for this patch
  #   for(repetition in 1:numReps){
  #     popDataMale[ ,repetition, ] <- as.matrix(data.table::fread(input = file.path(repFiles[repetition], maleFiles[repetition]),
  #                                                                verbose = FALSE, showProgress = FALSE, drop = c("Time", "Patch")))
  #     popDataFemale[ ,repetition, ] <- as.matrix(data.table::fread(input = file.path(repFiles[repetition], femaleFiles[repetition]),
  #                                                                  verbose = FALSE, showProgress = FALSE, drop = c("Time", "Patch")))
  #   }
  #   
  #   
  #   #setup return file names. Just do them all, whether or not they get used.
  #   maleFileVec[1] <- file.path(writeDirectory, file.path("ADM_Mean_", patch, ".csv", fsep = ""))
  #   femaleFileVec[1] <- file.path(writeDirectory,file.path("AF1_Mean_", patch, ".csv", fsep = ""))
  #   
  #   if(doQuant){
  #     maleFileVec[2:vecLen] <- file.path(writeDirectory,
  #                                        file.path("ADM_Quantile_",
  #                                                  quantFormat,
  #                                                  "_", patch, ".csv", fsep = "")
  #     )
  #     
  #     femaleFileVec[2:vecLen] <- file.path(writeDirectory,
  #                                          file.path("AF1_Quantile_",
  #                                                    quantFormat,
  #                                                    "_", patch, ".csv", fsep = "")
  #     )
  #     
  #   }
    
    
    
    
    
    # #export rest of work to Rcpp
    # backupMGDrivEv2::Mean_Quantiles(maleNames = maleFileVec, femaleNames = femaleFileVec,
    #                                 doMean = doMean, doQuant = doQuant,
    #                                 quantiles = quantiles, colNames = columnNames, eol = eol,
    #                                 maleData = popDataMale, femaleData = popDataFemale)
    
    
    
    
    
    
    #cat("Going into Cpp")
    
    fwritetest::M_Q(writeDir = writeDirectory, maleFiles = maleFiles,
                    femaleFiles = femaleFiles, doMean = doMean, doQuant = doQuant, quantiles = quantiles,
                    numReps = length(repFiles), simTime = simTime,
                    numPatch = numPatch, genotypes = genotypes)
    
    
    # remove files
    if(remFiles){
      # unlink(x = list.dirs(path = readDir, full.names = TRUE, recursive = FALSE),
      #        recursive = TRUE, force = TRUE)
      invisible(file.remove(femaleFiles, maleFiles))
    }
    
    
#  }#end loop over patches
    
    
}#end function


