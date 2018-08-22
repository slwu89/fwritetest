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
  
  
  
  
    
    fwritetest::M_Q(writeDir = writeDirectory, inputFiles = list(maleFiles, femaleFiles), doMean = doMean, doQuant = doQuant, quantiles = quantiles,
                    numReps = length(repFiles), simTime = simTime,
                    numPatch = numPatch, genotypes = genotypes)
    
    
    # remove files
    if(remFiles){
      # unlink(x = list.dirs(path = readDir, full.names = TRUE, recursive = FALSE),
      #        recursive = TRUE, force = TRUE)
      invisible(file.remove(femaleFiles, maleFiles))
    }
    
    
    
    
}#end function


