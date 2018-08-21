#'
#'
#' Split and Aggregate MGDrivE Output
#' 
#' Splits all runs by patch, and aggregates females by mate
#' 
#' @usage Split_Aggregate(readDir, writeDir, remFiles)
#' 
#' @param readDir Directory of subfolders with each run inside
#' @param writeDir Directory with same structure as readDir
#' @param remFiles Boolean, remove read files and directories. Default is FALSE
#'
#' @export
Split_Aggregate_MGDrivE <- function(readDir, writeDir=readDir, simTime, numPatch,
                            genotypes, remFiles=FALSE){
  
  # list all files to be worked on and where to write
  femaleFiles <- list.files(path = readDir, pattern = "AF1", full.names = TRUE, recursive = TRUE)
  maleFiles <- list.files(path = readDir, pattern = "ADM", full.names = TRUE, recursive = TRUE)
  writeFiles <- list.dirs(path = writeDir, full.names = TRUE)[-1]
  
  if(length(writeFiles) != length(femaleFiles) || length(writeFiles) != length(maleFiles)){
    stop("check read/write directories for the correct files and folder structure")
  }
  
  # do the actual work
  fwritetest::Split_Aggregate(outputFiles = writeFiles, maleFiles = maleFiles,
                              femaleFiles = femaleFiles, simTime = simTime,
                              numPatch = numPatch, genotypes = genotypes)
  
  # remove files
  if(remFiles){
    unlink(x = list.dirs(path = readDir, full.names = TRUE), recursive = TRUE, force = TRUE)
  }
  
}