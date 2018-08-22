femaleData <- as.matrix(read.csv(file = "/home/jared/Desktop/HOLD/MGDrivE/MGDRIVE_DET_C/AF1_Run001.csv", header = TRUE, stringsAsFactors = FALSE))
maleData <- as.matrix(read.csv(file = "/home/jared/Desktop/HOLD/MGDrivE/MGDRIVE_DET_C/ADM_Run001.csv", header = TRUE, stringsAsFactors = FALSE))


maleFile <- "/home/jared/Desktop/HOLD/MGDrivE/MGDRIVE_DET_C/ADM_Run001.csv"
femaleFile <- "/home/jared/Desktop/HOLD/MGDrivE/MGDRIVE_DET_C/AF1_Run001.csv"


maleFiles <- list.files(path = "~/Desktop/HOLD/MGDrivE/", pattern = "ADM", full.names = TRUE, recursive = TRUE)
femaleFiles <- list.files(path = "~/Desktop/HOLD/MGDrivE/", pattern = "AF1", full.names = TRUE, recursive = TRUE)


fwritetest::Split_Aggregate(maleFile = maleFiles, femaleFile = femaleFiles,
                            simTime = 500, numPatch = 5, genotypes = c("WW","WH","WR","WB","HH","HR","HB","RR","RB","BB"))




fwritetest::testRead(fileName = "/home/jared/Desktop/HOLD/MGDrivE/MGDRIVE_DET_C/ADM_Run001.csv")




tA <- Sys.time()
fwritetest::Split_Aggregate_MGDrivE(readDir = "~/Desktop/HOLD/MGDrivE",
                                    simTime = 1000, numPatch = 50,
                                    genotypes = c("WW","WH","WR","WB","HH","HR","HB","RR","RB","BB"),
                                    remFiles = TRUE)
tB <- Sys.time()

tB-tA







microbenchmark::microbenchmark(fwritetest::AnalyzeQuantiles(readDirectory = "~/Desktop/HOLD/MGDrivE/", writeDirectory = "/home/jared/Desktop/HOLD/MGDrivEHOLD/",
                                                doMean = TRUE, quantiles = NULL, simTime = 1000, numPatch = 1000,
                                                genotypes = c("WW","WH","WR","WB","HH","HR","HB","RR","RB","BB"), remFiles = FALSE),
                               MGDrivE::AnalyzeQuantiles(readDirectory = "~/Desktop/HOLD/MGDrivE/",
                                                         writeDirectory = "~/Desktop/HOLD/MGDriveHOLD_OLD/",
                                                         mean = TRUE,
                                                         quantiles = c(0.1, 0.5)),
                               times = 10)


100 nodes
miliseconds
min       lq      mean    median        uq       max neval
404.2001  408.293  411.8978  409.0209  411.5154  439.1044    10
4879.5569 4929.987 4979.5918 4947.2186 4967.3995 5184.2496    10

500 nodes
seconds
min        lq      mean    median        uq       max neval
2.751668  2.812007  2.862977  2.825714  2.885851  3.104741    25
25.237478 25.484354 25.638120 25.597291 25.724277 26.409201    25

1000 nodes
seconds
min        lq      mean    median        uq       max neval
8.142908  8.241141  8.592716  8.607431  8.704635  9.428544    10
54.969962 55.581985 56.130251 55.799082 56.368240 58.922647    10






MGDrivE::AnalyzeQuantiles(readDirectory = "~/Desktop/HOLD/MGDrivE/",
                 writeDirectory = "~/Desktop/HOLD/MGDriveHOLD_OLD/",
                 mean = TRUE,
                 quantiles = c(0.1, 0.5)
)







AnalyzeQuantiles(readDirectory = "~/Desktop/HOLD/MGDrivE/", writeDirectory = "~/Desktop/HOLD/MGDrivEHOLD/",
                 doMean = TRUE, quantiles = c(0.1, 0.5), simTime = 1000, numPatch = 1000,
                 genotypes = c("WW","WH","WR","WB","HH","HR","HB","RR","RB","BB"), remFiles = FALSE)



newFiles <- list.files(path = "~/Desktop/HOLD/MGDrivEHOLD/", full.names = TRUE)
oldFiles <- list.files(path = "~/Desktop/HOLD/MGDriveHOLD_OLD/", full.names = TRUE)


for(file in 1:length(newFiles)){
  
  newQuant <- as.matrix(read.csv(file = newFiles[file], header = TRUE))
  oldQuant <- as.matrix(read.csv(file = oldFiles[file], header = TRUE))
  
  print(all.equal(oldQuant, newQuant))
  
  
  
}
