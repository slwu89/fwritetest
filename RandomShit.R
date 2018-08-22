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







microbenchmark::microbenchmark(AnalyzeQuantiles(readDirectory = "~/Desktop/HOLD/MGDrivE/", writeDirectory = "/home/jared/Desktop/HOLD/MGDrivEHOLD/",
                                                doMean = TRUE, quantiles = NULL, simTime = 20, numPatch = 500,
                                                genotypes = c("WW","WH","WR","WB","HH","HR","HB","RR","RB","BB"), remFiles = FALSE),
                               times = 25)


seconds
min      lq     mean   median       uq      max neval
1.139364 1.15583 1.165441 1.163909 1.177769 1.188913    25






AnalyzeQuantiles(readDirectory = "~/Desktop/HOLD/MGDrivE/", writeDirectory = "/home/jared/Desktop/HOLD/MGDrivEHOLD/",
                 doMean = TRUE, quantiles = c(0.1, 0.5), simTime = 20, numPatch = 20,
                 genotypes = c("WW","WH","WR","WB","HH","HR","HB","RR","RB","BB"), remFiles = FALSE)

