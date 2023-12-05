source("../../Functions/ParallelProcessMCIntegration.R")
library(mvtnorm)
#90%
lower <- c(-1.6449,-1.6449)
upper <-c(5,5)
corrSeq <-seq(-1,1,by =0.01)
corrSeq <- head(corrSeq, length(corrSeq)-1)
corrSeq <- tail(corrSeq,length(corrSeq)-1)
muVector <- c(0,0)
nValues <- 100
start <- 0

resultListNormal <- list()
varianceDFNormal <- data.frame(row.names = c(corrSeq),sobol = numeric(length(corrSeq)),
                                    halton = numeric(length(corrSeq)),
                                    pseudo = numeric(length(corrSeq)))
sequentialEstimateHaltonNormal <- data.frame(matrix(NA, nrow = nValues, ncol = length(corrSeq)))
colnames(sequentialEstimateHaltonNormal) <-c(corrSeq)
sequentialEstimateSobolNormal <-  data.frame(matrix(NA, nrow = nValues, ncol = length(corrSeq)))
colnames(sequentialEstimateSobolNormal) <-c(corrSeq)
sequentialEstimatePseudoNormal <- data.frame(matrix(NA, nrow = nValues, ncol = length(corrSeq)))
colnames(sequentialEstimatePseudoNormal) <-c(corrSeq)

resultListT <- list()
varianceDFT <- data.frame(row.names = c(corrSeq),sobol = numeric(length(corrSeq)),
                               halton = numeric(length(corrSeq)),
                               pseudo = numeric(length(corrSeq)))

sequentialEstimateHaltonT <- data.frame(matrix(NA, nrow = nValues, ncol = length(corrSeq)))
colnames(sequentialEstimateHaltonT) <-c(corrSeq)
sequentialEstimateSobolT <-  data.frame(matrix(NA, nrow = nValues, ncol = length(corrSeq)))
colnames(sequentialEstimateSobolT) <-c(corrSeq)
sequentialEstimatePseudoT <- data.frame(matrix(NA, nrow = nValues, ncol = length(corrSeq)))
colnames(sequentialEstimatePseudoT) <-c(corrSeq)

counter <- 0
for(corr in corrSeq){
  counter <- counter + 1
  print(corr)
  covmatrix <- matrix(c(1, corr, corr, 1), nrow = 2, ncol = 2)
  resultNormal <- compareMCIntegrationMetrics(f = dmvnorm, lower, upper, muVector,
                              covMatrix = covmatrix, nValues = nValues, start = start)

  resultListNormal[[as.character(corr)]] <- resultNormal
 
  varianceDFNormal[counter, ] <- resultNormal$estimateMatrix[,2 ]

  sequentialEstimateHaltonNormal[,counter] <-c(resultNormal$haltonVector)
  sequentialEstimateSobolNormal[,counter] <-c(resultNormal$sobolVector)
  sequentialEstimatePseudoNormal[,counter] <-c(resultNormal$pseudoVector)
 
  
  resultT <- compareMCIntegrationMetrics(f = dmvt, lower = lower, upper = upper, muVector = muVector,
                                              covMatrix = covmatrix, nValues = nValues, start = start, df = 1)
  resultListT[[as.character(corr)]] <- resultT
  varianceDFT[counter, ] <- resultT$estimateMatrix[,2]
  sequentialEstimateHaltonT[,counter] <-resultT$haltonVector
  sequentialEstimateSobolT[,counter] <-resultT$sobolVector
  sequentialEstimatePseudoT[,counter] <-resultT$pseudoVector
}
write.table(sequentialEstimateSobolT, file = "sobolT.csv", sep = ";")
write.table(sequentialEstimatePseudoT, file = "pseudoT.csv", sep = ";")
write.table(sequentialEstimateHaltonT, file = "haltonT.csv", sep = ";")
write.table(varianceDFT, file = "varT.csv", sep = ";")


write.table(sequentialEstimateSobolNormal, file = "sobolNormal.csv", sep = ";")
write.table(sequentialEstimatePseudoNormal, file = "pseudoNormal.csv", sep = ";")
write.table(sequentialEstimateHaltonNormal, file = "haltonNormal.csv", sep = ";")
write.table(varianceDFNormal, file = "varNormal.csv", sep = ";")


