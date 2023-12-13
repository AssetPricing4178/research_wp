source("../../Functions/ParallelProcessMCIntegration.R")
library(mvtnorm)
#95%
lower <- c(-1.6449,-1.6449)
upper <-c(5,5)
corrSeq <-seq(-1,1,by = 0.001)
corrSeq <- head(corrSeq, length(corrSeq)-1)
corrSeq <- tail(corrSeq,length(corrSeq)-1)
muVector <- c(0,0)
nValues <- 1000
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
 
  

}



write.table(sequentialEstimateSobolNormal, file = "sobolNormal.csv", sep = ";")
write.table(sequentialEstimatePseudoNormal, file = "pseudoNormal.csv", sep = ";")
write.table(sequentialEstimateHaltonNormal, file = "haltonNormal.csv", sep = ";")
write.table(varianceDFNormal, file = "varNormalFixedLimits.csv", sep = ";")


