source("../../Functions/ParallelProcessMCIntegration.R")
#source("../../Functions/ImportanceSampling.R")
#sourceCpp("../../Functions/C++MCInt.cpp")
#source("../../Functions/MCIntegrationFunctions.R")
library(rootSolve)
library(ggplot2)
start <- 0
dimSeq <- 1:3
startingNvalues <-10

sobolMatrix <-matrix(0,nrow = length(dimSeq), ncol = 6)
haltonMatrix <-matrix(0,nrow = length(dimSeq), ncol = 6)
pseudoMatrix <-matrix(0,nrow = length(dimSeq), ncol = 6)
sequentialSobolEstimateMatrix <- matrix(NA,nrow = length(dimSeq), ncol = startingNvalues^tail(dimSeq,1))
sequentialPseudoEstimateMatrix <- matrix(NA,nrow = length(dimSeq), ncol = startingNvalues^tail(dimSeq,1))
sequentialHaltonEstimateMatrix <- matrix(NA,nrow = length(dimSeq), ncol = startingNvalues^tail(dimSeq,1))

colnames(sobolMatrix) <- c("Estimate", "Variance", "MSE", "CalcTime","Std. Estimate", "True value")
colnames(haltonMatrix) <- c("Estimate", "Variance", "MSE", "CalcTime","Std. Estimate", "True value")
colnames(pseudoMatrix) <- c("Estimate", "Variance", "MSE", "CalcTime","Std. Estimate", "True value")

nValuesGraph <-c()

for (nDim in dimSeq){
  print(paste("Dimension", nDim))
  
  
  lower <- rep(-1.6448534751579,nDim)#95% right tail
  upper <- rep(5,nDim)
  muVector <- rep(0,nDim)
  covMatrix <- diag(1, nDim)
  nValues <- startingNvalues^nDim
  nValuesGraph <- append(nValuesGraph, nValues*nDim)
  print(paste("#Generated numbers:",nValues*nDim))
  
  # Run your function
  collectionMatrix <- compareMCIntegrationMetrics(f = dmvnorm,lower, upper, muVector, 
                                                  covMatrix = covMatrix, nValues = nValues, start = start)
  
  
  
  sobolMatrix[nDim, ] <- collectionMatrix$estimateMatrix[1,]
  haltonMatrix[nDim, ] <- collectionMatrix$estimateMatrix[2,]
  pseudoMatrix[nDim, ] <- collectionMatrix$estimateMatrix[3,]
  
  sequentialSobolEstimateMatrix[nDim,1:nValues] <- collectionMatrix$sobolVector
  sequentialPseudoEstimateMatrix[nDim,1:nValues] <- collectionMatrix$pseudoVector
  sequentialHaltonEstimateMatrix[nDim,1:nValues] <- collectionMatrix$haltonVector
  
 
  
}



