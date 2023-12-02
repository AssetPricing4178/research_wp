source("../../Functions/ParallelProcessMCIntegration.R")
#source("../../Functions/MCIntegrationFunctions.R")
#source("../../Functions/SegmentedMCIntegrationFunctions.R")
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
lowerZ <- uniroot(f = function(x) x-0.05, interval = c(0, 1))$root
lastDistance <- abs(-qnorm(lowerZ)-5)
for (nDim in dimSeq){
print(paste("Dimension", nDim))

  
lowerZ <- uniroot(f = function(x) x^nDim -0.05, interval = c(0, 1))$root
lowerX <- -qnorm(lowerZ)
lower <- rep(lowerX,nDim)
upper <- rep(5,nDim)
distance <- abs(lowerX-5)
#print(paste(lowerX,"-",5,"=", distance))
#print(lastDistance)
#print(distance)

muVector <- rep(0,nDim)
covMatrix <- diag(1, nDim)
compensationFactor <- distance/lastDistance
print(paste("Compensation factor:",compensationFactor))
nValues <- round((compensationFactor * 70)^nDim)
#nValues <- round(compensationFactor*1000)

nValuesGraph <- append(nValuesGraph, compensationFactor*nValues*nDim)
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


