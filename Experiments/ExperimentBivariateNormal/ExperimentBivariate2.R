source("../../Functions/ProbabilityDensityFunctions.R")
source("../../Functions/MCIntegrationFunctions.R")
library(mvtnorm)

nseries <- 3
nvalues <- 1000
nDim <- 2
vectorLowerLimit <- c(0.76, 0.76)
vectorUpperLimit <- c(4, 4)
vectorRng <- c("Halton", "Pseudo", "Sobol")
muVector <- c(0,0)
corr <- 0
covMatrix <- cbind(c(1,corr),c(corr,1))


#Plot av variansen vid ökande korrelation
correlationSeq <- seq(-1,1, by = 0.001)
correlationSeq <-correlationSeq[1:(length(correlationSeq)-1)]
correlationSeq <- correlationSeq[-1]
matrixHalton <- matrix(0, 3, 1)
matrixPseudo <- matrix(0, 3, 1)
matrixSobol <- matrix(0, 3, 1)
counter <- 0

for(i in correlationSeq){
  corr <- i
  covMatrix <- cbind(c(1,corr),c(corr,1))
  metricMatrix <- compareMcIntegrationMetrics(nseries,nvalues,nDim,fDim = nDimNormal, 
                         vectorLowerLimit,vectorUpperLimit, vectorRng, covMatrix = covMatrix, 
                         muVector = muVector)
 
  matrixHalton <- cbind(matrixHalton, metricMatrix[,1])
  matrixPseudo <- cbind(matrixPseudo, metricMatrix[,2])
  matrixSobol <- cbind(matrixSobol, metricMatrix[,3])
  
  counter <- counter + 1
  relativeprogress <- round((counter/length(correlationSeq)) * 100, 2)
  print(paste(relativeprogress,"%"))
  
}
matrixPseudo <- matrixPseudo[,-1]
matrixHalton <- matrixHalton[,-1]
matrixSobol <- matrixSobol[,-1]

#pdf(file = "VarianceOfEstimateCorrelatedBivariateNormal.pdf")
plot(correlationSeq, matrixSobol[2,], main = "Variance-Sobol"
     ,cex = 1, pch = 20, col = "red",  xlab = "correlation", ylab = "Variance of estimate", type = "l")
plot(correlationSeq, matrixPseudo[2,], main = "Variance-Pseudo"
     ,cex = 1, pch = 20, col = "blue", xlab = "correlation", ylab = "Variance of estimate", type = "l")
plot(correlationSeq, matrixHalton[2,], main = "Variance-Halton"
     ,cex = 1, pch = 20, col = "green", xlab = "correlation", ylab = "Variance of estimate", type = "l")

#dev.off()