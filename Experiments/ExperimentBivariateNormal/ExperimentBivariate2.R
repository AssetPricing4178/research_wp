source("MCIntegrationFunctions.R")
source("ProbabilityDensityFunctions.R")

nseries <- 2
nvalues <- 5000
nDim <- 2
vectorLowerLimit <- c(-50, -50)
vectorUpperLimit <- c(50, 50)
vectorRng <- c("Quasi", "Pseudo")
muVector <- c(0,0)
corr <- 0
covMatrix <- cbind(c(1,corr),c(corr,1))


#Plot av variansen vid ökande korrelation
correlationSeq <- seq(-1,1, by = 0.01)
correlationSeq <-correlationSeq[1:(length(correlationSeq)-1)]
correlationSeq <- correlationSeq[-1]
matrixQuasi <- matrix(0, 3, 1)
matrixPseudo <- matrix(0, 3, 1)
counter <- 0

for(i in correlationSeq){
  corr <- i
  covMatrix <- cbind(c(1,corr),c(corr,1))
  
  metricMatrix <- compareMcIntegrationMetrics(nseries,nvalues,nDim,fDim = nDimNormal, 
                         vectorLowerLimit,vectorUpperLimit, vectorRng, covMatrix = covMatrix, 
                         muVector = muVector)
  matrixQuasi <- cbind(matrixQuasi, metricMatrix[,1])
  matrixPseudo <- cbind(matrixPseudo, metricMatrix[,2])
  
  counter <- counter + 1
  relativeprogress <- round((counter/length(correlationSeq)) * 100, 2)
  print(paste(relativeprogress,"%"))
  
}
matrixPseudo <- matrixPseudo[,-1]
matrixQuasi <- matrixQuasi[,-1]

#pdf(file = "VarianceOfEstimateCorrelatedBivariateNormal.pdf")
plot(correlationSeq, matrixQuasi[2,], main = "Variance-Quasi" 
     ,cex = 1, pch = 20, col = "red",  xlab = "correlation", ylab = "Variance of estimate")
plot(correlationSeq, matrixPseudo[2,], main = "Variance-Pseudo" 
     ,cex = 1, pch = 20, col = "blue", xlab = "correlation", ylab = "Variance of estimate")
#dev.off()
