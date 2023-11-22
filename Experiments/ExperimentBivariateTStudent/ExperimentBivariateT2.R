source("../../Functions/ProbabilityDensityFunctions.R")
source("../../Functions/MCIntegrationFunctions.R")
#Plot av variansen vid ökande korrelation
correlationSeq <- seq(-1,1, by = 0.001)
correlationSeq <-correlationSeq[1:(length(correlationSeq)-1)]
correlationSeq <- correlationSeq[-1]
matrixQuasi <- matrix(0, 3, 1)
matrixPseudo <- matrix(0, 3, 1)
nvalues = 5000
lower <-c(-15,-15)
upper <-c(15,15)
muvector <-c(0,0)
ndim <-2
df <- 1
counter <- 0
nseries <- 2
ndim <- 2

for(i in correlationSeq){
  corr <- i
  covMatrix <- cbind(c(1,corr),c(corr,1))
  
  metricMatrix <- compareMcIntegrationMetrics(nSeries = 2,nDim = 2, nValues = nvalues, fDim = nDimTStudent,
                                              vectorLowerLimit = lower, vectorUpperLimit = upper, 
                                              vectorRNG = c("Quasi", "Pseudo"),
                                              covMatrix = covMatrix, df = df,
                                              muVector = muvector)
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
     ,cex = 1, pch = 20, col = "red",  xlab = "correlation", ylab = "Variance of estimate", type  = "l")
plot(correlationSeq, matrixPseudo[2,], main = "Variance-Pseudo"
     ,cex = 1, pch = 20, col = "blue", xlab = "correlation", ylab = "Variance of estimate", type  = "l")

#dev.off()