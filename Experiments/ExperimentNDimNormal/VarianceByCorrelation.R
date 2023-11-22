correlationSeq <- seq(-1,1, by = 0.01)
correlationSeq <-correlationSeq[1:(length(correlationSeq)-1)]
correlationSeq <- correlationSeq[-1]

matrixHalton <- matrix(0, 3, 1)
matrixPseudo <- matrix(0, 3, 1)
matrixSobol <- matrix(0, 3, 1)

#counter <- 0
for(i in correlationSeq){
  corrx12 <- i
  correlationVector <-rep(0, nDim)#x12,x13,x23
  correlationVector[1] <- corrx12
  covMatrix <- generateCovarianceMatrix(correlationVector)
  
  
  metricMatrix <- compareMcIntegrationMetrics(nseries,nvalues,nDim,fDim = nDimNormal, 
                                              vectorLowerLimit,vectorUpperLimit, vectorRng, covMatrix = covMatrix, 
                                              muVector = muVector)
  matrixHalton <- cbind(matrixHalton, metricMatrix[,1])
  matrixPseudo <- cbind(matrixPseudo, metricMatrix[,2])
  matrixSobol <- cbind(matrixSobol, metricMatrix[,3])
  
  counter <- counter + 1
  relativeprogress <- round(counter/(length(correlationSeq)*length(dimSeq)) * 100, 2)
  print(paste(relativeprogress,"%"))
  
}
matrixPseudo <- matrixPseudo[,-1]
matrixHalton <- matrixHalton[,-1]
matrixSobol <- matrixSobol[,-1]
innerList <-list(matrixPseudo, matrixHalton, matrixSobol)
listofVarianceByCorrelation[[nDim]] <- innerList


plot(correlationSeq, matrixSobol[2,], main = "Variance-Sobol"
     ,cex = 1, pch = 20, col = "red",  xlab = "correlation", ylab = "Variance of estimate", type = "l")
plot(correlationSeq, matrixPseudo[2,], main = "Variance-Pseudo"
     ,cex = 1, pch = 20, col = "blue", xlab = "correlation", ylab = "Variance of estimate", type = "l")
plot(correlationSeq, matrixHalton[2,], main = "Variance-Halton"
     ,cex = 1, pch = 20, col = "green", xlab = "correlation", ylab = "Variance of estimate", type = "l")


