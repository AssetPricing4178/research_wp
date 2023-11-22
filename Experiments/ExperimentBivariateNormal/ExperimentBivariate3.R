source("../../Functions/ProbabilityDensityFunctions.R")
source("../../Functions/MCIntegrationFunctions.R")
nseries <- 2
nvalues <- 1000
nDim <- 3
vectorLowerLimit <- c(-1, -1, -1)
vectorUpperLimit <- c(1, 1, 1)
vectorRng <- c("Quasi", "Pseudo")
muVector <- c(0,0, 0)
corr <- 0
covMatrix <- rbind(c(1,corr,0),c(corr,1,0),c(0,0,1))


#Plot av variansen vid ökande korrelation
correlationSeq <- seq(-1,1, by = 0.01)
correlationSeq <-correlationSeq[1:(length(correlationSeq)-1)]
correlationSeq <- correlationSeq[-1]
matrixQuasi <- matrix(0, 3, 1)
matrixPseudo <- matrix(0, 3, 1)
counter <- 0

#en funktion av variansen (corrxy,corrxz, corrzy)
corrxz <- 0
corryz <- 0

for(i in correlationSeq){
  corrxy <- i
  covMatrix <- rbind(c(1,corrxy,corrxz),c(corrxy,1,corryz),c(corrxz,corryz,1))
  
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

pdf(file = "VarianceOfEstimate3dimNormal.pdf")
plot(correlationSeq, matrixQuasi[2,], main = "Variance-Quasi" 
     ,cex = 1, pch = 20, col = "red", type ='l', xlab = "correlation", ylab = "Variance of estimate")
plot(correlationSeq, matrixPseudo[2,], main = "Variance-Pseudo" 
     ,cex = 1, pch = 20, col = "blue", type ='l', xlab = "correlation", ylab = "Variance of estimate")
dev.off()




x <- seq(-0.99, 0.99, length.out = 100)
y <- seq(-0.99, 0.99, length.out = 100)

z <- outer(x, y, nDimNormal(c(x,y),2,covMatrix = diag(1,nrow=2)))

# Basic 3D plot
persp(x, y, z, theta = 30, phi = 30)
