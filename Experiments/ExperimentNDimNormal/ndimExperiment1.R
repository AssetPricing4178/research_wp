source("../../Functions/ProbabilityDensityFunctions.R")
source("../../Functions/TESTMCIntegrationFunctions.R")
library(mvtnorm)
library(rootSolve)
#dimSeq <- c(2,3,4,5,6,7,8,9,10,15,25,30,35,40,50,60,65)
dimSeq <- 2
listOfMetricMatrixes <- list()
dfOfMetricMatrixes <- data.frame(matrix(nrow = 3, ncol = 0))

counter <- 0
for(nDim in dimSeq){
  counter <- counter + 1
  print(paste("Dimension", nDim))
  nseries <- 3
  nvalues <- 1000^(nDim-1)
  #nDim <- 3
  lowerZ <- uniroot(f = function(x) x^nDim -0.05, interval = c(0, 1))$root
  lower <--qnorm(lowerZ)
  #print(lower)
  vectorLowerLimit <- rep(lower,nDim)
  vectorUpperLimit <- rep(5,nDim)
  vectorRng <- c("Halton", "Pseudo", "Sobol")
  muVector <- rep(0,nDim)
  correlationVector <- rep(0,nDim)#x12,x13,x23
  covMatrix <- generateCovarianceMatrix(correlationVector)
  #print(covMatrix)
  
  compareMcIntegrationGraph(nseries, nvalues, nDim, fDim = nDimNormal, 
                            vectorLowerLimit, vectorUpperLimit, 
                            vectorRng, covMatrix = covMatrix, 
                            muVector = muVector, typeOfDist = "N", start = 100, main = paste(nDim," dimensions"))
  
  metricMatrix <- compareMcIntegrationMetrics(nseries,nvalues,nDim,fDim = nDimNormal, 
                                              vectorLowerLimit,vectorUpperLimit, vectorRng, covMatrix = covMatrix, 
                                              muVector = muVector)
  
  listOfMetricMatrixes[[nDim]] <- metricMatrix
  
  # Assuming metricMatrix is your data frame or matrix
  colnames(metricMatrix) <- paste0(nDim, colnames(metricMatrix))
  
  # Binding specific columns of metricMatrix to dfOfMetricMatrixes
  dfOfMetricMatrixes <- cbind(dfOfMetricMatrixes, metricMatrix[, 1:3])
  
  # Updating column names for the last three columns
  colnames(dfOfMetricMatrixes)[(ncol(dfOfMetricMatrixes)-2):ncol(dfOfMetricMatrixes)] <- colnames(metricMatrix)
  


}
View(dfOfMetricMatrixes)
haltonSeries <- dfOfMetricMatrixes[,seq(1,175,by =3)]
pseudoSeries <- dfOfMetricMatrixes[,seq(2,176,by =3)]
sobolSeries <- dfOfMetricMatrixes[,seq(3,177,by =3)]

pdf(file = "ExperimentNormalIncreasingDimResults.pdf")
#Estimate

plot(2:60, haltonSeries[1,],cex = 1, pch = 20, col = "green",type = "p", main ="Halton-Estimate", xlab = "Dimension"
     , ylab = "Estimate")
abline(h =0.05)
grid()


plot(2:60,sobolSeries[1,],cex = 1, pch = 20, col = "red",type = "p", main ="Sobol-Estimate", xlab = "Dimension"
     , ylab = "Estimate")
abline(h =0.05)
grid()


plot(2:60,pseudoSeries[1,],cex = 1, pch = 20, col = "blue",type = "p", main ="Pseudo-Estimate", xlab = "Dimension"
     , ylab = "Estimate")
abline(h =0.05)
grid()

#Variance of Estimate
plot(2:60, haltonSeries[2,],cex = 1, pch = 20, col = "green",type = "p", main ="Variance of Halton-Estimate", xlab = "Dimension"
     , ylab = "Variance of estimate")

grid()


plot(2:60,sobolSeries[2,],cex = 1, pch = 20, col = "red",type = "p", main ="Variance of Sobol-Estimate", xlab = "Dimension"
     , ylab = "Variance of estimate")

grid()


plot(2:60,pseudoSeries[2,],cex = 1, pch = 20, col = "blue",type = "p", main ="Variance of Pseudo-Estimate", xlab = "Dimension"
     , ylab = "Variance of estimate")

grid()

#MSE of Estimate
plot(2:60, haltonSeries[3,],cex = 1, pch = 20, col = "green",type = "p", main ="MSE of Halton-Estimate", xlab = "Dimension"
     , ylab = "MSE of estimate")

grid()


plot(2:60,sobolSeries[3,],cex = 1, pch = 20, col = "red",type = "p", main ="MSE of Sobol-Estimate", xlab = "Dimension"
     , ylab = "MSE of estimate")

grid()


plot(2:60,pseudoSeries[3,],cex = 1, pch = 20, col = "blue",type = "p", main ="MSE of Pseudo-Estimate", xlab = "Dimension"
     , ylab = "MSE of estimate")

grid()

#type l
plot(2:60, haltonSeries[1,],cex = 1, pch = 20, col = "green",type = "l", main ="Halton-Estimate", xlab = "Dimension"
     , ylab = "Estimate")
abline(h =0.05)
grid()


plot(2:60,sobolSeries[1,],cex = 1, pch = 20, col = "red",type = "l", main ="Sobol-Estimate", xlab = "Dimension"
     , ylab = "Estimate")
abline(h =0.05)
grid()


plot(2:60,pseudoSeries[1,],cex = 1, pch = 20, col = "blue",type = "l", main ="Pseudo-Estimate", xlab = "Dimension"
     , ylab = "Estimate")
abline(h =0.05)
grid()

#Variance of Estimate
plot(2:60, haltonSeries[2,],cex = 1, pch = 20, col = "green",type = "l", main ="Variance of Halton-Estimate", xlab = "Dimension"
     , ylab = "Variance of estimate")

grid()


plot(2:60,sobolSeries[2,],cex = 1, pch = 20, col = "red",type = "l", main ="Variance of Sobol-Estimate", xlab = "Dimension"
     , ylab = "Variance of estimate")

grid()


plot(2:60,pseudoSeries[2,],cex = 1, pch = 20, col = "blue",type = "l", main ="Variance of Pseudo-Estimate", xlab = "Dimension"
     , ylab = "Variance of estimate")

grid()

#MSE of Estimate
plot(2:60, haltonSeries[3,],cex = 1, pch = 20, col = "green",type = "l", main ="MSE of Halton-Estimate", xlab = "Dimension"
     , ylab = "MSE of estimate")

grid()


plot(2:60,sobolSeries[3,],cex = 1, pch = 20, col = "red",type = "l", main ="MSE of Sobol-Estimate", xlab = "Dimension"
     , ylab = "MSE of estimate")

grid()


plot(2:60,pseudoSeries[3,],cex = 1, pch = 20, col = "blue",type = "l", main ="MSE of Pseudo-Estimate", xlab = "Dimension"
     , ylab = "MSE of estimate")

grid()

dev.off()