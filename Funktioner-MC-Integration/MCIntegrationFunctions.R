library(qrng)

#n-dimensional MC-integration, types of rng can be selected
mcIntNDim <- function(nvalues, nDim, fDim, vectorLowerLimit, vectorUpperLimit, RNG = c("Quasi", "Pseudo"), returnSequentialVector = FALSE) {
  
  randomVariables <- matrix(0, nvalues, nDim)
  
  if (RNG == "Quasi"){
    sobolMatrix <- sobol(nvalues, nDim)
    for (j in 1:nDim) {
      randomVariables[, j] <- vectorLowerLimit[j] + (vectorUpperLimit[j] - vectorLowerLimit[j])*sobolMatrix[,j]
    }
  }
  
  else if(RNG == "Pseudo"){
    for (j in 1:nDim) {
      randomVariables[, j] <- runif(nvalues, vectorLowerLimit[j], vectorUpperLimit[j])
    }
  }
  sumFn <- 0
  sequentialMean <-c() # needed for compareMCIntegration graph function
  for(i in 1:nvalues){
    sumFn <- sumFn + do.call(fDim, as.list(randomVariables[i, ]))
    sequentialMean <- append(sequentialMean, sumFn/i) 
    
  }
  
  mean <- sumFn /nvalues
  estimate <- mean * prod(vectorUpperLimit - vectorLowerLimit)
  
  #If sequentialVector is chosen a vector of estimates is chosen, otherwise the final estimate
  if(!returnSequentialVector){
    return(estimate)
  }
  
    else
      return(sequentialMean)
}

# Creates a scatterplot of the convergense of the MC Integration
compareMcIntegration <- function(nseries, nvalues, nDim, fDim, vectorLowerLimit, vectorUpperLimit, vectorRNG){
  
  matrixOfSeries <- matrix(0, nseries, nvalues)
  for (i in 1:nseries){
    matrixOfSeries[i,] <- mcIntNDim(nvalues, nDim, fDim, vectorLowerLimit, vectorUpperLimit, RNG = vectorRNG[i], returnSequentialVector = TRUE)
  }
  
  
  seriesColor <- c("red","blue", "green", "orange", "purple", "yellow")
  
  plot(1:nvalues,matrixOfSeries[1,] ,cex = 0.2, pch = 20, col=seriesColor[1], 
       ylim = c(matrixOfSeries[1,nvalues] -0.005, matrixOfSeries[1,nvalues] +0.005),
       main = "Comparison of Series", ylab = "Estimate", xlab = "n")
  for (i in 2:nseries){
    points(matrixOfSeries[i,],cex = 0.2, pch = 20, col = seriesColor[i])
  }
  legend("topright", legend = vectorRNG, 
         col = seriesColor[1:nseries], pch = 20, cex = 0.8)
}