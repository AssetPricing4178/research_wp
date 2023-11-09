library(qrng)


#n-dimensional MC-integration, types of rng can be selected
mcIntNDim <- function(nvalues, nDim, fDim, vectorLowerLimit, vectorUpperLimit, 
                      RNG = c("Quasi", "Pseudo"), returnSequentialVector = FALSE,
                      covMatrix = NULL, muVector = NULL) {
  
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
  sequentialEstimate <-c() # needed for compareMCIntegration graph function
  for(i in 1:nvalues){
    #sumFn <- sumFn + do.call(fDim, as.list(randomVariables[i, ],covMatrix = covMatrix)) #knas borde ändra fdim så att den tar emot en vector
    
    sumFn <- sumFn + fDim(randomVariables[i,], covMatrix = covMatrix, muVector = muVector, nDim = nDim)
    estimate <- prod(vectorUpperLimit - vectorLowerLimit) * sumFn/i
    sequentialEstimate <- append(sequentialEstimate, estimate) 
    
  }
  
 
  estimate <- sequentialEstimate[nvalues] 
  
  #If sequentialVector is chosen a vector of estimates is returned, otherwise the final estimate.
  if(!returnSequentialVector){
    return(estimate)
  }
  
    else
      return(sequentialEstimate)
}




# Creates a scatterplot of the convergense of the MC Integration
compareMcIntegration <- function(nseries, nvalues, nDim, fDim, vectorLowerLimit, 
                                 vectorUpperLimit, vectorRNG, main = "Comparison of Series",
                                 covMatrix = NULL, muVector = NULL){
 
  matrixOfSeries <- matrix(0,nvalues, nseries)
  
  for (i in 1:nseries){
    matrixOfSeries[,i] <- mcIntNDim(nvalues, nDim, fDim, vectorLowerLimit, vectorUpperLimit, RNG = vectorRNG[i], 
                                    returnSequentialVector = TRUE, covMatrix = covMatrix, muVector = muVector) #length 0??
  }
  
  seriesColor <- c("red","blue", "green", "orange", "purple", "yellow")
  
  #print estimates
  for(i in 1:nseries){
    Estimate <- matrixOfSeries[nvalues,i]
    varianceOfEstimate <- var(matrixOfSeries[,i])
    print(paste(vectorRNG[i],"-Estimate:", Estimate))
    print(paste("Variance of ",vectorRNG[i],"-Estimate:", varianceOfEstimate))
  }
  
  
  #scatterplot
  plot(1:nvalues,matrixOfSeries[,1] ,cex = 0.2, pch = 20, col=seriesColor[1], 
    #ylim = c(matrixOfSeries[nvalues, 1] -0.005, matrixOfSeries[nvalues,1] +0.005),
    main = main , ylab = "Estimate", xlab = "n")
  for (i in 2:nseries){
    points(matrixOfSeries[,i],cex = 0.2, pch = 20, col = seriesColor[i])
  }
    legend("topright", legend = vectorRNG, 
    col = seriesColor[1:nseries], pch = 20, cex = 0.8)
  
  #Histogram of estimate
  for(i in 1:nseries){
    hist(matrixOfSeries[,i], main = paste("Distribution of ",vectorRng[i],"-Estimate"), breaks = round(nvalues/2),
         #xlim = c(matrixOfSeries[nvalues,1] -0.01, matrixOfSeries[nvalues,1] +0.01), 
         xlab = "Estimate")
  }
}