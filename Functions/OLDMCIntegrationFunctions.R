library(qrng)


#n-dimensional MC-integration, types of rng can be selected
mcIntNDim <- function(nValues, nDim, fDim, vectorLowerLimit, vectorUpperLimit, 
                      RNG = c("Sobol","Halton","Pseudo"), returnSequentialVector = FALSE,
                      covMatrix = NULL, muVector = NULL, df = NULL, start = 1, typeOfDist = "N", calcEX =  FALSE, calcVX = FALSE) {
  
  randomVariables <- matrix(0, nValues, nDim)
  #print(RNG)
  
  if (RNG == "Sobol"){
    sobolMatrix <- sobol(nValues, nDim)
    for (j in 1:nDim) {
      randomVariables[, j] <- vectorLowerLimit[j] + (vectorUpperLimit[j] - vectorLowerLimit[j])*sobolMatrix[,j]
    }
  }
  
  else if(RNG == "Pseudo"){
    for (j in 1:nDim) {
      randomVariables[, j] <- runif(nValues, vectorLowerLimit[j], vectorUpperLimit[j])
    }
  }
  
  else if(RNG == "Halton"){
    haltonSequence <- ghalton(nValues, nDim)
    for (j in 1:nDim) {
      randomVariables[, j] <- vectorLowerLimit[j] + (vectorUpperLimit[j] - vectorLowerLimit[j]) * haltonSequence[, j]
    }
  }
  
  sumFn <- 0
  sequentialEstimate <-c() # needed for compareMCIntegration graph function
  
  if(typeOfDist == "T"){
    for(i in start:nValues){
      sumFn <- sumFn + fDim(vectorVariables = randomVariables[i,], covMatrix = covMatrix, muVector = muVector, nDim = nDim, df = df)
      estimate <- prod(vectorUpperLimit - vectorLowerLimit) * sumFn/(i+1-start)
      sequentialEstimate <- append(sequentialEstimate, estimate)
    }  
  }
  
  else{
    for(i in start:nValues){
      sumFn <- sumFn + fDim(vectorVariables = randomVariables[i,], covMatrix = covMatrix, muVector = muVector, nDim = nDim
                            , calcEX =  calcEX, calcVX = calcVX)
      estimate <- prod(vectorUpperLimit - vectorLowerLimit) * sumFn/(i+1-start)
      sequentialEstimate <- append(sequentialEstimate, estimate)
    }  
  }
  

  estimate <- sequentialEstimate[nValues-start+1] 
  
  #If sequentialVector is chosen a vector of estimates is returned, otherwise the final estimate.
  if(!returnSequentialVector){
    return(estimate)
  }
  
    else
      return(sequentialEstimate)
}




# Creates a scatterplot of the convergense of the MC Integration
compareMcIntegrationGraph <- function(nSeries, nValues, nDim, fDim, vectorLowerLimit, 
                                 vectorUpperLimit, vectorRNG, main = "Comparison of Series",
                                 covMatrix = NULL, muVector = NULL, df = NULL, start = 1, typeOfDist = "N", calcEX =  FALSE, calcVX = FALSE){
  
  matrixOfSeries <- matrix(0, nValues-start+1, nSeries)
  
  if(typeOfDist == "T"){
    for (i in 1:nSeries){
      a <-  mcIntNDim(nValues, nDim, fDim, vectorLowerLimit, vectorUpperLimit, RNG = vectorRNG[i], 
                    returnSequentialVector = TRUE, covMatrix = covMatrix, muVector = muVector, start = start, df=df)
      matrixOfSeries[,i] <- a
    }
  }
  else{
    for (i in 1:nSeries){
      a <-  mcIntNDim(nValues, nDim, fDim, vectorLowerLimit, vectorUpperLimit, RNG = vectorRNG[i], 
                      returnSequentialVector = TRUE, covMatrix = covMatrix, muVector = muVector, start = start
                      , calcEX =  calcEX, calcVX = calcVX)
      #print(length(a))
      matrixOfSeries[,i] <- a
    }
  }
  

  seriesColor <- c("red","blue", "green", "orange", "purple", "yellow")
    
  #print estimates
  for(i in 1:nSeries){
    Estimate <- matrixOfSeries[nValues-start+1,i]
    varianceOfEstimate <- var(matrixOfSeries[,i])
    print(paste(vectorRNG[i],"-Estimate:", Estimate))
    print(paste("Variance of ",vectorRNG[i],"-Estimate:", varianceOfEstimate))
  }
  if(typeOfDist == "T"){
     trueValue <-pmvt(vectorLowerLimit, vectorUpperLimit, sigma = covMatrix)[1]
  }
  else{
    trueValue <-pmvnorm(vectorLowerLimit, vectorUpperLimit, sigma = covMatrix)[1]
  }
  print(paste("True value:",trueValue ))
  
    #scatterplot
  plot(1:(nValues-start+1),matrixOfSeries[,1] ,cex = 0.2, pch = 20, col=seriesColor[1], 
    #ylim = c(matrixOfSeries[nValues, 1] -0.005, matrixOfSeries[nValues,1] +0.005),
    main = main , ylab = "Estimate", xlab = "n")
    if(typeOfDist == "T"){
      abline(h = pmvt(vectorLowerLimit, vectorUpperLimit, sigma = covMatrix)[1])
    }
    else{
      abline(h = pmvnorm(vectorLowerLimit, vectorUpperLimit, sigma = covMatrix)[1])
    }
  
    for (i in 2:nSeries){
      points(matrixOfSeries[,i],cex = 0.2, pch = 20, col = seriesColor[i])
    }
      legend("topright", legend = vectorRNG, 
      col = seriesColor[1:nSeries], pch = 20, cex = 0.8)
  
  #Histogram of estimate
  for(i in 1:nSeries){
    hist(matrixOfSeries[,i], main = paste("Distribution of ",vectorRNG[i],"-Estimate"), breaks = round(nValues/2),
      #xlim = c(matrixOfSeries[nValues,1] -0.01, matrixOfSeries[nValues,1] +0.01), 
      xlab = "Estimate")
  }
  
  
}


#returns matrix of metrics of estimate
compareMcIntegrationMetrics <- function(nSeries, nValues, nDim, fDim, vectorLowerLimit, 
                                      vectorUpperLimit, vectorRNG, covMatrix = NULL, 
                                      muVector = NULL, df = NULL, start = 1, typeOfDist = "N", calcEX =  FALSE, calcVX = FALSE){
  
  
  matrixOfSeries <- matrix(0, nValues-start+1, nSeries)
  
  if(typeOfDist == "T"){
    for (i in 1:nSeries){
      matrixOfSeries[,i] <- mcIntNDim(nValues, nDim, fDim, vectorLowerLimit, vectorUpperLimit, RNG = vectorRNG[i], 
                                  returnSequentialVector = TRUE, covMatrix = covMatrix, muVector = muVector, start = start, df = df) 
    }
  }
  
  else{
    for (i in 1:nSeries){
      matrixOfSeries[,i] <- mcIntNDim(nValues, nDim, fDim, vectorLowerLimit, vectorUpperLimit, RNG = vectorRNG[i], 
                                      returnSequentialVector = TRUE, covMatrix = covMatrix, muVector = muVector, start = start
                                      , calcEX =  calcEX, calcVX = calcVX) 
    }
  }
  
  matrixOFEstimateMetrics <- matrix(0, nrow = 3, ncol = nSeries)
  
  for(i in 1:nSeries){
    matrixOFEstimateMetrics[1, i] <- matrixOfSeries[nValues-start+1, i]  # Estimate
    matrixOFEstimateMetrics[2, i] <- var(matrixOfSeries[, i])    # Variance
    matrixOFEstimateMetrics[3, i] <- sum((matrixOfSeries[, i] - matrixOfSeries[nValues-start+1, i])^2) / (nValues-start+1)  # MSE
  }
  rownames(matrixOFEstimateMetrics) <- c("Estimate", "Variance", "MSE")
  colnames(matrixOFEstimateMetrics) <- vectorRNG#paste("Series", seq_len(nSeries))
  
  return(matrixOFEstimateMetrics)
}