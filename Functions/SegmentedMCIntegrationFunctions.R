library(qrng)
library(pryr)
library(mvtnorm)
library(foreach)
library(doParallel)

#nValues =< 2^32-1
mcIntNDimSequential <- function(f, lower, upper, muVector, covMatrix, 
                                RNG, nValues, df = NULL, numCores = 4, chunkSize = NULL) {
  
  gc()
  nValuesPseudo <-nValues
  nValues <- min(nValues, 2^31-1)# limit of qrng functions
  nDim <- length(upper)
  
  
  # Register parallel backend
  cl <- makeCluster(numCores)
  registerDoParallel(cl)
  
  # Load necessary libraries on each worker
  clusterEvalQ(cl, library(qrng))
  
  # Function to combine results from chunks
  combineChunks <- function(...) c(...)
  
  if(!(RNG == "Pseudo")){
    chunkSize <- ceiling(nValues / (numCores * nDim))
  }
  else{
    chunkSize <- ceiling(nValuesPseudo / (numCores * nDim))
  }
  if (RNG == "Sobol") {
    print("Sobol")
    randomVariables <- foreach(i = 1:numCores, .combine = combineChunks) %dopar% {
      chunk <- t(t(sobol(chunkSize, nDim)) * (upper - lower)) + lower
      f(chunk, mean = muVector, sigma = covMatrix)
    }
  } else if (RNG == "Halton") {
    print("Halton")
    randomVariables <- foreach(i = 1:numCores, .combine = combineChunks) %dopar% {
      chunk <- t(t(ghalton(chunkSize, nDim)) * (upper - lower)) + lower
      f(chunk, mean = muVector, sigma = covMatrix)
    }
  } else {
    print("Pseudo")
    randomVariables <- foreach(i = 1:numCores, .combine = combineChunks) %dopar% {
      chunk <- matrix(runif(chunkSize * nDim, lower, upper), chunkSize, nDim)
      f(chunk, mean = muVector, sigma = covMatrix)
    }
  }
  
  stopCluster(cl)
  
  # Combine results from different cores
  values <- unlist(randomVariables)
  
  # Calculate the cumulative sum of the results
  cumulativeSum <- as.vector(cumsum(values))
  
  # Store the last nValues values for storage purposes
  sequentialEstimateVector <- prod(upper - lower) * cumulativeSum / seq_along(cumulativeSum)
  
  return(sequentialEstimateVector)
}

compareMCIntegrationMetrics <- function(f, lower, upper, muVector, covMatrix
                                        , nValues, start = 1, df = NULL){
  nValues <- min(nValues, 2^31-1)
  nDim <- length(upper)
  lowerSeq <-seq(lower[1],upper[1],by=0.01)
  print(lowerSeq)
  if (is.null(df)) {
    trueValue <- pmvnorm(lower, upper, muVector, sigma = covMatrix)
  } else {
    trueValue <- pmvt(lower, upper, muVector, sigma = covMatrix, df = df)
  }
  
  
  sobolTime <- system.time({
    for(lower in lowerSeq){
      sobolVector <- mcIntNDimSequential(f, rep(lower,nDim), upper, muVector, covMatrix, 
                                       RNG = "Sobol", nValues)
    }
  })["elapsed"]
  
  
  haltonTime <- system.time({
    haltonVector <- mcIntNDimSequential(f, lower, upper, muVector, covMatrix, 
                                        RNG = "Halton", nValues)
  })["elapsed"]
  
  
  pseudoTime <- system.time({
    pseudoVector <- mcIntNDimSequential(f, lower, upper, muVector, covMatrix, 
                                        RNG = "Pseudo", nValues)
  })["elapsed"]
  
  
  
  
  
  estimateVector <- c(Sobol = tail(sobolVector, 1), Halton = tail(haltonVector, 1),
                      Pseudo = tail(pseudoVector, 1) )
  varianceVector <- c(Sobol = var(sobolVector), Halton = var(haltonVector),
                      Pseudo = var(pseudoVector) )
  mseVector <-c(Sobol = mean((trueValue -sobolVector)^2), 
                Halton = mean((trueValue -haltonVector)^2),
                Pseudo = mean((trueValue -pseudoVector)^2) )
  calcTime <-c(Sobol = sobolTime, Halton = haltonTime, Pseudo = pseudoTime)
  
  estimateMatrix <- cbind(estimateVector, varianceVector, mseVector, calcTime)
  rownames(estimateMatrix) <- c("Sobol", "Halton", "Pseudo")
  return(estimateMatrix)
  
}