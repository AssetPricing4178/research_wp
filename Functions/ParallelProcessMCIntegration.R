library(qrng)
#library(randtoolbox)
library(pryr)
library(mvtnorm)
library(foreach)
library(doParallel)

mcIntNDim <- function(f, lower, upper, muVector, covMatrix, 
                      RNG, nValues, df = NULL) {
  nDim <- length(upper)
  #randomVariables <- matrix(0, nValues, nDim)
  
  if (RNG == "Sobol") {
    sobolMatrix <- sobol(nValues, nDim)
    randomVariables <- t(apply(sobolMatrix, 1, function(row) lower + (upper - lower) * row))
  }
  
  else if (RNG == "Halton") {
    haltonMatrix <- ghalton(nValues, nDim)
    randomVariables <- t(apply(haltonMatrix, 1, function(row) lower + (upper - lower) * row))
  }
  
  else{
    randomVariables <- t(apply(matrix(0, nValues, nDim), 1, function(row) runif(nDim, lower, upper)))
  }
  
  
  sum <- apply(randomVariables, 1, function(row) f(row, mean = muVector, sigma = covMatrix))
  meanEstimate <- mean(sum)
  estimate <- prod(upper - lower) * meanEstimate
  return(estimate)
}

#nValues =< 2^32-1
mcIntNDimSequential <- function(f, lower, upper, muVector, covMatrix, 
                                RNG, nValues, df = NULL, numCores = 4, chunkSize = 64) {
  
  nValues <- min(nvalues, 2^31-1)# limit of qrng functions
  gc()
  nDim <- length(upper)
  
  # Register parallel backend
  cl <- makeCluster(numCores)
  registerDoParallel(cl)
  
  # Load necessary libraries on each worker
  clusterEvalQ(cl, library(qrng))
  
  # Function to combine results from chunks
  combineChunks <- function(...) c(...)
  
  if (RNG == "Sobol") {
    randomVariables <- foreach(i = 1:numCores, .combine = combineChunks) %dopar% {
      chunk <- t(t(sobol(chunkSize, nDim)) * (upper - lower)) + lower
      f(chunk, mean = muVector, sigma = covMatrix)
    }
  } else if (RNG == "Halton") {
    randomVariables <- foreach(i = 1:numCores, .combine = combineChunks) %dopar% {
      chunk <- t(t(ghalton(chunkSize, nDim)) * (upper - lower)) + lower
      f(chunk, mean = muVector, sigma = covMatrix)
    }
  } else {
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
  sequentialEstimateVector <- prod(upper - lower) * tail(cumulativeSum, nValues) / seq_along(tail(cumulativeSum, nValues))
  
  return(sequentialEstimateVector)
}

compareMCIntegrationMetrics <- function(f, lower, upper, muVector, covMatrix
                                        , nValues, start = 1, df = NULL){
  if (is.null(df)) {
    trueValue <- pmvnorm(lower, upper, muVector, sigma = covMatrix)
  } else {
    trueValue <- pmvt(lower, upper, muVector, sigma = covMatrix, df = df)
  }
  
  
  sobolTime <- system.time({
    sobolVector <- mcIntNDimSequential(f, lower, upper, muVector, covMatrix, 
                                       RNG = "Sobol", nValues)
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
  varianceVector <- c(Sobol = var(sobolVector[start:nValues]), Halton = var(haltonVector[start:nValues]),
                      Pseudo = var(pseudoVector[start:nValues]) )
  mseVector <-c(Sobol = mean((trueValue -sobolVector[start:nValues])^2), 
                Halton = mean((trueValue -haltonVector[start:nValues])^2),
                Pseudo = mean((trueValue -pseudoVector[start:nValues])^2) )
  calcTime <-c(Sobol = sobolTime, Halton = haltonTime, Pseudo = pseudoTime)
  
  estimateMatrix <- cbind(estimateVector, varianceVector, mseVector, calcTime)
  rownames(estimateMatrix) <- c("Sobol", "Halton", "Pseudo")
  return(estimateMatrix)
  
}