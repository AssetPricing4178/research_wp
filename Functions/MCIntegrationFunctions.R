  # Make sure to have the necessary library loaded for dmvnorm
library(qrng)
library(pryr)
library(mvtnorm)
library(foreach)
library(doParallel)

mcIntNDim <- function(f, lower, upper, muVector, covMatrix, 
                      RNG, nValues, df = NULL) {
  nValuesPseudo <- nValues
  nValues <- min(nValues, 2^31-1)# limit of qrng functions
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
    randomVariables <- t(apply(matrix(0, nValuesPseudo, nDim), 1, function(row) runif(nDim, lower, upper)))
  }
  
  
  sum <- apply(randomVariables, 1, function(row) f(row, mean = muVector, sigma = covMatrix))
  meanEstimate <- mean(sum)
  estimate <- prod(upper - lower) * meanEstimate
  return(estimate)
}

mcIntNDimSequential <- function(f, lower, upper, muVector, covMatrix, RNG, nValues, df = NULL, windowSize = 1000) {
  nValuesPseudo <- nValues
  nValues <- min(nValues, 2^31-1)
  gc()
  nDim <- length(upper)
  
  if (RNG == "Sobol") {
    randomVariables <- t(t(sobol(nValues, nDim)) * (upper - lower)) + lower
  } else if (RNG == "Halton") {
    randomVariables <- t(t(ghalton(nValues, nDim)) * (upper - lower)) + lower
  } else {
    randomVariables <- matrix(runif(nValuesPseudo * nDim, lower, upper), nValues, nDim)
  }
  
 
  
  values <- f(randomVariables, mean = muVector, sigma = covMatrix)
  
  # Calculate the cumulative sum of the results
  cumulativeSum <- as.vector(cumsum(values))
  
  # Store the last n-windowSize values for storage purposes
  sequentialEstimateVector <- prod(upper - lower) * tail(cumulativeSum, nValues) / seq_along(tail(cumulativeSum, windowSize))
  
  return(sequentialEstimateVector)
}

compareMCIntegrationMetrics <- function(f, lower, upper, muVector, covMatrix
                                        , nValues, start = 1, df = NULL){
  nValuesPseudo <- nValues
  nValues <- min(nValues, 2^31-1)
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
                                      RNG = "Pseudo", nValuesPseudo)
  })["elapsed"]
  
  
  
  
 
  estimateVector <- c(Sobol = tail(sobolVector, 1), Halton = tail(haltonVector, 1),
                     Pseudo = tail(pseudoVector, 1) )
  varianceVector <- c(Sobol = var(sobolVector[start:nValues]), Halton = var(haltonVector[start:nValues]),
                    Pseudo = var(pseudoVector[start:nValuesPseudo]) )
  mseVector <-c(Sobol = mean((trueValue -sobolVector[start:nValues])^2), 
                Halton = mean((trueValue -haltonVector[start:nValues])^2),
                Pseudo = mean((trueValue -pseudoVector[start:nValuesPseudo])^2) )
  calcTime <-c(Sobol = sobolTime, Halton = haltonTime, Pseudo = pseudoTime)
  
  estimateMatrix <- cbind(estimateVector, varianceVector, mseVector, calcTime)
  rownames(estimateMatrix) <- c("Sobol", "Halton", "Pseudo")
  
  return(estimateMatrix)
  
}


