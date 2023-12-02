library(qrng)
library(pryr)
library(mvtnorm)
library(foreach)
library(doParallel)

mcIntNDimSequential <- function(f, lower, upper, muVector, covMatrix, 
                                RNG, nValues, df = NULL, numCores = 2, batchSize = 100) {
  
  gc()
  nDim <- length(upper)
  
  # Register parallel backend
  cl <- makeCluster(numCores)
  registerDoParallel(cl)
  
  clusterEvalQ(cl, {
    library(qrng)
    library(mvtnorm)
  })
  
  
  # Function to combine results from batches
  combineBatches <- function(...) c(...)
  
  if (!(RNG == "Pseudo")) {
    batchSize <- min(batchSize, floor(nValues / (numCores * nDim)))
  } else {
    batchSize <- min(batchSize, floor(nValues / (numCores * nDim)))
  }
  
  # Reduce batchSize for memory efficiency
  batchSize <- max(batchSize, 100)
  
  batches <- split(1:nValues, ceiling(seq_along(1:nValues) / (batchSize * nDim)))
  
  randomVariables <- foreach(i = 1:length(batches), .combine = combineBatches) %dopar% {
    batchIndices <- batches[[i]]
    
    if (RNG == "Sobol") {
      chunk <- t(t(sobol(length(batchIndices), nDim)) * (upper - lower)) + lower
    } else if (RNG == "Halton") {
      chunk <- t(t(ghalton(length(batchIndices), nDim)) * (upper - lower)) + lower
    } else {
      chunk <- matrix(runif(length(batchIndices) * nDim, lower, upper), length(batchIndices), nDim)
    }
    
    # Importance sampling parameters
    is_mu <- muVector  # Set your importance sampling mean
    is_CovMatrix <- covMatrix  # Set your importance sampling covariance matrix
    
    # Evaluate the ratio of the target distribution to the importance sampling distribution
    pdf_target <- dmvnorm(chunk, mean = muVector, sigma = covMatrix)
    pdf_is <- dmvnorm(chunk, mean = is_mu, sigma = is_CovMatrix)
    weight <- pdf_target / pdf_is
    
    # Evaluate the integrand using the importance sampling weights
    values <- f(chunk, mean = muVector, sigma = covMatrix) * weight
    
    return(values)
  }
  
  stopCluster(cl)
  
  # Combine results from different batches
  values <- unlist(randomVariables)
  
  # Calculate the cumulative sum of the results
  cumulativeSum <- as.vector(cumsum(values))
  
  # Store the last nValues values for storage purposes
  sequentialEstimateVector <- prod(upper - lower) * cumulativeSum / seq_along(cumulativeSum)
  
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
  
  
  print("sobol")
  sobolTime <- system.time({
    sobolVector <- mcIntNDimSequential(f, lower, upper, muVector, covMatrix, 
                                       RNG = "Sobol", nValues)
  })["elapsed"]
  
  print("halton")
  haltonTime <- system.time({
    haltonVector <- mcIntNDimSequential(f, lower, upper, muVector, covMatrix, 
                                        RNG = "Halton", nValues)
  })["elapsed"]
  
  print("Pseudo")
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
  stdEstimateVector<- c(Sobol = tail(sobolVector, 1)-trueValue, Halton = tail(haltonVector, 1)-trueValue,
                        Pseudo = tail(pseudoVector, 1)-trueValue )
  trueValueVector <-rep(trueValue,3) 
  
  estimateMatrix <- cbind(estimateVector, varianceVector, mseVector,
                          calcTime, stdEstimateVector, trueValueVector)
  rownames(estimateMatrix) <- c("Sobol", "Halton", "Pseudo")
  return(estimateMatrix)
  
}