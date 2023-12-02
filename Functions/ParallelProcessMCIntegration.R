library(qrng)
library(pryr)
library(mvtnorm)
library(foreach)
library(doParallel)

mcIntNDimSequential <- function(f, lower, upper, muVector, covMatrix, 
                                RNG, nValues, df = NULL, numBatches = 100) {
  
  nValues <- min(nValues, 2^31-1)
  gc()
  nDim <- length(upper)
  numBatches <- ceiling(nValues/50000)
  
  # Determine the number of values per batch
  batchSize <- ceiling(nValues / numBatches)
  
  ## Create a list of batches directly
  batches <- list()
  for (i in seq_len(numBatches)) {
    startIdx <- (i - 1) * batchSize + 1
    endIdx <- min(i * batchSize, nValues)
    batches[[i]] <- startIdx:endIdx
  }
  
  # Initialize a vector to store the results
  result_vector <- numeric(0)
  
  # Process batches sequentially
  for (i in seq_along(batches)) {
    batchIndices <- batches[[i]]
    
    if (RNG == "Sobol") {
      chunk <- t(t(sobol(length(batchIndices), nDim)) * (upper - lower)) + lower
      result_vector <- c(result_vector, f(chunk, mean = muVector, sigma = covMatrix))
    } else if (RNG == "Halton") {
      chunk <- t(t(ghalton(length(batchIndices), nDim)) * (upper - lower)) + lower
      result_vector <- c(result_vector, f(chunk, mean = muVector, sigma = covMatrix))
    } else {
      chunk <- matrix(runif(length(batchIndices) * nDim, lower, upper), length(batchIndices), nDim)
      result_vector <- c(result_vector, f(chunk, mean = muVector, sigma = covMatrix))
    }
    
    print(paste(RNG,",",nDim,",Processed batch", i, "of", length(batches)))
  }
  
  # Combine results from different batches
  cumulativeSum <- cumsum(result_vector)
  
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
                                        RNG = "Pseudo", nValuesPseudo)
  })["elapsed"]
  
  #MSE and Varince vectors turn to NAs
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
  
  output <-list(
    estimateMatrix = estimateMatrix,
    sobolVector = sobolVector,
    pseudoVector = pseudoVector
  )
  return(output)
  
}