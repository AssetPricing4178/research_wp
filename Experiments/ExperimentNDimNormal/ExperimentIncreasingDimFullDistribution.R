library(rootSolve)
library(ggplot2)
library(plyr)
library(dplyr)

source("../../Functions/ParallelProcessMCIntegration.R")

start <- 0
dimSeq <- 1:3
startingNvalues <- 100

sobolMatrix <-matrix(0,nrow = length(dimSeq), ncol = 6)
haltonMatrix <-matrix(0,nrow = length(dimSeq), ncol = 6)
pseudoMatrix <-matrix(0,nrow = length(dimSeq), ncol = 6)

# Initialize sequential estimate matrices
sequentialSobolEstimateList <- vector("list", length = length(dimSeq))
sequentialPseudoEstimateList <- vector("list", length = length(dimSeq))
sequentialHaltonEstimateList <- vector("list", length = length(dimSeq))

nValuesGraph <- c()

for (nDim in dimSeq) {
  print(paste("Dimension", nDim))
  
  lower <- rep(-5, nDim)
  upper <- rep(5, nDim)
  
  muVector <- rep(0, nDim)
  covMatrix <- diag(1, nDim)
  
  nValues <-  startingNvalues^nDim
  nValuesGraph <- append(nValuesGraph,  nValues * nDim)
  print(paste("#Generated numbers:", nValues * nDim))
  
  # Run your function
  collectionMatrix <- compareMCIntegrationMetrics(
    f = dmvnorm, lower, upper, muVector,
    covMatrix = covMatrix, nValues = nValues, start = start
  )
  
  sobolMatrix[nDim, ] <- collectionMatrix$estimateMatrix[1,]
  haltonMatrix[nDim, ] <- collectionMatrix$estimateMatrix[2,]
  pseudoMatrix[nDim, ] <- collectionMatrix$estimateMatrix[3,]
  
  # Fill sequential estimate matrices
  sequentialSobolEstimateList[[nDim]] <- collectionMatrix$sobolVector[1:nValues]
  sequentialPseudoEstimateList[[nDim]] <- collectionMatrix$pseudoVector[1:nValues]
  sequentialHaltonEstimateList[[nDim]] <- collectionMatrix$haltonVector[1:nValues]
}

# Function to pad vectors with NA to a common length
pad_with_na <- function(vec_list, common_length) {
  lapply(vec_list, function(vec) {
    if (length(vec) < common_length) {
      c(vec, rep(NA, common_length - length(vec)))
    } else {
      vec
    }
  })
}

# Determine the maximum length among all vectors
max_length <- max(sapply(sequentialSobolEstimateList, length))

# Pad vectors in the lists with NA to the maximum length
padded_sobol_list <- pad_with_na(sequentialSobolEstimateList, max_length)
padded_pseudo_list <- pad_with_na(sequentialPseudoEstimateList, max_length)
padded_halton_list <- pad_with_na(sequentialHaltonEstimateList, max_length)

# Convert lists to matrices using bind_cols
sequentialSobolEstimateMatrix <- bind_cols(lapply(padded_sobol_list, as.data.frame))
sequentialPseudoEstimateMatrix <- bind_cols(lapply(padded_pseudo_list, as.data.frame))
sequentialHaltonEstimateMatrix <- bind_cols(lapply(padded_halton_list, as.data.frame))
