library(matlib)
library(mvtnorm)

generateCovarianceMatrix <- function(correlationVector) {
  n <- length(correlationVector) + 1
  
  covMatrix <- matrix(1, n, n)  # Initialize as an n x n matrix
  
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      covMatrix[i, j] <- correlationVector[j - i]
      covMatrix[j, i] <- correlationVector[j - i]
    }
  }
  
  # Extract the first 3x3 submatrix
  covMatrix <- covMatrix[1:(n-1), 1:(n-1)]
  
  return(covMatrix)
}

nDimNormal <-function(vectorVariables, muVector, covMatrix){
  return(dmvnorm(vectorVariables, muVector, covMatrix))
}

nDimTstudent <-function(vectorVariables, muVector, covMatrix, df){
  return(dmvt(vectorVariables, muVector, covMatrix, df, log = FALSE))
}