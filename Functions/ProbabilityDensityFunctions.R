library(matlib)

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

#ndim normal dist wikipedia multivariate normal distribution
nDimNormal <- function( vectorVariables, nDim, muVector, covMatrix, calcEX =  FALSE, calcVX = FALSE){
  
  #if(calcEX){
  #  term1 <- exp(-0.5 * t(vectorVariables-muVector) %*% inv(covMatrix) %*% (vectorVariables-muVector)) 
  #  term2 <- ((2*pi)^nDim * det(covMatrix))^0.5
  #  term3 <- vectorVariables
  #  EX <- vectorVariables * term1/term2
  #  print(dim(EX))
  #  print(EX)
  #  return(EX)
  #}
  
  #else if(calcVX){
  #  term1 <- exp(-0.5 * t(vectorVariables-muVector) %*% inv(covMatrix) %*% (vectorVariables-muVector)) 
  #  term2 <- ((2*pi)^nDim * det(covMatrix))^0.5
  #  term3 <- vectorVariables
  #  EX <- term3 * term1/term2
  #  EX2 <- term3^2*term1/term2
  #  return(EX2 -EX^2)
  #}
  #
  #else{
    term1 <- exp(-0.5 * t(vectorVariables-muVector) %*% inv(covMatrix) %*% (vectorVariables-muVector)) 
    term2 <- ((2*pi)^nDim * det(covMatrix))^0.5
    return(term1/term2)
  #}
}

#nåt knasigt med denna
nDimTStudent <- function(vectorVariables, nDim, muVector = NULL, df, covMatrix = NULL){
  term1 <-gamma((df+nDim)/2)
  #print(term1)
  term2 <-gamma(df/2)*(df^(nDim/2))*(pi^(nDim/2))*sqrt(det(covMatrix))
  #print(term2)
  term3 <-(1 + 1/df*t(vectorVariables-muVector) %*% inv(covMatrix) %*% (vectorVariables-muVector))^(-(df+nDim)/2)
  #print(term3)
  return(term1/term2*term3)
}


