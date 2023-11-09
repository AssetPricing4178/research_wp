library(matlib)

#ndim normal dist wikipedia multivariate normal distribution
nDimNormal <- function( vectorVariables, muVector, covMatrix, nDim){
  term1 <- exp(-0.5 * t(vectorVariables-muVector)%*%inv(covMatrix) %*% (vectorVariables-muVector)) #stämmer detta? * eller%*%
  term2 <- ((2*pi)^nDim * det(covMatrix))^0.5
  return(term1/term2)
}