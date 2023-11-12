library(matlib)

#ndim normal dist wikipedia multivariate normal distribution
nDimNormal <- function( vectorVariables, nDim, muVector = rep(0, nDim), covMatrix){
  term1 <- exp(-0.5 * t(vectorVariables-muVector)%*%inv(covMatrix) %*% (vectorVariables-muVector)) #stämmer detta? * eller%*%
  term2 <- ((2*pi)^nDim * det(covMatrix))^0.5
  return(term1/term2)
}

#nåt knasigt med denna
nDimTStudent <- function(vectorVariables, nDim, muVector = NULL, df, covMatrix = NULL){
  term1 <-gamma((df+nDim)/2)
  term2 <-gamma(df/2)*df^(nDim/2)*det(covMatrix)^(0.5)
  term3 <-(1 + 1/df*t(vectorVariables-muVector) %*% inv(covMatrix) %*% (vectorVariables-muVector))^-(df+nDim)/2
  return(term1/term2*term3)
}