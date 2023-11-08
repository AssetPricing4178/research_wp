source("MCIntegrationFunctions.R")

#Bivariate NF no covariance
bivNF <- function(x, y, mu_x = 0, mu_y = 0, sigma_x = 1, sigma_y = 1){
  
  term1 <- 1 / (2 * pi * sigma_x * sigma_y)
  term2 <- -((x - mu_x)^2 / (2 * sigma_x^2) + (y - mu_y)^2 / (2 * sigma_y^2))
  return(term1 * exp(term2))
}

nseries <- 2
nvalues <- 1000
nDim <- 2
vectorLowerLimit <- c(-1, -1)
vectorUpperLimit <- c(1, 1)
vectorRng <- c("Quasi", "Pseudo")

compareMcIntegration(nseries,nvalues,nDim,fDim = bivNF, vectorLowerLimit, vectorUpperLimit, vectorRng)