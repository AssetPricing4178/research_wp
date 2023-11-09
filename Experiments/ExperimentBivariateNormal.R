source("MCIntegrationFunctions.R")
library(mvtnorm)

#Bivariate NF 
bivNormal <- function(x, y, mu_x = 0, mu_y = 0, sigma_x = 1, sigma_y = 1, corr = 0){
  
  term1 <- 1 / (2 * pi * sigma_x * sigma_y * sqrt((1-corr^2)))
  term2 <- (-1/(2*(1-corr^2)) * 
              (( (x-mu_x)^2/(sigma_x)^2 -2*corr*(x-mu_x)/(sigma_x) * (y-mu_y)/(sigma_y) +(y-mu_y)^2/(sigma_y)^2 )))
  return(term1 * exp(term2))
}

nseries <- 2
nvalues <- 1000
nDim <- 2
vectorLowerLimit <- c(-1, -1)
vectorUpperLimit <- c(1, 1)
vectorRng <- c("Quasi", "Pseudo")

compareMcIntegration(nseries,nvalues,nDim,fDim = bivNormal, vectorLowerLimit, vectorUpperLimit, vectorRng)

trueValue <- pmvnorm(vectorLowerLimit, vectorUpperLimit, sigma = cbind(c(1,0),c(0,1)))
print(paste("True value:", trueValue))