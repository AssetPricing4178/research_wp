source("MCIntegrationFunctions.R")
source("ProbabilityDensityFunctions.R")
library(mvtnorm)

nseries <- 2
nvalues <- 1000
nDim <- 2
vectorLowerLimit <- c(-1, -1)
vectorUpperLimit <- c(1, 1)
vectorRng <- c("Quasi", "Pseudo")
muVector <- c(0,0)

pdf(file = "ExperimentBivariateNormalResults.pdf")

# No Correlation
print("No correlation")
covMatrix <- cbind(c(1,0),c(0,1))
compareMcIntegration(nseries,nvalues,nDim,fDim = nDimNormal, 
                     vectorLowerLimit, vectorUpperLimit, 
                     vectorRng, covMatrix = covMatrix, 
                     muVector = muVector, main = "No correlation")
trueValue <- pmvnorm(vectorLowerLimit, vectorUpperLimit, sigma = covMatrix)
print(paste("True value:", trueValue))

# Small correlation
print("Small correlation")
corr = 0.1
covMatrix <- cbind(c(1,corr),c(corr,1))
compareMcIntegration(nseries,nvalues,nDim,fDim = nDimNormal, 
                     vectorLowerLimit, vectorUpperLimit, 
                     vectorRng, covMatrix = covMatrix, 
                     muVector = muVector, main = "Small correlation")
trueValue <- pmvnorm(vectorLowerLimit, vectorUpperLimit, sigma = covMatrix)
print(paste("True value:", trueValue))

# medium correlation
print("Medium correlation")
corr = 0.5
covMatrix <- cbind(c(1,corr),c(corr,1))
compareMcIntegration(nseries,nvalues,nDim,fDim = nDimNormal, 
                     vectorLowerLimit, vectorUpperLimit, 
                     vectorRng, covMatrix = covMatrix, 
                     muVector = muVector, main = "Medium correlation")
trueValue <- pmvnorm(vectorLowerLimit, vectorUpperLimit, sigma = covMatrix)
print(paste("True value:", trueValue))

# High correlation
print("High correlation")
corr = 0.9
covMatrix <- cbind(c(1,corr),c(corr,1))
compareMcIntegration(nseries,nvalues,nDim,fDim = nDimNormal, 
                     vectorLowerLimit, vectorUpperLimit, 
                     vectorRng, covMatrix = covMatrix, 
                     muVector = muVector, main = "High correlation")
trueValue <- pmvnorm(vectorLowerLimit, vectorUpperLimit, sigma = covMatrix)
print(paste("True value:", trueValue))

dev.off()