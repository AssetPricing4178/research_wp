source("MCIntegrationFunctions.R")
source("ProbabilityDensityFunctions.R")
library(mvtnorm)

nseries <- 2
nvalues <- 10000
nDim <- 2
vectorLowerLimit <- c(-10, -10)
vectorUpperLimit <- c(10, 10)
vectorRng <- c("Quasi", "Pseudo")
muVector <- c(0,0)
corr <- 0
covMatrix <- cbind(c(1,corr),c(corr,1))
start = 50

resultsMatrix <-compareMcIntegrationMetrics(nSeries = 2, nvalues, 2, fDim = nDimNormal, 
                            vectorLowerLimit, vectorUpperLimit, vectorRNG = c("Quasi", "Pseudo"),
                            covMatrix = covMatrix, muVector = muVector, start = start)
compareMcIntegrationGraph(nSeries = 2, nvalues, 2, fDim = nDimNormal, 
                          vectorLowerLimit, vectorUpperLimit, vectorRNG = c("Quasi", "Pseudo"),
                          covMatrix = covMatrix, muVector = muVector, start = start)
