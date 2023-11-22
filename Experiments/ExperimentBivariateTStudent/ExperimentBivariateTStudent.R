source("../../Functions/ProbabilityDensityFunctions.R")
source("../../Functions/MCIntegrationFunctions.R")
library(mvtnorm)


nvalues = 10000
lower <-c(-10,-10)
upper <-c(10,10)
muvector <-c(0,0)
covmatrix <- diag(1,2) 
ndim <-2
df <- 1

#a <- mcIntNDim(nvalues, nDim = 2, fDim = nDimTStudent, vectorLowerLimit = lower, vectorUpperLimit = upper, 
#          RNG = "Quasi",covMatrix = covmatrix, df = df, muVector = muvector)
#print(a)

#trueValue <-pmvt(lower, upper, sigma = covmatrix)
#print(trueValue)

compareMcIntegrationGraph(nSeries = 2,nDim = 2, nValues = nvalues, fDim = nDimTStudent,
                          vectorLowerLimit = lower, vectorUpperLimit = upper, 
                        vectorRNG = c("Quasi", "Pseudo"),covMatrix = covmatrix, df = df,
                          typeOFDist = "T", muVector = muvector, start = 100)