source("MCIntegrationFunctions.R")
source("ProbabilityDensityFunctions.R")
library(mvtnorm)


nvalues = 1000
lower <-c(0,0)
upper <-c(1,1)
muvector <-c(0,0)
covmatrix <- diag(1,2) #stämmer detta?
ndim <-2
df <- 1

a <- mcIntNDim(nvalues, nDim = 2, fDim = nDimTStudent, vectorLowerLimit = lower, vectorUpperLimit = upper, 
          RNG = "Quasi",covMatrix = covmatrix, df = df, muVector = muvector )
print(a)

pmvt(lower, upper,)