source("../../Functions/ParallelProcessMCIntegration.R")
library(rootSolve)
start <- 100
dimSeq <- 6:10

sobolMatrix <-matrix(0,nrow = length(dimSeq)+1, ncol = 4)
haltonMatrix <-matrix(0,nrow = length(dimSeq)+1, ncol = 4)
pseudoMatrix <-matrix(0,nrow = length(dimSeq)+1, ncol = 4)

colnames(sobolMatrix) <- c("Estimate", "Variance", "MSE", "CalcTime")
colnames(haltonMatrix) <- c("Estimate", "Variance", "MSE", "CalcTime")
colnames(pseudoMatrix) <- c("Estimate", "Variance", "MSE", "CalcTime")

nValuesGraph <-c()

for (nDim in dimSeq){
print(paste("Dimension", nDim))

lowerZ <- uniroot(f = function(x) x^nDim -0.05, interval = c(0, 1))$root
lowerX <- -qnorm(lowerZ)
lower <- rep(lowerX,nDim)
upper <- rep(5,nDim)
muVector <- rep(0,nDim)
covMatrix <- diag(1, nDim)
nValues <- 32^(nDim)
nValuesGraph <- append(nValuesGraph, nValues)
print(paste("#Generated numbers:",nValues*nDim))


# Run your function
collectionMatrix <- compareMCIntegrationMetrics(f = dmvnorm,lower, upper, muVector, 
                                                covMatrix = covMatrix, nValues = nValues, start = start)



sobolMatrix[nDim, ] <- collectionMatrix[1, ]
haltonMatrix[nDim, ] <- collectionMatrix[2, ]
pseudoMatrix[nDim, ] <- collectionMatrix[3, ]

}

library(ggplot2)

# Combine the data into a data frame
df <- data.frame(
  dimSeq = dimSeq,
  nValuesGraph = nValuesGraph,
  sobolEstimate = sobolMatrix[dimSeq, 1],
  haltonEstimate = haltonMatrix[dimSeq, 1],
  pseudoEstimate = pseudoMatrix[dimSeq, 1],
  sobolVariance = sobolMatrix[dimSeq, 2],
  haltonVariance = haltonMatrix[dimSeq, 2],
  pseudoVariance = pseudoMatrix[dimSeq, 2],
  sobolMSE = sobolMatrix[dimSeq, 3],
  haltonMSE = haltonMatrix[dimSeq, 3],
  pseudoMSE = pseudoMatrix[dimSeq, 3],
  sobolTime = sobolMatrix[dimSeq, 4],
  haltonTime = haltonMatrix[dimSeq, 4],
  pseudoTime = pseudoMatrix[dimSeq, 4],
  mseOverTimeSobol = sobolMatrix[dimSeq, 3] / sobolMatrix[dimSeq, 4],
  mseOverTimeHalton = haltonMatrix[dimSeq, 3] / haltonMatrix[dimSeq, 4],
  mseOverTimePseudo = pseudoMatrix[dimSeq, 3] / pseudoMatrix[dimSeq, 4]
)

# Generated values per dimension
ggplot(df, aes(x = dimSeq, y = nValuesGraph, color = "Generated values per dimension")) +
  geom_line() +
  labs(title = "Generated values per dimension", x = "Dimension", y = "Generated values") +
  theme_minimal()

# Estimate by dimension
ggplot(df, aes(x = dimSeq)) +
  geom_line(aes(y = sobolEstimate, color = "Sobol"), size = 1) +
  geom_line(aes(y = haltonEstimate, color = "Halton"), size = 1) +
  geom_line(aes(y = pseudoEstimate, color = "Pseudo"), size = 1) +
  labs(title = "Estimate by dimension", x = "Dimension", y = "Estimate") +
  theme_minimal() +
  scale_color_manual(values = c("Sobol" = "blue", "Halton" = "green", "Pseudo" = "red"))

# Variance of estimate by dimension
ggplot(df, aes(x = dimSeq)) +
  geom_line(aes(y = sobolVariance, color = "Sobol"), size = 1) +
  geom_line(aes(y = haltonVariance, color = "Halton"), size = 1) +
  geom_line(aes(y = pseudoVariance, color = "Pseudo"), size = 1) +
  labs(title = "Variance of estimate by dimension", x = "Dimension", y = "Variance") +
  theme_minimal() +
  scale_color_manual(values = c("Sobol" = "blue", "Halton" = "green", "Pseudo" = "red"))

# MSE of estimate by dimension
ggplot(df, aes(x = dimSeq)) +
  geom_line(aes(y = sobolMSE, color = "Sobol"), size = 1) +
  geom_line(aes(y = haltonMSE, color = "Halton"), size = 1) +
  geom_line(aes(y = pseudoMSE, color = "Pseudo"), size = 1) +
  labs(title = "MSE of estimate by dimension", x = "Dimension", y = "MSE") +
  theme_minimal() +
  scale_color_manual(values = c("Sobol" = "blue", "Halton" = "green", "Pseudo" = "red"))

# Calculation time of estimate by dimension
ggplot(df, aes(x = dimSeq)) +
  geom_line(aes(y = sobolTime, color = "Sobol"), size = 1) +
  geom_line(aes(y = haltonTime, color = "Halton"), size = 1) +
  geom_line(aes(y = pseudoTime, color = "Pseudo"), size = 1) +
  labs(title = "Calculation time of estimate by dimension", x = "Dimension", y = "Calculation time") +
  theme_minimal() +
  scale_color_manual(values = c("Sobol" = "blue", "Halton" = "green", "Pseudo" = "red"))

# MSE/Calculation time
ggplot(df, aes(x = dimSeq)) +
  geom_line(aes(y = mseOverTimeSobol, color = "Sobol"), size = 1) +
  geom_line(aes(y = mseOverTimeHalton, color = "Halton"), size = 1) +
  geom_line(aes(y = mseOverTimePseudo, color = "Pseudo"), size = 1) +
  labs(title = "MSE/Calculation time", x = "Dimension", y = "MSE/Calculation time") +
  theme_minimal() +
  scale_color_manual(values = c("Sobol" = "blue", "Halton" = "green", "Pseudo" = "red"))

# MSE/Calculation time
ggplot(df, aes(x = dimSeq)) +
  geom_line(aes(y = log(mseOverTimeSobol), color = "Sobol"), size = 1) +
  geom_line(aes(y = log(mseOverTimeHalton), color = "Halton"), size = 1) +
  geom_line(aes(y =log(mseOverTimePseudo), color = "Pseudo"), size = 1) +
  labs(title = "MSE/Calculation time", x = "Dimension", y = "log(MSE/Calculation time)") +
  theme_minimal() +
  scale_color_manual(values = c("Sobol" = "blue", "Halton" = "green", "Pseudo" = "red"))
