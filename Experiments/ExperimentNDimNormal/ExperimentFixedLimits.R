source("../../Functions/ParallelProcessMCIntegration.R")
#source("../../Functions/ImportanceSampling.R")
#sourceCpp("../../Functions/C++MCInt.cpp")
#source("../../Functions/MCIntegrationFunctions.R")
library(rootSolve)
library(ggplot2)
start <- 0
dimSeq <- 1:2

sobolMatrix <-matrix(0,nrow = length(dimSeq)+1, ncol = 6)
haltonMatrix <-matrix(0,nrow = length(dimSeq)+1, ncol = 6)
pseudoMatrix <-matrix(0,nrow = length(dimSeq)+1, ncol = 6)

colnames(sobolMatrix) <- c("Estimate", "Variance", "MSE", "CalcTime","Std. Estimate", "True value")
colnames(haltonMatrix) <- c("Estimate", "Variance", "MSE", "CalcTime","Std. Estimate", "True value")
colnames(pseudoMatrix) <- c("Estimate", "Variance", "MSE", "CalcTime","Std. Estimate", "True value")

nValuesGraph <-c()

for (nDim in dimSeq){
  print(paste("Dimension", nDim))
  
  
  lower <- rep(-1.6448534751579,nDim)#95% right tail
  upper <- rep(5,nDim)
  muVector <- rep(0,nDim)
  covMatrix <- diag(1, nDim)
  nValues <- 10^nDim
  nValuesGraph <- append(nValuesGraph, nValues*nDim)
  print(paste("#Generated numbers:",nValues*nDim))
  
  # Run your function
  collectionMatrix <- compareMCIntegrationMetrics(f = dmvnorm,lower, upper, muVector, 
                                                  covMatrix = covMatrix, nValues = nValues, start = start)
  
  
  
  sobolMatrix[nDim, ] <- collectionMatrix$estimateMatrix[1,]
  haltonMatrix[nDim, ] <- collectionMatrix$estimateMatrix[2,]
  pseudoMatrix[nDim, ] <- collectionMatrix$estimateMatrix[3,]
 
  
}



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
  pseudoStdEstimate = pseudoMatrix[dimSeq, 5],
  haltonStdEstimate = haltonMatrix[dimSeq, 5],
  sobolStdEstimate = -sobolMatrix[dimSeq, 5],
  trueValue = sobolMatrix[dimSeq, 6],
  mseOverTimeSobol = sobolMatrix[dimSeq, 3] / sobolMatrix[dimSeq, 4],
  mseOverTimeHalton = haltonMatrix[dimSeq, 3] / haltonMatrix[dimSeq, 4],
  mseOverTimePseudo = pseudoMatrix[dimSeq, 3] / pseudoMatrix[dimSeq, 4]
)

pdf(file = "ExperimentNdimNormalFixedLimitsExponentialN.pdf", width = 10, height = 8)

# Generated values per dimension
ggplot(df, aes(x = dimSeq, y = nValuesGraph, color = "Generated values")) +
  geom_line() +
  labs(title = "Generated values per dimension", x = "Dimension", y = "Generated values") +
  theme_minimal()

# Log Generated values per dimension
ggplot(df, aes(x = dimSeq, y = log(nValuesGraph), color = "log Generated values")) +
  geom_line() +
  labs(title = "Generated values per dimension", x = "Dimension", y = "Generated values") +
  theme_minimal()

# Estimate by dimension
ggplot(df, aes(x = dimSeq)) +
  geom_line(aes(y = sobolEstimate, color = "Sobol"), size = 0.2) +
  geom_line(aes(y = haltonEstimate, color = "Halton"), size = 0.2) +
  geom_line(aes(y = pseudoEstimate, color = "Pseudo"), size = 0.2) +
  geom_line(aes(y = trueValue, color = "True value"), size = 0.2) +
  labs(title = "Estimate by dimension", x = "Dimension", y = "Estimate") +
  theme_minimal() +
  scale_color_manual(values = c("Sobol" = "blue", "Halton" = "green", "Pseudo" = "red", "True value" = "gray"))

# Log Estimate by dimension
ggplot(df, aes(x = dimSeq)) +
  geom_line(aes(y = log(sobolEstimate), color = "Sobol"), size = 0.2) +
  geom_line(aes(y = log(haltonEstimate), color = "Halton"), size = 0.2) +
  geom_line(aes(y = log(pseudoEstimate), color = "Pseudo"), size = 0.2) +
  geom_line(aes(y = log(trueValue), color = "True value"), size = 0.2) +
  labs(title = "log estimate by dimension", x = "Dimension", y = "log Estimate") +
  theme_minimal() +
  scale_color_manual(values = c("Sobol" = "blue", "Halton" = "green", "Pseudo" = "red", "True value" = "gray"))

# std. Estimate by dimension
ggplot(df, aes(x = dimSeq)) +
  geom_line(aes(y = sobolStdEstimate, color = "Sobol"), size = 0.2) +
  geom_line(aes(y = haltonStdEstimate, color = "Halton"), size = 0.2) +
  geom_line(aes(y = pseudoStdEstimate, color = "Pseudo"), size = 0.2) +
  geom_line(aes(y = 0, color = "True value"), size = 0.2) +
  labs(title = "Std. estimate by dimension", x = "Dimension", y = "Estimate") +
  theme_minimal() +
  scale_color_manual(values = c("Sobol" = "blue", "Halton" = "green", "Pseudo" = "red", "True value" = "gray"))

# Variance of estimate by dimension
ggplot(df, aes(x = dimSeq)) +
  geom_line(aes(y = sobolVariance, color = "Sobol"), size = 0.2) +
  geom_line(aes(y = haltonVariance, color = "Halton"), size = 0.2) +
  geom_line(aes(y = pseudoVariance, color = "Pseudo"), size = 0.2) +
  labs(title = "Variance of estimate by dimension", x = "Dimension", y = "Variance") +
  theme_minimal() +
  scale_color_manual(values = c("Sobol" = "blue", "Halton" = "green", "Pseudo" = "red"))

# Log Variance of estimate by dimension
ggplot(df, aes(x = dimSeq)) +
  geom_line(aes(y = log(sobolVariance), color = "Sobol"), size = 0.2) +
  geom_line(aes(y = log(haltonVariance), color = "Halton"), size = 0.2) +
  geom_line(aes(y = log(pseudoVariance), color = "Pseudo"), size = 0.2) +
  labs(title = "log Variance of estimate by dimension", x = "Dimension", y = "log Variance") +
  theme_minimal() +
  scale_color_manual(values = c("Sobol" = "blue", "Halton" = "green", "Pseudo" = "red"))

# MSE of estimate by dimension
ggplot(df, aes(x = dimSeq)) +
  geom_line(aes(y = sobolMSE, color = "Sobol"), size = 0.2) +
  geom_line(aes(y = haltonMSE, color = "Halton"), size = 0.2) +
  geom_line(aes(y = pseudoMSE, color = "Pseudo"), size = 0.2) +
  labs(title = "MSE of estimate by dimension", x = "Dimension", y = "MSE") +
  theme_minimal() +
  scale_color_manual(values = c("Sobol" = "blue", "Halton" = "green", "Pseudo" = "red"))

# Log MSE of estimate by dimension
ggplot(df, aes(x = dimSeq)) +
  geom_line(aes(y = log(sobolMSE), color = "Sobol"), size = 0.2) +
  geom_line(aes(y = log(haltonMSE), color = "Halton"), size = 0.2) +
  geom_line(aes(y = log(pseudoMSE), color = "Pseudo"), size = 0.2) +
  labs(title = "log MSE of estimate by dimension", x = "Dimension", y = "log MSE") +
  theme_minimal() +
  scale_color_manual(values = c("Sobol" = "blue", "Halton" = "green", "Pseudo" = "red"))

# Calculation time of estimate by dimension
ggplot(df, aes(x = dimSeq)) +
  geom_line(aes(y = sobolTime, color = "Sobol"), size = 0.2) +
  geom_line(aes(y = haltonTime, color = "Halton"), size = 0.2) +
  geom_line(aes(y = pseudoTime, color = "Pseudo"), size = 0.2) +
  labs(title = "Calculation time of estimate by dimension", x = "Dimension", y = "Calculation time") +
  theme_minimal() +
  scale_color_manual(values = c("Sobol" = "blue", "Halton" = "green", "Pseudo" = "red"))

# MSE/Calculation time
ggplot(df, aes(x = dimSeq)) +
  geom_line(aes(y = mseOverTimeSobol, color = "Sobol"), size = 0.2) +
  geom_line(aes(y = mseOverTimeHalton, color = "Halton"), size = 0.2) +
  geom_line(aes(y = mseOverTimePseudo, color = "Pseudo"), size = 0.2) +
  labs(title = "MSE/Calculation time", x = "Dimension", y = "MSE/Calculation time") +
  theme_minimal() +
  scale_color_manual(values = c("Sobol" = "blue", "Halton" = "green", "Pseudo" = "red"))

# MSE/Calculation time
ggplot(df, aes(x = dimSeq)) +
  geom_line(aes(y = log(mseOverTimeSobol), color = "Sobol"), size = 0.2) +
  geom_line(aes(y = log(mseOverTimeHalton), color = "Halton"), size = 0.2) +
  geom_line(aes(y = log(mseOverTimePseudo), color = "Pseudo"), size = 0.2) +
  labs(title = "log MSE/Calculation time", x = "Dimension", y = "log(MSE/Calculation time)") +
  theme_minimal() +
  scale_color_manual(values = c("Sobol" = "blue", "Halton" = "green", "Pseudo" = "red"))

# Close the PDF device
dev.off()
