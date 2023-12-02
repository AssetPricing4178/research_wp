source("../../Functions/ParallelProcessMCIntegration.R")
#source("../../Functions/MCIntegrationFunctions.R")
#source("../../Functions/SegmentedMCIntegrationFunctions.R")
library(rootSolve)
start <- 0
dimSeq <- 1:4

sobolMatrix <-matrix(0,nrow = length(dimSeq)+1, ncol = 6)
haltonMatrix <-matrix(0,nrow = length(dimSeq)+1, ncol = 6)
pseudoMatrix <-matrix(0,nrow = length(dimSeq)+1, ncol = 6)

colnames(sobolMatrix) <- c("Estimate", "Variance", "MSE", "CalcTime","Std. Estimate", "True value")
colnames(haltonMatrix) <- c("Estimate", "Variance", "MSE", "CalcTime","Std. Estimate", "True value")
colnames(pseudoMatrix) <- c("Estimate", "Variance", "MSE", "CalcTime","Std. Estimate", "True value")

nValuesGraph <-c()
lowerZ <- uniroot(f = function(x) x-0.05, interval = c(0, 1))$root
lastDistance <- abs(-qnorm(lowerZ)-5)
for (nDim in dimSeq){
print(paste("Dimension", nDim))

  
lowerZ <- uniroot(f = function(x) x^nDim -0.05, interval = c(0, 1))$root
lowerX <- -qnorm(lowerZ)
lower <- rep(lowerX,nDim)
upper <- rep(5,nDim)
distance <- abs(lowerX-5)
#print(paste(lowerX,"-",5,"=", distance))
#print(lastDistance)
#print(distance)

muVector <- rep(0,nDim)
covMatrix <- diag(1, nDim)
compensationFactor <- distance/lastDistance
print(paste("Compensation factor:",compensationFactor))
nValues <- round((compensationFactor * 70)^nDim)
#nValues <- round(compensationFactor*1000)

nValuesGraph <- append(nValuesGraph, compensationFactor*nValues*nDim)
print(paste("#Generated numbers:",nValues*nDim))

# Run your function
collectionMatrix <- compareMCIntegrationMetrics(f = dmvnorm,lower, upper, muVector, 
                                                covMatrix = covMatrix, nValues = nValues, start = start)



sobolMatrix[nDim, ] <- collectionMatrix[1, ]
#sobolMatrix[, 5] <- apply(sobolMatrix, 1, function(row) 0.05^row - row[1])
sobolMatrix[, 5] <- sobolMatrix[,6] - sobolMatrix[,1]

haltonMatrix[nDim, ] <- collectionMatrix[2, ]
#haltonMatrix[, 5] <- apply(haltonMatrix, 1, function(row) 0.05^row - row[1])
haltonMatrix[,5] <- haltonMatrix[,6] - haltonMatrix[,1]


pseudoMatrix[nDim, ] <- collectionMatrix[3, ]
#pseudoMatrix[, 5] <- apply(pseudoMatrix, 1, function(row) 0.05^row - row[1])
pseudoMatrix[, 5] <- pseudoMatrix[,6] - pseudoMatrix[,1]

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
  sobolStdEstimate = sobolMatrix[dimSeq, 5],
  trueValue = sobolMatrix[dimSeq, 6],
  mseOverTimeSobol = sobolMatrix[dimSeq, 3] / sobolMatrix[dimSeq, 4],
  mseOverTimeHalton = haltonMatrix[dimSeq, 3] / haltonMatrix[dimSeq, 4],
  mseOverTimePseudo = pseudoMatrix[dimSeq, 3] / pseudoMatrix[dimSeq, 4]
)

pdf(file = "ExperimentNdimNormalArea005CompensationFactorExponential.pdf", width = 10, height = 8)
# Generated values per dimension
ggplot(df, aes(x = dimSeq, y = nValuesGraph, color = "Generated values")) +
  geom_line() +
  #geom_line(aes(y = 2^31-1, color = "Limit of qrng"), size = 0.2)+
  labs(title = "Generated values per dimension", x = "Dimension", y = "Generated values") +
  theme_minimal()

ggplot(df, aes(x = dimSeq, y = log(nValuesGraph), color = "log Generated values")) +
  geom_line() +
  #geom_line(aes(y = log(2^31-1), color = "Limit of qrng"), size = 0.2)+
  labs(title = "Generated values per dimension", x = "Dimension", y = "Generated values") +
  theme_minimal()

# Estimate by dimension
ggplot(df, aes(x = dimSeq)) +
  geom_line(aes(y = sobolEstimate, color = "Sobol"), size = 0.2) +
  geom_line(aes(y = haltonEstimate, color = "Halton"), size = 0.2) +
  geom_line(aes(y = pseudoEstimate, color = "Pseudo"), size = 0.2) +
  geom_line(aes(y = 0.05, color = "True value"), size = 0.2) +
  labs(title = "Estimate by dimension", x = "Dimension", y = "Estimate") +
  theme_minimal() +
  scale_color_manual(values = c("Sobol" = "blue", "Halton" = "green", "Pseudo" = "red",
                                "True value" = "gray"))

# log Estimate by dimension
ggplot(df, aes(x = dimSeq)) +
  geom_line(aes(y = log(sobolEstimate), color = "Sobol"), size = 0.2) +
  geom_line(aes(y = log(haltonEstimate), color = "Halton"), size = 0.2) +
  geom_line(aes(y = log(pseudoEstimate), color = "Pseudo"), size = 0.2) +
  geom_line(aes(y = log(0.05), color = "True value"), size = 0.2) +
  labs(title = "log Estimate by dimension", x = "Dimension", y = "log Estimate") +
  theme_minimal() +
  scale_color_manual(values = c("Sobol" = "blue", "Halton" = "green", "Pseudo" = "red",
                                "True value" = "gray"))

# Variance of estimate by dimension
ggplot(df, aes(x = dimSeq)) +
  geom_line(aes(y = sobolVariance, color = "Sobol"), size = 0.2) +
  geom_line(aes(y = haltonVariance, color = "Halton"), size = 0.2) +
  geom_line(aes(y = pseudoVariance, color = "Pseudo"), size = 0.2) +
  labs(title = "Variance of estimate by dimension", x = "Dimension", y = "Variance") +
  theme_minimal() +
  scale_color_manual(values = c("Sobol" = "blue", "Halton" = "green", "Pseudo" = "red"))

# log Variance of estimate by dimension
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

# logMSE of estimate by dimension
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
  geom_line(aes(y =log(mseOverTimePseudo), color = "Pseudo"), size = 0.2) +
  labs(title = "log MSE/Calculation time", x = "Dimension", y = "log(MSE/Calculation time)") +
  theme_minimal() +
  scale_color_manual(values = c("Sobol" = "blue", "Halton" = "green", "Pseudo" = "red"))
dev.off()