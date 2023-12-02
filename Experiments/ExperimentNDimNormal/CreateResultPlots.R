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

# Generated values per dimension
p1 <- ggplot(df, aes(x = dimSeq, y = nValuesGraph, color = "Generated values")) +
  geom_line() +
  labs(title = "Generated values per dimension", x = "Dimension", y = "Generated values") +
  theme_minimal()

ggsave("GeneratedValuesPerDimension.pdf", plot = p1, width = 10, height = 8)

# log Generated values per dimension
p2 <- ggplot(df, aes(x = dimSeq, y = log(nValuesGraph), color = "log Generated values")) +
  geom_line() +
  labs(title = "Generated values per dimension", x = "Dimension", y = "Generated values") +
  theme_minimal()

ggsave("LogGeneratedValuesPerDimension.pdf", plot = p2, width = 10, height = 8)

# Estimate by dimension
p3 <- ggplot(df, aes(x = dimSeq)) +
  geom_line(aes(y = sobolEstimate, color = "Sobol"), size = 0.2) +
  geom_line(aes(y = haltonEstimate, color = "Halton"), size = 0.2) +
  geom_line(aes(y = pseudoEstimate, color = "Pseudo"), size = 0.2) +
  geom_line(aes(y = 0.05, color = "True value"), size = 0.2) +
  labs(title = "Estimate by dimension", x = "Dimension", y = "Estimate") +
  theme_minimal() +
  scale_color_manual(values = c("Sobol" = "blue", "Halton" = "green", "Pseudo" = "red"))

ggsave("EstimateByDimension.pdf", plot = p3, width = 10, height = 8)

# log Estimate by dimension
p4 <- ggplot(df, aes(x = dimSeq)) +
  geom_line(aes(y = log(sobolEstimate), color = "Sobol"), size = 0.2) +
  geom_line(aes(y = log(haltonEstimate), color = "Halton"), size = 0.2) +
  geom_line(aes(y = log(pseudoEstimate), color = "Pseudo"), size = 0.2) +
  geom_line(aes(y = log(0.05), color = "True value"), size = 0.2) +
  labs(title = "log Estimate by dimension", x = "Dimension", y = "log Estimate") +
  theme_minimal() +
  scale_color_manual(values = c("Sobol" = "blue", "Halton" = "green", "Pseudo" = "red"))

ggsave("LogEstimateByDimension.pdf", plot = p4, width = 10, height = 8)

# Variance of estimate by dimension
p5 <- ggplot(df, aes(x = dimSeq)) +
  geom_line(aes(y = sobolVariance, color = "Sobol"), size = 0.2) +
  geom_line(aes(y = haltonVariance, color = "Halton"), size = 0.2) +
  geom_line(aes(y = pseudoVariance, color = "Pseudo"), size = 0.2) +
  labs(title = "Variance of estimate by dimension", x = "Dimension", y = "Variance") +
  theme_minimal() +
  scale_color_manual(values = c("Sobol" = "blue", "Halton" = "green", "Pseudo" = "red"))

ggsave("VarianceOfEstimateByDimension.pdf", plot = p5, width = 10, height = 8)

# log Variance of estimate by dimension
p6 <- ggplot(df, aes(x = dimSeq)) +
  geom_line(aes(y = log(sobolVariance), color = "Sobol"), size = 0.2) +
  geom_line(aes(y = log(haltonVariance), color = "Halton"), size = 0.2) +
  geom_line(aes(y = log(pseudoVariance), color = "Pseudo"), size = 0.2) +
  labs(title = "log Variance of estimate by dimension", x = "Dimension", y = "log Variance") +
  theme_minimal() +
  scale_color_manual(values = c("Sobol" = "blue", "Halton" = "green", "Pseudo" = "red"))

ggsave("LogVarianceOfEstimateByDimension.pdf", plot = p6, width = 10, height = 8)

# MSE of estimate by dimension
p7 <- ggplot(df, aes(x = dimSeq)) +
  geom_line(aes(y = sobolMSE, color = "Sobol"), size = 0.2) +
  geom_line(aes(y = haltonMSE, color = "Halton"), size = 0.2) +
  geom_line(aes(y = pseudoMSE, color = "Pseudo"), size = 0.2) +
  labs(title = "MSE of estimate by dimension", x = "Dimension", y = "MSE") +
  theme_minimal() +
  scale_color_manual(values = c("Sobol" = "blue", "Halton" = "green", "Pseudo" = "red"))

ggsave("MSEOfEstimateByDimension.pdf", plot = p7, width = 10, height = 8)

# logMSE of estimate by dimension
p8 <- ggplot(df, aes(x = dimSeq)) +
  geom_line(aes(y = log(sobolMSE), color = "Sobol"), size = 0.2) +
  geom_line(aes(y = log(haltonMSE), color = "Halton"), size = 0.2) +
  geom_line(aes(y = log(pseudoMSE), color = "Pseudo"), size = 0.2) +
  labs(title = "log MSE of estimate by dimension", x = "Dimension", y = "log MSE") +
  theme_minimal() +
  scale_color_manual(values = c("Sobol" = "blue", "Halton" = "green", "Pseudo" = "red"))

ggsave("LogMSEOfEstimateByDimension.pdf", plot = p8, width = 10, height = 8)

# Calculation time of estimate by dimension
p9 <- ggplot(df, aes(x = dimSeq)) +
  geom_line(aes(y = sobolTime, color = "Sobol"), size = 0.2) +
  geom_line(aes(y = haltonTime, color = "Halton"), size = 0.2) +
  geom_line(aes(y = pseudoTime, color = "Pseudo"), size = 0.2) +
  labs(title = "Calculation time of estimate by dimension", x = "Dimension", y = "Calculation time") +
  theme_minimal() +
  scale_color
