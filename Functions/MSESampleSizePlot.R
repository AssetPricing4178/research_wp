#matrices of estimates with increasing sample size, row = dimension, column sample size
trueValuesVector <- sobolMatrix[,6] # i is the dimension

# I want to calculate the MSE for each sample size for each dimension and for each type of RNG

# Function to calculate MSE
calculateMSE <- function(estimateMatrix, trueValuesVector) {
  if (is.vector(estimateMatrix)) {
    return(mean((estimateMatrix - trueValuesVector)^2))
  } else {
    return(rowMeans((estimateMatrix - trueValuesVector)^2))
  }
}

# Sample sizes and dimensions
sampleSizes <- seq(from = 1, to = ncol(sequentialSobolEstimateMatrix), by = 1)
dimensions <- seq(from = 1, to = nrow(sequentialSobolEstimateMatrix), by = 1)

# Initialize empty data frames
mseSobol <- data.frame()
msePseudo <- data.frame()
mseHalton <- data.frame()

# Calculate MSE for Sobol
for (dimension in dimensions) {
  mse <- sapply(sampleSizes, function(size) calculateMSE(sequentialSobolEstimateMatrix[dimension, 1:size], trueValuesVector[dimension]))
  mseSobol <- rbind(mseSobol, data.frame(Dimension = dimension, SampleSize = sampleSizes, MSE = mse))
}

# Calculate MSE for Pseudo
for (dimension in dimensions) {
  mse <- sapply(sampleSizes, function(size) calculateMSE(sequentialPseudoEstimateMatrix[dimension, 1:size], trueValuesVector[dimension]))
  msePseudo <- rbind(msePseudo, data.frame(Dimension = dimension, SampleSize = sampleSizes, MSE = mse))
}

# Calculate MSE for Halton
for (dimension in dimensions) {
  mse <- sapply(sampleSizes, function(size) calculateMSE(sequentialHaltonEstimateMatrix[dimension, 1:size], trueValuesVector[dimension]))
  mseHalton <- rbind(mseHalton, data.frame(Dimension = dimension, SampleSize = sampleSizes, MSE = mse))
}

# Combine all MSE data frames into one
mseData <- rbind(transform(mseSobol, RNG = "Sobol"),
                 transform(msePseudo, RNG = "Pseudo"),
                 transform(mseHalton, RNG = "Halton"))

# Create a single plot with facet_wrap
combinedPlot <- ggplot(mseData, aes(x = SampleSize, y = MSE, color = as.factor(Dimension))) +
  geom_line() +
  labs(title = "MSE vs Sample Size",
       x = "Sample Size",
       y = "MSE") +
  theme_minimal() +
  facet_wrap(~RNG)

# Show the combined plot
print(combinedPlot)

# Save the combined plot to your hard drive
ggsave("combined_plot.png", plot = combinedPlot, width = 10, height = 6, units = "in")