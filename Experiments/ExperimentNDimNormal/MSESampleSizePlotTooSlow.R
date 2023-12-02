rueValuesVector <- sobolMatrix[,6] # i is the dimensio

# Function to calculate MSE
calculateMSE <- function(estimateList, trueValuesList) {
  if (is.list(estimateList)) {
    mseList <- lapply(seq_along(estimateList), function(dim) {
      estimateVector <- estimateList[[dim]]
      trueValuesVector <- trueValuesList[[dim]]
      mseValues <- sapply(seq_along(estimateVector), function(size) {
        mean((estimateVector[1:size] - trueValuesVector[1:size])^2)
      })
      return(data.frame(Dimension = dim, SampleSize = seq_along(estimateVector), MSE = mseValues))
    })
    return(do.call(rbind, mseList))
  } else {
    stop("Input must be a list of vectors.")
  }
}

# Sample sizes and dimensions
sampleSizes <- seq(from = 1, to = length(sequentialSobolEstimateList[[1]]), by = 1)

# Calculate MSE for Sobol
mseSobol <- calculateMSE(sequentialSobolEstimateList, sobolMatrix)

# Calculate MSE for Pseudo
msePseudo <- calculateMSE(sequentialPseudoEstimateList, pseudoMatrix)

# Calculate MSE for Halton
mseHalton <- calculateMSE(sequentialHaltonEstimateList, haltonMatrix)

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
