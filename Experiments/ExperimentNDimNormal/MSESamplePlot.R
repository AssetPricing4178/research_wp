library(reshape2)
library(ggplot2)

# Define the true values
trueValue <- rep(1, nrow(sequentialHaltonEstimateMatrix))

# Function to calculate cumulative sum of squared differences
cumulativeSquaredDiff <- function(column) {
  cumulativeSum <- cumsum((column - trueValue[seq_along(column)])^2)
  return(cumulativeSum / seq_along(column))
}

# Generate data
haltonMSE <- apply(sequentialHaltonEstimateMatrix, 2, cumulativeSquaredDiff)
sobolMSE <- apply(sequentialSobolEstimateMatrix, 2, cumulativeSquaredDiff)
pseudoMSE <- apply(sequentialPseudoEstimateMatrix, 2, cumulativeSquaredDiff)

# Combine matrices into a single data frame
MSE_df <- data.frame(cbind(1:nrow(haltonMSE), haltonMSE, sobolMSE, pseudoMSE))
colnames(MSE_df) <- c("n", paste("Halton", dimSeq), paste("Sobol", dimSeq), paste("Pseudo", dimSeq))

# Melt the data frame to long format for easier plotting
MSE_long <- melt(MSE_df, id.vars = "n", variable.name = "RNG_Type_Dim", value.name = "MSE")

# Create a function to generate and save the plot
save_plot <- function(data, title, filename) {
  plot <- ggplot(data, aes(x = n, y = MSE, color = RNG_Type_Dim)) +
    geom_line(size = 1) +
    labs(title = title, x = "n", y = "MSE") +
    theme_minimal()
  
  ggsave(filename, plot, width = 8, height = 6)  # Adjust width and height as needed
}

# Generate and save three plots
save_plot(subset(MSE_long, grepl("Halton", RNG_Type_Dim)), "MSE vs. n for Halton RNG", "halton_MSEvsn.png")
save_plot(subset(MSE_long, grepl("Sobol", RNG_Type_Dim)), "MSE vs. n for Sobol RNG", "sobol_MSEvsn.png")
save_plot(subset(MSE_long, grepl("Pseudo", RNG_Type_Dim)), "MSE vs. n for Pseudo RNG", "pseudo_MSEvsn.png")
