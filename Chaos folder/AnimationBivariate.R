# Install and load the necessary packages
if (!requireNamespace("rgl", quietly = TRUE)) {
  install.packages("rgl")
}
if (!requireNamespace("animation", quietly = TRUE)) {
  install.packages("animation")
}
library(rgl)
library(animation)

# Function to generate the bivariate normal distribution plot
plot_bivariate_normal <- function(rho) {
  x <- seq(-3, 3, length.out = 100)
  y <- seq(-3, 3, length.out = 100)
  z <- matrix(0, nrow = length(x), ncol = length(y))
  
  for (i in 1:length(x)) {
    for (j in 1:length(y)) {
      z[i, j] <- standard_bivariate_normal_pdf(x[i], y[j], rho)
    }
  }
  
  # Ensure valid range for z
  z_range <- range(z, finite = TRUE)
  
  # Adjust for empty ranges
  if (is.infinite(z_range[1])) z_range[1] <- -1
  if (is.infinite(z_range[2])) z_range[2] <- 1
  
  persp(x, y, z, theta = 30, phi = 30, col = "lightblue", main = paste("Bivariate Normal Distribution (rho =", round(rho, 2), ")"), zlim = z_range)
}

# Create an animation for continuous rho values
ani.options(interval = 0.5)
saveGIF({
  rhos <- seq(-1, 1, by = 0.05)
  for (rho_val in rhos) {
    plot_bivariate_normal(rho_val)
    Sys.sleep(0.1)
    clear3d()
  }
}, movie.name = "bivariate_normal_animation.gif", interval = 0.5, ani.width = 600, ani.height = 600)
