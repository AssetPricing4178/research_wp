
library(rgl)
library(animation)
library(webshot)

# Define the bivariate normal PDF with correlation
standard_bivariate_normal_pdf <- function(x, y, rho) {
  constant <- 1 / (2 * pi * sqrt(1 - rho^2))
  exponent <- -((x^2 - 2 * rho * x * y + y^2) / (2 * (1 - rho^2)))
  
  return(constant * exp(exponent))
}

# Function to generate the bivariate normal distribution plot with colored contour lines
plot_bivariate_normal <- function(rho) {
  x <- seq(-3, 3, length.out = 100)
  y <- seq(-3, 3, length.out = 100)
  z <- matrix(0, nrow = length(x), ncol = length(y))
  
  for (i in 1:length(x)) {
    for (j in 1:length(y)) {
      pdf_value <- standard_bivariate_normal_pdf(x[i], y[j], rho)
      z[i, j] <- ifelse(is.finite(pdf_value), pdf_value, 0)
    }
  }
  
  # Ensure valid range for z
  z_range <- range(z, finite = TRUE)
  
  # Adjust for empty ranges
  if (is.infinite(z_range[1])) z_range[1] <- -1
  if (is.infinite(z_range[2])) z_range[2] <- 1
  
  # Highlight the volume where x and y are in the range [-1, 1]
  x_index <- which(x >= -1 & x <= 1)
  y_index <- which(y >= -1 & y <= 1)
  
  # Plot the bivariate normal distribution
  image(x, y, z, col = terrain.colors(20), zlim = z_range,
        main = paste("Bivariate Normal Distribution (rho =", round(rho, 2), ")"))
  
  # Add contour lines for better visualization
  contour(x, y, z, add = TRUE, levels = c(0.05, 0.1, 0.2, 0.3), col = "black", lwd = 2, drawlabels = FALSE)
  
  # Color the contour lines corresponding to the region of interest
  contour(x, y, z, add = TRUE, levels = 0.2, col = "red", lwd = 2, drawlabels = FALSE)
}

# Create an animation for continuous rho values and save as HTML
ani.options(interval = 0.5, nmax = 50)
saveHTML({
  rhos <- seq(-1, 1, by = 0.04)
  for (rho_val in rhos) {
    plot_bivariate_normal(rho_val)
    Sys.sleep(0.1)
  }
}, interval = 0.5, htmlfile = "bivariate_normal_animation_colored_contours.html", ani.width = 600, ani.height = 600)

# Convert HTML frames to images using webshot
webshot("bivariate_normal_animation_colored_contours.html", file = "frame", cliprect = "viewport", selector = "body")

# Use an external tool like ImageMagick to convert images to GIF
system("magick convert -delay 50 frame*.png bivariate_normal_animation_colored_contours.gif")

# Clean up temporary files
unlink("frame*.png")
