pdf(file = "Coolaflerdimplots.pdf")
# Example for a 2D function
f <- function(x, y) {
  return(x^2 + y^2)
}

x <- seq(-5, 5, length.out = 100)
y <- seq(-5, 5, length.out = 100)

z <- outer(x, y, f)

# Basic 2D plot
contour(x, y, z)


# Example for a 3D function
f <- function(x, y) {
  return(x^2 + y^2)
}

x <- seq(-5, 5, length.out = 100)
y <- seq(-5, 5, length.out = 100)

z <- outer(x, y, f)

# Basic 3D plot
persp(x, y, z, theta = 30, phi = 30)


# Example for a 4D function
f <- function(x, y, z) {
  return(x^2 + y^2 + z^2)
}

# Generate data points
x <- seq(-5, 5, length.out = 30)
y <- seq(-5, 5, length.out = 30)
z <- seq(-5, 5, length.out = 30)

# Create a 3D scatter plot for a subset of variables
library(scatterplot3d)
xyz <- expand.grid(x, y, z)
xyz$w <- f(xyz$Var1, xyz$Var2, xyz$Var3)

scatterplot3d(xyz$Var1, xyz$Var2, xyz$Var3, color = heat.colors(length(xyz$w)), pch = 16, main = "4D Function")

#Bivariate normal
# Load necessary libraries
library(MASS)  # For mvrnorm function
library(rgl)   # For 3D plotting

# Define the standard bivariate normal PDF
standard_bivariate_normal_pdf <- function(x, y, rho = 0) {
  constant <- 1 / (2 * pi * sqrt(1 - rho^2))
  exponent <- -((x^2 - 2 * rho * x * y + y^2) / (2 * (1 - rho^2)))
  
  return(constant * exp(exponent))
}

# Create a grid of values for x and y
x <- seq(-3, 3, length.out = 100)
y <- seq(-3, 3, length.out = 100)
z <- matrix(0, nrow = length(x), ncol = length(y))

# Calculate the PDF values for each combination of x and y
for (i in 1:length(x)) {
  for (j in 1:length(y)) {
    z[i, j] <- standard_bivariate_normal_pdf(x[i], y[j])
  }
}

# Create a 3D surface plot
persp(x, y, z, theta = 30, phi = 30, col = "lightblue", main = "Standard Bivariate Normal Distribution")

#med korrelation
# Load necessary libraries
library(MASS)  # For mvrnorm function
library(rgl)   # For 3D plotting

# Define the bivariate normal PDF with correlation
bivariate_normal_pdf <- function(x, y, rho) {
  constant <- 1 / (2 * pi * sqrt(1 - rho^2))
  exponent <- -((x^2 - 2 * rho * x * y + y^2) / (2 * (1 - rho^2)))
  
  return(constant * exp(exponent))
}

# Create a grid of values for x and y
x <- seq(-3, 3, length.out = 100)
y <- seq(-3, 3, length.out = 100)
z <- matrix(0, nrow = length(x), ncol = length(y))

# Set correlation coefficient
rhoseq <- seq(-0.99,0.99, by = 0.2)
for (rho in rhoseq){

# Calculate the PDF values for each combination of x and y
for (i in 1:length(x)) {
  for (j in 1:length(y)) {
    z[i, j] <- bivariate_normal_pdf(x[i], y[j], rho)
  }
}

# Create a 3D surface plot
persp(x, y, z, theta = 30, phi = 30, col = "lightblue", main = paste("Bivariate Normal Distribution with Correlation =", rho))
}
dev.off()