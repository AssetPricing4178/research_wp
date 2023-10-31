library(mvtnorm)
# Monte Carlo integration of a bivariate normal distribution using "mvtnorm" package
n <- 100000  # Antal slumpmässiga punkter
a <- -100     # Lägsta gräns i x och y
b <- 100      # Högsta gräns i x och y

# Medelvärden och kovariansmatris för bivariat normalfördelning
mean_vector <- c(0, 0)
cov_matrix <- matrix(c(1, 0.5, 0.5, 1), nrow = 2)

pmvnorm(c(a, a), c(b, b), mean = mean_vector, sigma = cov_matrix)

# Funktion för att evaluera normalfördelningen
f <- function(x, y) {
  return(dmvnorm(cbind(x, y), mean = mean_vector, sigma = cov_matrix))
}

# Pseudo-slumpmässiga variabler
uPseudo <- matrix(runif(2 * n, min = a, max = b), ncol = 2)

# Quasi-slumpvariabler
uSobol <- matrix(a + (b - a) * sobol(2*n), ncol = 2)
uGhalton <- matrix(a + (b - a) * ghalton(2*n), ncol = 2)

# Uppskattningar av integralen
estimatePseudo <- numeric(n)
estimateSobol <- numeric(n)
estimateGhalton <- numeric(n)

# Loop för att utföra integration
for (i in 1:n) {
  
  #Pseudo
  resultPseudo <- (b - a)^2 * mean(f(uPseudo[1:i, 1], uPseudo[1:i, 2]))
  estimatePseudo[i] <- resultPseudo
  
  #Quasi
  resultSobol <- (b - a)^2 * mean(f(uSobol[1:i, 1], uSobol[1:i, 2]))
  estimateSobol[i] <- resultSobol
  
  resultGhalton <- (b - a)^2 * mean(f(uGhalton[1:i, 1], uGhalton[1:i, 2]))
  estimateGhalton[i] <- resultGhalton
}

# Skapa en sekvens av antalet punkter
x <- seq(1, n)

# Plotta resultatet
plot(x, estimatePseudo, cex = 0.2, pch = 20, xlab = "n", col = "blue")
abline(h = pmvnorm(c(a, a), c(b, b), mean = mean_vector, sigma = cov_matrix))
points(estimateSobol, col ="red", cex = 0.2, pch = 20)
points(estimateGhalton, col ="yellow", cex = 0.2, pch = 20)


hej <- pmvnorm(c(a, a), c(b, b), mean = mean_vector, sigma = cov_matrix)
print(hej)