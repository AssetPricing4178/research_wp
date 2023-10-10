library(qrng)
library(random)
# Monte Carlo integration of a 3D normal distribution
n <- 10000  # Antal slumpmässiga punkter
a <- -100   # Lägsta gräns i x, y och z
b <- 100    # Högsta gräns i x, y och z

# Funktion för 3D normalfördelningen
f <- function(x, y, z) {
  return(1 / (2 * pi)^(3/2) * exp(-(x^2 + y^2 + z^2) / 2))
}

# Pseudo-slumpmässiga variabler
uPseudo <- matrix(runif(3 * n, min = a, max = b), ncol = 3)

#quasi
uSobol <- matrix((a + (b - a) * sobol(3*n) ),ncol = 3)
uGhalton <- matrix((a + (b - a) * ghalton(3*n)), ncol = 3)

# Uppskattningar av integralen
estimatePseudo <- numeric(n)
estimateSobol <- numeric(n)
estimateGhalton <- numeric(n)

# Loop för att utföra integration
for (i in 1:n) {
  #pseudo
  resultPseudo <- (b - a)^3 * mean(f(uPseudo[1:i, 1], uPseudo[1:i, 2], uPseudo[1:i, 3]))
  estimatePseudo[i] <- resultPseudo
  
  #quasi
  resultSobol <- (b - a)^3 * mean(f(uSobol[1:i, 1], uSobol[1:i, 2], uSobol[1:i, 3]))
  estimateSobol[i] <- resultSobol
  
  resultGhalton <- (b - a)^3 * mean(f(uGhalton[1:i, 1], uGhalton[1:i, 2], uGhalton[1:i, 3]))
  estimateGhalton[i] <- resultGhalton
}

# Skapa en sekvens av antalet punkter
x <- seq(1, n)

# Plotta resultatet
x <- seq(1,n)
plot(x,estimatePseudo, cex = 0.2, pch = 20, xlab = "n")
points(estimateSobol, col ="red", cex = 0.2, pch = 20)
points(estimateGhalton, col ="yellow", cex = 0.2, pch = 20)
abline(h=1)
