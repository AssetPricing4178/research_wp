library(qrng)
set.seed(6435)
n <- 1000000
d <- 1

pseudoTimeVector <- c()
sobolTimeVector  <- c()
haltonTimeVector <- c()

dimSeq <- 1:80

for(d in dimSeq){

pseudoTime <- system.time({
  pseudoSeq <- matrix(runif(d * n), ncol = d)

})["elapsed"]

sobolTime <- system.time({
  sobolSeq  <- sobol(n, d)
})["elapsed"]

haltonTime <- system.time({
  haltonSeq <-  ghalton(n, d, method = "halton")
})["elapsed"]

pseudoTimeVector <- append(pseudoTimeVector, pseudoTime)
sobolTimeVector  <- append(sobolTimeVector, sobolTime)
haltonTimeVector <- append(haltonTimeVector, haltonTime)
}

plot(dimSeq,haltonTimeVector, type = "l", col = "red", 
     xlab ="Dimension", ylab = "Computation time", main = "Computation time with increasing dimensions")
  points(sobolTimeVector, type = "l", col = "green",)
  points(pseudoTimeVector, type = "l", col = "blue")
  legend("topleft", legend = c("Halton", "Sobol", "Pseudo"),lty =1, 
         col = c("red", "green", "blue"), 
         cex = 0.8, title = "Sequences")

plot(dimSeq,sobolTimeVector, type = "l", col = "green", 
       xlab ="Dimension", ylab = "Computation time", main = "Computation time with increasing dimensions")
  points(pseudoTimeVector, type = "l", col = "blue")
  legend("topleft", legend = c( "Sobol", "Pseudo"),lty =1, 
         col = c( "green", "blue"), 
         cex = 0.8, title = "Sequences")
  
  