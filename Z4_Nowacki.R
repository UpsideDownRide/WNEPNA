# Zadanie 2.4

daneZ24 <- data.frame(x=c(1,2,3), y=c(1,2,1))
xi <- c(rep(1, length(daneZ24$x)), daneZ24$x) |>
  matrix(ncol = 2)
yi <- daneZ24$y

A <- solve(t(xi) %*% xi) 
beta <- A %*% t(xi) %*% yi
res <- yi - xi %*% beta
df <- nrow(xi) - (ncol(xi) - 1) - 1
resVar <- 1/df * sum(res^2)
covar <- resVar * A
covb0b1 <- covar[[1,2]] 

