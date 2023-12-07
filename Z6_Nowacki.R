# Zadanie 1
funToPlotZ1 <- \(x, y) sin(x^2 / 2 - y^2 / 4) * cos(2*x - exp(y))
valuesZ1 <- seq(from = -5, to = 5, by = 0.05)
toPlotZ1 <- outer(valuesZ1, valuesZ1, funToPlotZ1)

plot3D::persp3D(valuesZ1, valuesZ1, toPlotZ1)

# Zadanie 5
daneZ5 <- c(4.76,0.35,0.04,-1.26,3.30,3.79,0.82,-1.18,-0.77,2.47,1.50,2.62,1.62,3.27,1.89,1.45, 1.61,2.78,-0.98,2.41)
nZ5 <- length(daneZ5)
logLikZ5 <- function(param){
  mu <- param[1]
  sigma <- param[2]
  -0.5 * nZ5 * log(2*pi) - nZ5 * log(sigma) - 0.5 * sum((daneZ5 - mu)^2/sigma^2) 
}

gradLogLikZ5 <- function(param){
  mu <- param[1]
  sigma <- param[2]
  c(sum((daneZ5 - mu) / sigma^2), -nZ5/sigma + sum((daneZ5 - mu)^2 / sigma^3))
}

hessLogLikZ5 <- function(param){
  mu <- param[1]
  sigma <- param[2]
  hess <- matrix(nrow = 2, ncol = 2)
  hess[1,1] <- -nZ5 / sigma^2
  hess[1,2] <- -2 * sum((daneZ5 - mu) / sigma^3)
  hess[2,1] <- hess[1,2]
  hess[2,2] <- nZ5 / sigma^3 - 3 * sum((daneZ5 - mu)^2/sigma^4)
  hess
}

estimatesZ5 <- maxLik::maxLik(logLikZ5, gradLogLikZ5, hessLogLikZ5, start = c(mu=0, sigma=1))$estimate

# Zadanie 6
# epsilon = miles - \beta_0 - \beta_1 * income
# epislon shoiuld have normal distribution with mu = 0, and sd^2


daneZ6 <- read.csv('PNA_Z06/vacation.csv')

nZ6 <- nrow(daneZ6)

logLikZ6 <- function(param){
  
  b0 <- param[1]
  b1 <- param[2]
  sigma <- param[3]
  x <- daneZ6$miles - (b0 + b1 * daneZ6$income)
  
  -0.5 * nZ6 * log(2*pi) - nZ6 * log(sigma) - 0.5 * sum(x^2) * 1/sigma^2
}

gradLogLikZ6 <- function(param){
  b0 <- param[1]
  b1 <- param[2]
  sigma <- param[3]
  x <- daneZ6$miles - (b0 + b1 * daneZ6$income)
  
  g <- numeric(3)
  g[1] <- 1/(2*sigma^2) * sum(x)*2
  g[2] <- 1/(2*sigma^2) * sum(x*daneZ6$income)*2
  g[3] <- -nZ6/sigma + 1/(sigma^3) * sum(x^2)
  g
}

hessLogLikZ6 <- function(param){
  b0 <- param[1]
  b1 <- param[2]
  sigma <- param[3]
  x <- daneZ6$miles - (b0 + b1 * daneZ6$income)
  
  hess <- matrix(0, nrow = 3, ncol = 3)
  hess[1,1] <- -nZ6/sigma^2
  hess[1,2] <- -1/sigma^2 * sum(daneZ6$income)
  hess[1,3] <- -2/sigma^3 * sum(x)
  hess[2,1] <- hess[1,2]
  hess[2,2] <- -sum(daneZ6$income^2)/sigma^2
  hess[2,3] <- -2/sigma^3 * sum(x*daneZ6$income)
  hess[3,1] <- hess[1,3]
  hess[3,2] <- hess[2,3]
  hess[3,3] <- nZ6 / sigma^2 - 3/sigma^4 * sum(x^2)
  hess
}

estimatesZ6 <- maxLik::maxLik(logLikZ6, gradLogLikZ6, hessLogLikZ6, start = c(b0=0, b1=0, sigma=1))

# Zadanie 8

nZ8 <- 1

logLikZ8 <- function(param){
  
  b0 <- param[1]
  b1 <- param[2]
  sigma <- param[3]
  x <- daneZ6$miles - (b0 + b1 * daneZ6$income)
  
  -0.5 * 1 * log(2*pi) - 1 * log(sigma) - 0.5 * x^2 * 1/sigma^2
}


gradLogLikZ8 <- function(param){
  b0 <- param[1]
  b1 <- param[2]
  sigma <- param[3]
  x <- daneZ6$miles - (b0 + b1 * daneZ6$income)
  
  g1 <- 1/(2*sigma^2) * x*2
  g2 <- 1/(2*sigma^2) * x*daneZ6$income*2
  g3 <- -1/sigma + 1/(sigma^3) * x^2
  cbind(g1, g2, g3)
}

estimatesZ8 <- maxLik::maxBHHH(logLikZ8, gradLogLikZ8, start=c(b0=0, b1=0, sigma=110))
