### Zadanie 1
funToPlotZ1 <- \(x, y) sin(x^2 / 2 - y^2 / 4) * cos(2*x - exp(y))
valuesZ1 <- seq(from = -5, to = 5, by = 0.05)
toPlotZ1 <- outer(valuesZ1, valuesZ1, funToPlotZ1)

plot3D::persp3D(valuesZ1, valuesZ1, toPlotZ1)

### Zadanie 2

rosenbrockZ2 <- \(x, y) (1-x)^2 + 100 * (y - x^2)^2
valuesZ2 <- seq(from = -5, to = 5, by = 0.05)
toPlotZ2 <- outer(valuesZ2, valuesZ2, rosenbrockZ2)
plot3D::persp3D(valuesZ2, valuesZ2, toPlotZ2)

### Zadanie 3

funToPlotZ3 <- \(x, y) sin(5 * x) * cos(5 * y) * 0.2

valuesZ3 <- seq(from = -2, to = 2, by = 0.05)
toPlotZ3 <- outer(valuesZ3, valuesZ3, funToPlotZ3)
plot3D::persp3D(valuesZ3, valuesZ3, toPlotZ3)

### Zadanie 4

funToPlotZ4 <- \(x, y) sin(10 * (x^2 + y^2)) * 0.1

valuesZ4 <- seq(from = -1, to = 1, by = 0.05)
toPlotZ4 <- outer(valuesZ4, valuesZ4, funToPlotZ4)
plot3D::persp3D(valuesZ4, valuesZ4, toPlotZ4)

### Zadanie 5
daneZ5 <- c(4.76,0.35,0.04,-1.26,3.30,3.79,0.82,-1.18,-0.77,2.47,1.50,2.62,1.62,3.27,1.89,1.45, 1.61,2.78,-0.98,2.41)

logLikNormal <- function(data) { function(param){
  n <- length(data)
  mu <- param[1]
  sigma <- param[2]
  -0.5 * n * log(2*pi) - n * log(sigma) - 0.5 * sum((data - mu)^2/sigma^2) 
}}

gradLogLikNormal <- function(data) { function(param){
  n <- length(data)
  mu <- param[1]
  sigma <- param[2]
  c(sum((data - mu) / sigma^2), -n/sigma + sum((data - mu)^2 / sigma^3))
}}

hessLogLikNormal <- function(data) { function(param){
  mu <- param[1]
  sigma <- param[2]
  n <- length(data)
  hess <- matrix(nrow = 2, ncol = 2)
  
  hess[1,1] <- -n / sigma^2
  hess[1,2] <- -2 * sum((data - mu) / sigma^3)
  hess[2,1] <- hess[1,2]
  hess[2,2] <- n / sigma^3 - 3 * sum((data - mu)^2/sigma^4)
  hess
}}

#      mu   sigma 
# 1.52450 1.68957 
estimatesZ5 <- maxLik::maxLik(logLikNormal(daneZ5),
                              gradLogLikNormal(daneZ5),
                              hessLogLikNormal(daneZ5),
                              start = c(mu=0, sigma=1))$estimate
# 2.854645
estimatedVarZ5 <- estimatesZ5['sigma']^2

# 3.005016 
unbiasedVarZ5 <- var(daneZ5)

# 2.854765 
biasedVarZ5 <- unbiasedVarZ5 * (length(daneZ5) - 1) / length(daneZ5)

# 1.5245
meanZ5 <- mean(daneZ5)

xsZ5 <- seq(from = 0.5, to = 2.5, by = 0.05)
ysZ5 <- seq(from = 0.5, to = 2.5, by = 0.05)

biparamLLN <- \(mu, sigma) logLikNormal(daneZ5)(c(mu, sigma))
toPlotZ5 <- outer(xsZ5, ysZ5, Vectorize(biparamLLN))

plot3D::persp3D(xsZ5, ysZ5, toPlotZ5)


### Zadanie 6
# epsilon = miles - \beta_0 - \beta_1 * income
# epislon should have normal distribution with mu = 0, and sd^2


daneZ6 <- read.csv('PNA_Z06/vacation.csv')

logLikZ6 <- function(data) { function(param){
  
  b0 <- param[1]
  b1 <- param[2]
  sigma <- param[3]
  n <- length(data)
  x <- data$miles - (b0 + b1 * data$income)
  
  -0.5 * n * log(2*pi) - n * log(sigma) - 0.5 * sum(x^2) * 1/sigma^2
}}

gradLogLikZ6 <- function(data) { function(param){
  b0 <- param[1]
  b1 <- param[2]
  sigma <- param[3]
  n <- length(data)
  x <- data$miles - (b0 + b1 * data$income)
  
  g <- numeric(3)
  g[1] <- 1/(2*sigma^2) * sum(x)*2
  g[2] <- 1/(2*sigma^2) * sum(x*data$income)*2
  g[3] <- -n/sigma + 1/(sigma^3) * sum(x^2)
  g
}}

hessLogLikZ6 <- function(data) { function(param){
  b0 <- param[1]
  b1 <- param[2]
  sigma <- param[3]
  n <- length(data)
  x <- data$miles - (b0 + b1 * data$income)
  
  hess <- matrix(0, nrow = 3, ncol = 3)
  hess[1,1] <- -n/sigma^2
  hess[1,2] <- -1/sigma^2 * sum(data$income)
  hess[1,3] <- -2/sigma^3 * sum(x)
  hess[2,1] <- hess[1,2]
  hess[2,2] <- -sum(data$income^2)/sigma^2
  hess[2,3] <- -2/sigma^3 * sum(x*data$income)
  hess[3,1] <- hess[1,3]
  hess[3,2] <- hess[2,3]
  hess[3,3] <- n / sigma^2 - 3/sigma^4 * sum(x^2)
  hess
}}

#          b0         b1      sigma 
#  49.6347303 12.3796891  0.5517624 
estimatesZ6 <- maxLik::maxLik(logLikZ6(daneZ6), gradLogLikZ6(daneZ6), hessLogLikZ6(daneZ6), start = c(b0=0, b1=0, sigma=1))

# (Intercept)       income  sqrt(R^2)
#       48.94        15.73   0.52154
lm(miles ~ income, daneZ6)

### Zadanie 7

dataZ7 <- resampledata::Turbine$AveSpeed

likWeibull <- \(data) \(params) {
  n <- length(data)
  k <- params[1]
  l <- params[2]
  
  k^n * l^-(k * n) * prod(data^(k-1), exp(-(data/l)^k))
}

logLikWeibull <- function(k, l, data, n) {
  n * log(k) - k * n * log(l) + sum((k-1) * log(data) - (data / l)^k)
}

maxLLikWeibull <- \(data) \(params) {
  n <- length(data)
  k <- params[1]
  l <- params[2]
  
  logLikWeibull(k, l, data, n)
}

maxGradLLikWeibull <- \(data) \(params) {
  n <- length(data)
  k <- params[1]
  l <- params[2]
  
  g <- numeric(2)
  
  # n * (1/k - log(l)) + sum(log(data) - (data/l)^k * (log(data) - log(l)))
  g[1] <- Deriv::Deriv(logLikWeibull, "k")(k, l, data, n)
  
  # sum(data * k * (data/l)^(k - 1)/l^2) - k * n/l
  g[2] <- Deriv::Deriv(logLikWeibull, "l")(k, l, data, n)
  
  g
}

#        k        l 
# 3.169320 7.661278 

estimatesZ7 <- maxLik::maxNR(maxLLikWeibull(dataZ7),
                             maxGradLLikWeibull(dataZ7),
                             start=c(k=0, l=0))$estimates


xsZ7 <- seq(from = 1.5, to = 3.5, by = 0.05)
ysZ7 <- seq(from = 4.5, to = 10.5, by = 0.05)

biparamLLW <- \(k, l) maxLLikWeibull(dataZ7)(c(k, l))
toPlotZ7 <- outer(xsZ7, ysZ7, Vectorize(biparamLLW))

plot3D::persp3D(xsZ7, ysZ7, toPlotZ7)

### Zadanie 8

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

#       b0        b1     sigma 
# 49.08494  15.72367 470.48986 
estimatesZ8 <- maxLik::maxBHHH(logLikZ8, gradLogLikZ8, start=c(b0=0, b1=0, sigma=110))
