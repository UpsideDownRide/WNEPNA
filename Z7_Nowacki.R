# Zadanie 1

dataZ1 <- c(-1.13,-0.62,0.06,-1.82,-0.27,-0.74,0.49,-0.35,-0.41,-0.05,-0.20,0.06,-1.06,0.58,1.20, 1.59,0.20,0.65,-0.53,-0.73,-1.16,1.34,1.77,0.70,-0.10,-0.30,0.60,0.49,-0.45,-1.15,-0.31,1.30,0.91,0.47,-0.44,1.23,-0.15,1.22,-1.26,-0.18)

nZ1 <- length(dataZ1)
logLikZ1 <- function(param){
  mu <- param
  -0.5 * nZ1 * log(2*pi) - 0.5 * sum((dataZ1 - mu)^2) 
}
resultZ1 <- maxLik::maxLik(logLikZ1, start = c(mu=0))
estimateZ1 <- resultZ1$estimate
varZ1 <- -solve(resultZ1$hessian - 0) / sqrt(nZ1)

statisticZ1 <- (estimatesZ1 - 0) / sqrt(varZ1)
pvalueZ1 <- 2 * (1 - pnorm(abs(statisticZ1)))

# Zadanie 2

dataZ2 <- c(0,0,3,1,3,11,0,7,2,3,4,2,0,1,2,1,3,2,4,2,1,4,6,4,0,6,0,0,4,2)
nZ2 <- length(dataZ2)

logLikZ2 <- function(param){
  n <- nZ2
  p <- param
  n * log(p) - n * log(1 - p) + log(1 - p) * sum(dataZ2)
}

resultZ2 <- maxLik::maxLik(logLikZ2, start = 0.5)
estimateZ2 <- resultZ2$estimate
varZ2 <- -solve(resultZ2$hessian - 0) / sqrt(nZ2)

statisticZ2 <- (estimateZ2 - 0.25) / sqrt(varZ2)
pvalueZ2 <- 2 * (1 - pnorm(abs(statisticZ2)))
# Odrzucamy H_0 o tym, ¿e parametr jest równy 0.25

# Zadanie 5

# !!!!!!!! $operator return different type than Service[2] FFFFFSSSSS
dataZ5 <- resampledata::Service$Times
nZ5 <- length(dataZ5)

logLikZ5 <- function(params){
  k <- params[1]
  theta <- params[2]
  xi <- dataZ5
  n <- nZ5
  
  (k - 1) * sum(log(xi)) - 1/theta * sum(xi) - n * k * log(theta) - n * log(gamma(k))
}

resultZ5 <- maxLik::maxLik(logLikZ5, start = c(1, 1))

lrZ5 <- 2 * (resultZ5$maximum - logLikZ5(c(1, 2)))
pvalueZ5 <- 1 - pchisq(lrZ5, df = 2)
# Odrzucamy H_0

# Zadanie 7

dataZ7 <- c(0.62,-2.52,-0.31,-0.73,2.54,-1.52,-1.18,2.06,2.53,2.52,0.66,0.02,-0.93,-0.09,0.81 -1.60,2.70,0.52,1.75,-0.79,3.66,-1.05,-1.32,-2.42,0.41,-2.09,2.67,1.36,0.94,0.58,-0.40,1.91,0.18,1.41,4.56,-0.01,-1.60,-0.07,1.79,2.23,0.52,-2.81,-1.74,0.71,2.09,2.25,1.33,0.37,-2.04,2.29)
nZ7 <- length(dataZ7)

loglikZ7 <- function(param){
  mu <- param
  sigma <- 2
  n <- nZ7
  xi <- dataZ7
  -0.5 * n * log(2 * pi) - n * log(sigma) - 0.5 * 1 / sigma^2 * sum((xi - mu)^2)
}

resultZ7 <- maxLik::maxNR(loglikZ7, start = 1)
lrZ7 <- 2 * (resultZ7$maximum - loglikZ7(0))
pvalueZ7 <- 1 - pchisq(lrZ7, df = 1)
# Nie ma podstaw do odrzucenia H_0 - \mu = 0
