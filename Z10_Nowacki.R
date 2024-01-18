hipoTestSingleParam <- function(data, fun, start, testValue, alpha = 0.05, twoSided = TRUE) {
  N <- length(data)
  alphaAdjusted <- alpha * (1 - twoSided / 2)
  
  maxim <- maxLik::maxNR(fun(data), start = start)
  estimates <- maxim$estimate
  vcov <- -solve(maxim$hessian)
  stdErr <- sqrt(vcov)
  zTest <- (estimates - testValue) / stdErr * sqrt(N)
  pval <- (twoSided * 2) * (1 - pnorm(abs(zTest), 0, 1))
  critV <- qnorm(1 - alphaAdjusted, 0, 1)
  
  list(`Z-statistic` = zTest, `critical value` = critV, `p-value` = pval, estimates = estimates)
}

hipoTestingMultiParam <- function(data, fun, weightMatrix, constraintVector, startVector, alpha = 0.05){
  N <- length(data)
  maxim <- maxLik::maxNR(fun(data), start = startVector)
  estimates <- maxim$estimate
  sigmaHat <- -solve(maxim$hessian)
  S <- weightMatrix %*% estimates - constraintVector
  W <- t(S) %*% solve(weightMatrix %*% sigmaHat %*% t(weightMatrix)) %*% S * N
  
  ## df = constraints = number of linearaly independent rows of constraint matrix
  df <- qr(weightMatrix)$rank
  criticalV <- qchisq(1 - alpha, df = df)
  pval <- 1 - pchisq(W, df = df)
  
  list(`W-statistic` = W, `critical value` = criticalV, `p-value` = pval, estimates = estimates)
}

# Zadanie 1
dataZ1 <- read.csv('./PNA_Z10/t1000.csv')$x

funZ1 <- function(data) { function(params) {
  df <- params[1]
  N <- length(data)
  mu <- mean(data)
  
  mt <- rbind((data - mu)^2 - df / (df - 2),
              (data - mu)^4 - 3 * df^2 / ((df - 2) * (df - 4)))
  Mt <- rowMeans(mt)
  Wt <- mt %*% t(mt) / N
  
  -0.5 * (t(Mt) %*% solve(Wt) %*% Mt)
}}

## Zbiega się różnie dla różnych wartości startowych, przy 3.5 estimate = 3.19 i odrzucamy
## przy > 4.5 estimate = 6.3524, z-statistic = 0.68944, p-value = 0.49054, nie odrzucamy H0 df = 6
hipoTestSingleParam(dataZ1, funZ1, start = 15.5, testValue = 6)

# Zadanie 2 
dataZ2 <- read.csv('./PNA_Z10/GammaSamp.csv')$x
funZ2 <- function(data) { function(params){
  alpha <- params[1]
  x <- data
  n <- length(data)
  
  M <- rbind(1/n * sum(x) - alpha,
             1/n * sum(x^2) - alpha * (alpha+1))
  W <- matrix(0, nrow = 2, ncol = 2)

  W[1, 1] <- 1/n * sum((x - alpha)^2)
  W[1, 2] <- 1/n * sum((x^2 - alpha) * (x^2 - alpha*(alpha+1)))
  W[2, 1] <- W[1, 2]
  W[2, 2] <- 1/n * sum((x^2 - alpha * (alpha+1))^2)

  -(t(M) %*% solve(W) %*% M)
}}

# Estimate = 4.805196
# z = -0.2365
# p-value = 0.8130247
# Nie ma powodów do odrzucenia hipotezy zerowej alpha = 5
hipoTestSingleParam(dataZ2, funZ2, start = 4, testValue = 5)


# Zadanie 3
dataZ3 <- read.csv("./PNA_Z10/Norm1000.csv")$x

funZ3 <- function(data) { function(params) {
  mi <- params[1]
  sigma <- params[2]
  N <- length(data)
  
  mt <- rbind(data - mi, (data - mi)^2 - sigma^2, (data - mi)^4 - 3 * sigma^4)
  Mt <- rowMeans(mt)
  Wt <- mt %*% t(mt) * 1 / N
  -0.5 * (t(Mt) %*% solve(Wt) %*% Mt)
}}


# W = 1.183, critical value = 5.99, p-value = 0.5534, nie ma powodów do odrzucenia H0 mu = 3, sigma = 1
hipoTestingMultiParam(dataZ3, funZ3, weightMatrix = diag(1, 2), constraintVector = c(3, 1), startVector = c(2.5, 1))

# Z = 1.069, critical value = 1.96, p-value = 0.285, nie ma powodów do odrzucenia H0 mu = 3
hipoTestSingleParam(dataZ3, funZ3, start = c(2.5, 1), testValue = 3)

#### Zadanie 4

dataZ4 <- read.csv("./PNA_Z10/GammaBothParm.csv")$x

funZ4 <- function(x) { function(params){
  alpha <- params[1]
  beta <- params[2]
  n <- length(x)
  
  M <- rbind(1/n * sum(x) - alpha/beta,
             1/n * sum(x^2) - (alpha * (alpha+1) / beta^2),
             1/n * sum(1/x) - beta / (alpha - 1))
  
  W <- matrix(0, nrow = 3, ncol = 3)
  
  e1 <- x - alpha/beta
  e2 <- x^2 - (alpha*(alpha+1)) / beta^2 
  e3 <- 1/x - beta / (alpha - 1)
  
  W[1, 1] <- sum(e1^2)
  W[1, 2] <- sum(e1 * e2)
  W[1, 3] <- sum(e1 * e3)
  W[2, 1] <- W[1, 2]
  W[2, 3] <- sum(e2 * e3)
  W[2, 2] <- sum(e2^2)
  W[3, 1] <- W[1, 3]
  W[3, 2] <- W[2, 3]
  W[3, 3] <- sum(e3^2)
  
  W <- W / n
  
  -(t(M) %*% solve(W) %*% M)
}}

#RZ4 <- diag(1, 2)
#qZ4 <- c(5, 5)
RZ4 <- matrix(c(1, 1, 0, 1), nrow=2)
qZ4 <- c(5, 10)

## Without reformulation of constraints the RZ4 the matrix should be
## [1 0] a
## [1 1] a + b
## and then obviously qZ4 is 5, 10

NZ4 <- length(dataZ4)
resZ4 <- maxLik::maxNR(funZ4(dataZ4), start = c(2.5, 1))

# 3.833091 4.741484
thetaZ4 <- resZ4$estimate

sigmaHat <- -solve(resZ4$hessian)/NZ4
SZ4 <- RZ4 %*% thetaZ4 - qZ4

## 6586.113 results are the same as with equivalent version of constraints.
## szok i niedowierzanie
WZ4 <- t(SZ4) %*% solve(RZ4 %*% sigmaHat %*% t(RZ4)) %*% SZ4 * NZ4

## df = constraints = number of linearaly independent rows of constraint matrix

# 5.99
criticalVZ4 <- qchisq(0.95, df = 2)

# 0, reject null
pZ4 <- 1 - pchisq(WZ4, df = 2)


