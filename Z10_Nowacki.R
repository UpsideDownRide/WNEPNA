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

RZ2 <- diag(1, 1)
qZ2 <- c(5)

NZ2 <- length(dataZ2)
resZ2 <- maxLik::maxNR(funZ2(dataZ2), start = 4)
#  4.805181
thetaZ2 <- resZ2$estimate
sigmaHat <- -solve(resZ2$hessian)/NZ2
S <- RZ2 %*% thetaZ2 - qZ2

## 1183.108
W <- t(S) %*% solve(RZ2 %*% sigmaHat %*% t(RZ2)) %*% S * NZ2

vcov <- -solve(resZ2$hessian)
stdErr <- sqrt(vcov)
## -0.2365
zTest <- (resZ2$estimate - 5) / stdErr * sqrt(NZ2)
pval2 <- 2 * (1 - pnorm(abs(zTest), 0, 1))

critV2 <- qnorm(0.975, 0, 1)


# Zadanie 3
dataZ3 <- read.csv("./PNA_Z10/Norm1000.csv")$x

RZ3 <- diag(1, 2)
qZ3 <- c(3,1)
NZ3 <- length(dataZ3)
resZ3 <- maxLik::maxNR(funZ3alt(dataZ3), start = c(2.5, 1))
thetaZ3 <- resZ3$estimate
sigmaHat <- -solve(resZ3$hessian)/NZ3
S <- RZ3 %*% thetaZ3 - qZ3

## 1183.108
W <- t(S) %*% solve(RZ3 %*% sigmaHat %*% t(RZ3)) %*% S * NZ3

## df = constraints = number of linearaly independent rows of constraint matrix

# 5.99
criticalV <- qchisq(0.95, df = 2)

# 0, reject null
pZ3 <- 1 - pchisq(W, df = 2)

# H0 mi = 3

vcov <- -solve(resZ3$hessian)
stdErr <- sqrt(vcov[1, 1])
zTest <- (resZ3$estimate[1] - 3) / stdErr * sqrt(NZ3)
pval2 <- 2 * (1 - pnorm(abs(zTest), 0, 1))

critV2 <- qnorm(0.975, 0, 1)

funZ3 <- function(data) { function(params){
  mu <- params[1]
  sigma <- params[2]
  n <- length(data)
  m1 <- data
  m2 <- (data - mu)^2
  m3 <- (data - mu)^4
  z1 <- mu
  z2 <- sigma^2
  z3 <- 3 * sigma^4
  
  M <- rbind(sum(m1) / n - z1,
             sum(m2) / n - z2,
             sum(m3) / n - z3)
  
  W <- matrix(0, nrow = 3, ncol = 3)
  
  e1 <- m1 - z1
  e2 <- m2 - z2
  e3 <- m3 - z3
  
  W[1, 1] <- sum(e1^2)
  W[1, 2] <- sum(e1 * e2)
  W[1, 3] <- sum(e1 * e3)
  W[2, 1] <- W[1, 2]
  W[2, 3] <- sum(e2 * e3)
  W[3, 1] <- W[1, 3]
  W[3, 2] <- W[2, 3]
  W[2, 2] <- sum(e2^2)
  W[3, 3] <- sum(e3^2)
  
  W <- W / n
  
  -(t(M) %*% solve(W) %*% M)
}}

funZ3alt <- function(data) { function(params) {
  mi <- params[1]
  sigma <- params[2]
  N <- length(data)
  
  mt <- rbind (data - mi, (data - mi)^2 - sigma^2, (data - mi)^4 - 3 * sigma^4)
  Mt <- rowMeans(mt)
  Wt <- mt%*%t(mt) * 1 / N
  -0.5 * (t(Mt) %*% solve(Wt) %*% Mt)
}}

maxLik::maxNR(funZ3alt(dataZ3), start = c(2.5, 1))$estimate

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


