### Zadanie 1

funZ1 <- \(v) v/(v-2) - 10
plot(funZ1, from = 0, to = 3)
abline(h = 0, col = "red")
uniroot(funZ1, interval = c(2, 3))

### Zadanie 2
funZ2 <- \(x) x^4 - 8 * x^3 + 10 * x^2 - 3 * x + 9 
plot(funZ2, from = -5, to = 10)
abline(h = 0, col = "red")

# x1 = 1.792568
# x2 = 6.499701
x1Z2 <- uniroot(funZ2, interval = c(1, 2))$root
x2Z2 <- uniroot(funZ2, interval = c(5, 7))$root

### Zadanie 3
funZ3 <- \(x) exp(3*x) - 4
plot(funZ3, from = -0, to = 2)
abline(h = 0, col = "red")

# x ~= 0.4620955
xZ3 <- uniroot(funZ3, interval = c(0, 1))$root

### Zadanie 4

dataZ4 <- read.csv('PNA_Z09/zep.csv')$x

funZ4 <- function(data) { function(params){
  alpha <- params[1]
  beta <- params[2]
  equation1 <- alpha / beta - mean(data)
  equation2 <- alpha / beta^2 - var(data)
  c(equation1, equation2)
}}

# \alpha =  0.9970755 \beta = 4.9912561
# Znaczaco rozni sie od wynikow z Z8 ??
rootSolve::multiroot(funZ4(dataZ4), start = c(1, 1))

### Zadanie 5

dataZ5 <- read.csv('PNA_Z09/BetaSamp.csv')$x

funZ5 <- function(data) { function(params) {
  alpha <- params[1]
  beta <- params[2]
  equation1 <- alpha / (alpha + beta) - mean(data)
  equation2 <- alpha * beta / ((alpha + beta)^2 * (alpha + beta + 1)) - var(data)
  c(equation1, equation2)
}}

# \alpha = 3.130687 \beta = 7.184186
rootSolve::multiroot(funZ5(dataZ5), start = c(1, 1))

### Zadanie 6

dataZ6 <- read.csv('PNA_Z09/FSdat.csv')$x

# F-distribution, Ex = d / (d - 2), Ex^2 = d^2 * (f + 2) / (f * (d - 2) * (d - 4))
# mean = d / (d - 2), var = 2 * d^2 * (d + f - 2) / (f * (d - 2)^2 * (d - 4))
# d = 2*mean / (mean - 1) 
# f = -(2 * mean^2) / (mean^3 - mean^2 + mean * var - 2 * var)  
mZ6 <- mean(dataZ6)
vZ6 <- var(dataZ6)

# 9.427639, 18.80839
dmmZ6 <- 2 * mZ6 / (mZ6 - 1)
fmmZ6 <- -(2 * mZ6^2) / (mZ6^3 - mZ6^2 + mZ6 * vZ6 - 2 * vZ6)

### Zadanie 7

dataZ7 <- read.csv('./PNA_Z09/GammaSamp.csv')$x

funZ7 <- function(x) { function(params){
  alpha <- params[1]
  n <- length(x)
  
  M <- rbind(1/n * sum(x) - alpha,
             1/n * sum(x^2) - alpha * (alpha+1))
  
  W <- matrix(0, nrow = 2, ncol = 2)
  
  e1 <- x - alpha
  e2 <- x^2 - alpha * (alpha+1)
  
  W[1, 1] <- sum(e1^2)
  W[1, 2] <- sum(e1 * e2)
  W[2, 1] <- W[1, 2]
  W[2, 2] <- sum(e2^2)
  W <- W / n
  
  -(t(M) %*% solve(W) %*% M)
}}


# 4.560983
maxLik::maxNR(funZ7(dataZ7), start = 4)$estimate

# 4.535683
maxLik::maxBFGSR(funZ7(dataZ7), start = 4)$estimate


checkZ7 <- function(params, data){
  alpha <- params[1]
  
  m1 <- data - alpha
  m2 <- data^2 - alpha * (alpha + 1)
  
  cbind(m1, m2)
}

# 4.6767 - two step 
gmm::gmm(checkZ7, dataZ7, 3)


### Zadanie 8

daneZ8 <- read.csv('PNA_Z09/GammaBothParm.csv')$x

funZ8 <- function(x) { function(params){
  alpha <- params[1]
  beta <- params[2]
  n <- length(x)
  
  M <- rbind(1/n * sum(x) - alpha/beta,
             1/n * sum(x^2) - (alpha * (alpha+1) / beta^2),
             1/n * sum(1/x) - beta / (alpha - 1))
  
  W <- matrix(0, nrow = 3, ncol = 3)
  
  e1 <- x - alpha/beta
  e2 <- x^2 - alpha*(alpha+1) / beta^2 
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

# Nie zbiega sie do niczego sensownego
maxLik::maxNR(funZ8(daneZ8), start = c(0.01, 0.01))$estimate

checkZ8 <- function(params, data){
  alpha <- params[1]
  beta <- params[2]
  
  m1 <- data - alpha/beta
  m2 <- data^2 - (alpha * (alpha+1) / beta^2)
  m3 <- 1/data - beta / (alpha - 1)
  
  cbind(m1, m2, m3)
}

# a ~= 0, b ~= 0
gmm::gmm(checkZ8, daneZ8, c(0.1, 0.1))

### Zadanie 9

dataZ9 <- read.csv('PNA_Z09/Norm1000.csv')$x

funZ9 <- function(data) { function(params){
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
             sum(m2) / n - z1,
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
  
  #W <- W / n
  
  -(t(M) %*% solve(W) %*% M)
}}

maxLik::maxNR(funZ9(dataZ9), start = c(1, 1))$estimate

checkZ9 <- function(params, data){
  mu <- params[1]
  sigma <- params[2]
  
  m1 <- data
  m2 <- (data - mu)^2
  m3 <- (data - mu)^4
  z1 <- mu
  z2 <- sigma^2
  z3 <- 3 * sigma^4
  
  cbind(m1 - z1, m2 - z2, m3 - z3)
}

# mu = 3.034, sigma = 1.0047
gmm::gmm(checkZ9, dataZ9, c(1, 1))