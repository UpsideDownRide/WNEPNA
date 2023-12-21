# Zadanie 1

funZ1 <- \(v) v/(v-2) - 10
plot(funZ1, from = 0, to = 3)
abline(h = 0, col = "red")
uniroot(funZ1, interval = c(2, 3))

# Zadanie 4

daneZ4 <- read.csv('PNA_Z09/zep.csv')$x
meanZ4 <- mean(daneZ4)
varZ4 <- var(daneZ4)

funZ4 <- function(params){
  alpha <- params[1]
  beta <- params[2]
  equation1 <- alpha / beta - meanZ4
  equation2 <- alpha / beta^2 - varZ4
  c(equation1, equation2)
}

rootSolve::multiroot(funZ4, start = c(1, 1))

# Zadanie 7

daneZ7 <- read.csv('PNA_Z09/GammaSamp.csv')$x
meanZ7 <- mean(daneZ7)
varZ7 <- var(daneZ7)
nZ7 <- length(daneZ7)

funZ7 <- function(params){
  alpha <- params[1]
  x <- daneZ7
  M <- rbind(1/nZ7 * sum(x) - alpha,
             1/nZ7 * sum(x^2) - alpha * (alpha+1))
  W <- matrix(0, nrow = 2, ncol = 2)
  W[1, 1] <- 1/nZ7 * sum((x - alpha)^2)
  W[1, 2] <- 1/nZ7 * sum((x^2 - alpha) * (x^2 - alpha*(alpha+1)))
  W[2, 1] <- W[1, 2]
  W[2, 2] <- 1/nZ7 * sum((x^2 - alpha * (alpha+1))^2)
  -(t(M) %*% solve(W) %*% M)
}

# should benear mean of data
maxLik::maxNR(funZ7, start = 3)$estimate

# Zadanie 8

daneZ8 <- read.csv('PNA_Z09/GammaBothParm.csv')$x
meanZ8 <- mean(daneZ8)
varZ8 <- var(daneZ8)
nZ8 <- length(daneZ8)

funZ7 <- function(params){
  alpha <- params[1]
  beta <- params[2]
  x <- daneZ8
  n <- nZ8
  M <- rbind(1/n * sum(x) - alpha/beta,
             1/n * sum(x^2) - (alpha * (alpha+1) / beta^2),
             1/n * sum(1/x) - beta / (alpha - 1))
  W <- matrix(0, nrow = 3, ncol = 3)
  W[1, 1] <- 1/n * sum((x - alpha/beta)^2)
  W[1, 2] <- 1/n * sum((x - alpha/beta) * (x^2 - alpha*(alpha+1) / beta^2))
  W[2, 1] <- W[1, 2]
  W[1, 3] <- 1/n * sum((x - alpha/beta) * (1/x - beta / (alpha - 1)))
  W[3, 1] <- W[3, 1]
  W[2, 3] <- 1/n * sum(x^2 - (alpha * (alpha+1) / beta^2))
  W[3, 2] <- W[2, 3]
  W[2, 2] <- 1/n * sum((x^2 - alpha * (alpha+1)/beta^2)^2)
  W[3, 3] <- 1/n * sum((1/x - beta / (alpha - 1))^2)
  -(t(M) %*% solve(W) %*% M)
}

maxLik::maxNR(funZ7, start = c(5, 6))$estimate
