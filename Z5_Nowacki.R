### Zadanie 1

dataZ1 <- c(0, 0, 0, 1, 0, 1, 0, 1)

p <- seq(0, 1, by = 0.01)
pmf <- p^5*(1-p)^3
plot(p, pmf, type='l')

logLikZ1 <- \(p) 5*log(p) + 3*log(1-p)

## Newton max algo
maxLik::maxNR(logLikZ1, grad = NULL, hess = NULL, start = 0.01)
maxLik::maxNR(logLikZ1, grad = \(p) 5/p - 3/(1-p), start = 0.01)
maxLik::maxNR(logLikZ1, grad = \(p) 5/p - 3/(1-p),
              hess = \(p) -5/p^2 - 3/(1-p)^2,
              start = 0.01)

optimize(logLikZ1, seq(0, 1, by = 0.01), maximum = TRUE)

### Zadanie 2

dataZ2 <- c(3, 4, 3, 7)
# \lambda^k \exp(-lambda) / k!
# likelihood - \prod_{i=1}^n \frac{l\lambda^{x_i} * \exp(-\lambda){x_i!}

likPoiss <- function(data){
  n <- length(data)
  \(l) prod(exp(-l) * l^(data) / factorial(data))
}

logLikPoiss <- function(data){
  n <- length(data)
  fact <- data |> sapply(factorial) |> log() |> sum()
  \(l) log(l) * sum(data) - n*l - fact
}

# 4.25
estimateZ2 <- maxLik::maxNR(logLikPoiss(daneZ2), start=1)$estimate

plotZ2 <- \(...) plot(..., type = 'l', lwd = 2, xlab = expression(lambda ~ ""),
                      ylab = expression(prod(frac(e^lambda~lambda^x[i], x[i]~"!") , i==1, N)),
                      mgp=c(1.5, 0.5, 0),
                      main="Likelihood function for Poisson given data")

seq(1, 8, by = 0.01) |> (\(x) plotZ2(x, sapply(x, likPoiss(dataZ2))))()
abline(v=estimateZ2, col = "deeppink")

### Zadanie 3
dataZ3 <- c(1.46, 0.81, 0.88, 0.53, 0.46)

likExp <- function(data){
  n <- length(data)
  \(l) l^n * exp(-l * sum(data))
}

logLikExp <- function(data){
  n <- length(data)
  \(l) n * log(l) - l * sum(data)
}

# 1.207729
estimateZ3 <- maxLik::maxNR(likExp(dataZ3), start = 1)$estimate

plotZ3 <- \(...) plot(..., type = 'l', lwd = 2, xlab = expression(lambda ~ ""),
                      ylab = expression(lambda^n~e^(-lambda~sum(x[i], i==1, N))),
                      mgp=c(1.5, 0.5, 0),
                      main="Likelihood function for exponential distribution given the data")

seq(0.5, 2, by = 0.05) |> (\(x) plotZ3(x, sapply(x, likExp(dataZ3))))()
abline(v = estimateZ3, col = "deeppink")

### Zadanie 4

## likelihood - \prod_i=1^n 1/\beta indicator(0:\beta) x_i
## beta_hat - max(x_i) - 4

dataZ4 <- c(2.13, 1.24, 2, 1.27, 0.04, 2.83)
n <- dataZ4 |> length()
beta <- seq(0.01, 5, by = 0.01)
L <- 1/beta^n

indices <- which(beta < max(dataZ4))
L[indices] <- 0
plot(beta, L, type='l')

### Zadanie 5

dataZ5 <- c(1, 2, 2, 3, 4)

likCauchy <- function(data) {
  n <- length(data)
  \(t) 1 / (pi^n * prod(1 + (data - t)^2))
}

logLikCauchy <- function(data) {
  n <- length(data)
  \(t) -n * log(pi) - sum(log(1 + (data - t)^2))
}

# 2.2212
estimateZ5 <- maxLik::maxNR(logLikCauchy(dataZ5), start = 1)$estimate

plotZ5 <- \(...) plot(..., type = 'l', lwd = 2, xlab = expression(lambda ~ ""),
                      ylab = "Likelihood",
                      mgp=c(1.5, 0.5, 0),
                      main="Likelihood function for Cauchy distribution given the data")

seq(0.5, 4, by = 0.05) |> (\(x) plotZ5(x, sapply(x, likCauchy(dataZ5))))()
abline(v = estimateZ5, col = "deeppink")

### Zadanie 6

dataZ6 <- c(51, 115, 150, 190, 217, 228, 350)

# 0.005380477
estimateZ6 <- maxLik::maxNR(logLikExp(dataZ6), start = 0.01)$estimate

# 185.8571
hoursZ6 <- 1 / estimateZ6

### Zadanie 7

dataZ7 <- c(3, 4, 3, 2, 7)

likGeom <- function(data) {
  n <- length(data)
  \(p) (1 - p)^(sum(data - 1)) * p^n
}

logLikGeom <- function(data) {
  n <- length(data)
  \(p) sum(data - 1) * log(1 - p) + n * log(p)
}

# 0.2631579
estimateZ7 <- maxLik::maxNR(logLikGeom(dataZ7), start = 0.5)$estimate

### Zadanie 8

dataZ8 <- read.csv('./PNA_Z05/Auld_Enemy.txt', sep = '\t')

# 1.462264
estimateZ8 <- maxLik::maxNR(logLikPoiss(dataZ8$Goals.England), start = 1)$estimate

### Zadanie 9

dataZ9 <- read.csv('./PNA_Z05/Purse_Snatch.txt', header = FALSE)$V1

# 13.77467
estimateZ9 <- maxLik::maxNR(logLikPoiss(dataZ9), start=1)$estimate

lambdasZ9 <- seq(0, 100, by = 0.01)

plot(lambdasZ9, likPoiss(dataZ9)(lambdasZ9), type = 'l')
abline(v = estimateZ9, col = 'deeppink')

qqplot(rpois(length(dataZ9), estimateZ9), dataZ9)
qqline(dataZ9, distribution=\(p) qpois(p, estimateZ9), probs = c(0.01, 0.99))
