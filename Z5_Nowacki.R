# Zadanie 1

dataZ1 <- c(0, 0, 0, 1, 0, 1, 0, 1)

p <- seq(0, 1, by = 0.01)
pmf <- p^5*(1-p)^3
plot(p, pmf, type='l')

logLik <- \(p) 5*log(p) + 3*log(1-p)

## Newton max algo
maxLik::maxNR(logLik, grad = NULL, hess = NULL, start = 0.01)
maxLik::maxNR(logLik, grad = \(p) 5/p - 3/(1-p), start = 0.01)
maxLik::maxNR(logLik, grad = \(p) 5/p - 3/(1-p),
              hess = \(p) -5/p^2 - 3/(1-p)^2,
              start = 0.01)

optimize(logLik, seq(0, 1, by = 0.01), maximum = TRUE)

# Zadanie 2

daneZ2 <- c(3, 4, 3, 7)
# \lambda^k \exp^(-lambda) / k!
# likelihood - \prod_{i=1}^n \frac{l\lambda^{x_i} * \exp^{-\lambda}}{x_i!}
n <- length(daneZ2)

logLikPoiss <- \(l) log(l) * sum(daneZ2) - n*l + log(1/prod(sapply(daneZ2, factorial)))
maxLik::maxNR(logLikPoiss, start=4)

# Zadanie 4

## likelihood - \prod_i=1^n 1/\beta indicator(0:\beta) x_i
## beta_hat - max(x_i) - 4

dataZ4 <- c(2.13, 1.24, 2, 1.27, 0.04, 2.83)
n <- dataZ4 |> length()
beta <- seq(0.01, 5, by = 0.01)
L <- 1/beta^n
plot(beta, L, type='l')
indices <- which(beta < max(dataZ4))
L[indices] <- 0
plot(beta, L, type='l')

# Zadanie 7

daneZ7 <- c(3, 4, 3, 2, 7)

likGeom <- \(p) (1 - p)^(sum(daneZ7-1))*p^(length(daneZ7))
logLikGeom <- \(p) sum(daneZ7 - 1) * log(1 - p) + length(daneZ7) * log(p)
maxLik::maxNR(logLikGeom, start=0)

# zadanie 9

daneZ9 <- read.csv('./PNA_Z05/Purse_Snatch.txt', header = FALSE)$V1
nZ9 <- daneZ9 |> length()
logLikPoissZ9 <- \(l) log(l) * sum(daneZ9) - nZ9*l
estimateZ9 <- maxLik::maxNR(logLikPoissZ9, start=1)$estimate
lambdaZ9 <- seq(0, 100, by = 0.01)
pmfZ9 <- sum(daneZ9) * log(lambdaZ9) - nZ9*lambdaZ9

plot(lambdaZ9, pmfZ9, type = 'l')
abline(v = estimateZ9, col = 'blue')

qqplot(rpois(nZ9, estimateZ9), daneZ9)
qqline(daneZ9, distribution=\(p) qpois(p, estimateZ9), probs = c(0.01, 0.99))
