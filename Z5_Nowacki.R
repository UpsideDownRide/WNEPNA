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

# Zadanie 7

daneZ7 <- c(3, 4, 3, 2, 7)

likGeom <- \(p) (1 - p)^(sum(daneZ7-1))*p^(length(daneZ7))
logLikGeom <- \(p) sum(daneZ7 - 1) * log(1 - p) + length(daneZ7) * log(p)
maxLik::maxNR(logLikGeom, start=0)
