### Zadanie 1 

dataZ1 <- c(5, 9, 6, 4, 1)

# Unif(0, b) => mean = b/2 => b = mean * 2
# \beta = 10
betaZ1 <- mean(dataZ1) * 2 

### Zadanie 2

dataZ2 <- c(1, 1, 1, 9)

# \beta = 6
# Co oczywiście jest 'bzdura' bo mamy w probce obserwacje wieksza niz 6
betaZ2 <- mean(dataZ2) * 2

### Zadanie 3

dataZ3 <- read.csv("PNA_Z08/led.csv")$x
lambdaZ3 <- 1 / mean(dataZ3)

# 0.021415, p = 0.7488
# Nie ma podstaw do odrzucenia H0 o identycznosci rozkladow
stats::ks.test(dataZ3, 'pexp', lambdaZ3)

#homework - do using stats::ecdf

homebrewKS <- function(data, cdf) {
  stopifnot(is.function(cdf))
  ecdfD <- ecdf(data)
  sorted <- sort(data)
  
  max(abs(ecdfD(sorted) - cdf(sorted)))
}

# 0.020415
# Roznica miedzy ks.test a tym wynikiem to 1 / length(dataZ3) ???

homebrewKS(dataZ3, \(x) pexp(x, lambdaZ3))

#### Zadanie 4

dataZ4 <- read.csv('PNA_Z08/gret.csv')$x

# Pierwszy moment Ex = \mu, drugi moment Ex^2 = \mu^2 + \sigma^2
# Ex^2 = var(x) + (Ex)^2
# \mu = mean, \sigma^2 = 1/n * sum(x^2) - mean^2 = (n-1)/n * var

muZ4 <- mean(dataZ4)
varnZ4 <- (length(dataZ4) - 1) / length(dataZ4) * var(dataZ4)

# D = 0.0060575, p-value = 0.8566
# Nie ma podstaw do odrzucenia H0 o identycznosci rozkladow
statZ4 <- ks.test(dataZ4, 'pnorm', 0, sqrt(varnZ4))

### Zadanie 5

dataZ5 <- read.csv('PNA_Z08/king.txt', sep = " ")$x

# Poisson, pierwszy moment Ex = \lambda, drugi moment Ex^2 = \lambda^2 + \lambda

# \lambda = 6.94
lambdaZ5 <- mean(dataZ5)

homebrewChiFitTest <- function(data, cdf) {
  stopifnot(is.function(cdf))
  
  levels <- 0:max(data)
  observed <- table(factor(data, levels = levels))
  
  probabilities <- cdf(levels)
  
  expected <- probabilities * length(data)
  
  chistat <- sum((observed - expected)^2 / expected)
  pvalue <- 1 - pchisq(chistat, df = length(levels) - 1)
  
  list(statistic = chistat, `p-value` = pvalue)
}

# \chi^2 = 36.41195, p-value = 0.004039536
# Odrzucamy H0 o identycznosci rozkladow
resultZ5 <- homebrewChiFitTest(dataZ5, \(x) dpois(x, lambdaZ5))

### Zadanie 6

dataZ6 <- read.csv('PNA_Z08/negb.csv', sep = ",")$x
lambdaZ6 <- mean(dataZ6)

# \chi^2 = 535868.7, p-value = 0
# Modelowanie rozkladu za pomoca poissona jest zupelnie nieadekwatne
resultPoisZ6 <- homebrewChiFitTest(dataZ6, \(x) dpois(x, lambdaZ6))

# neg binom
# p = 0.3960485, r = 19.81779
pmmZ6 <- mean(dataZ6) / var(dataZ6)
rmmZ6 <- mean(dataZ6) / (1 - pmmZ6) * pmmZ6

# \chi^2 = 71.71283, p-value = 0.356
# Nie ma podstaw do odrzucenia H0 o identycznosci rozkladow
resultNegBinomZ6 <- homebrewChiFitTest(dataZ6, \(x) dnbinom(x, rmmZ6, pmmZ6))

### Zadanie 7 

dataZ7 <- read.csv('PNA_Z08/lhur2.csv', header = TRUE, sep = ";")$Lhur

# Neg binom mean = r * (1 - p) / p, var = r * (1 - p) / p^2
# Ex = r * (1 - p) / p, Ex^2 = r * (1 - p) / p^2 + r * (1 - p) / p^2

pmmZ7 <- mean(dataZ7) / var(dataZ7)
rmmZ7 <- mean(dataZ7) / (1 - pmmZ7) * pmmZ7

# \chi^2 = 34.72543, p-value = 0.2948103
resultZ7 <- homebrewChiFitTest(dataZ7, \(x) dnbinom(x, rmmZ7, pmmZ7))

### Zadanie 8
dataZ8 <- read.csv2('PNA_Z08/gosset.csv')$x

# min           max         mean           sd
# -6.65245967  5.91285256 -0.01294558  1.18772476
# pewnie rozklad normalny
baseStatsZ8 <- sapply(c(min, max, mean, sd), \(f) f(dataZ8))

meanZ8 <- mean(dataZ8)
varnZ8 <- length(dataZ8) / (length(dataZ8) - 1) * var(dataZ8)

# D = 0.025027, p-value = 0.00381
# Jednak nie, odrzucamy H0
ks.test(dataZ8, 'pnorm', meanZ8, sqrt(varnZ8))

# Troche graficznej eksploracji 
cdfZ8 <- \(x) pnorm(x, meanZ8, sqrt(varnZ8))

# Nazwa pliku to gosset wiedz pewnie rozklad T. 6 znalezione szukajac od gory i dzielac przez 100 i 1000, 5 prawie pasowalo, 6 lezy prawie idealnie?
tcdfZ8 <- \(x) pt(x, 6)

graphECDFvCDF <- function(data, cdf) {
  stopifnot(is.function(cdf))
  vals <- seq(min(data), max(data), length.out = length(data))
  plot(ecdf(data), main = "ECDF vs Theoretical CDF", xlab = "Value", ylab = "Cumulative Probability", col = "blue")
  lines(vals, cdf(vals), col = 'red')
}

# Pasuje wizualnie
graphECDFvCDF(dataZ8, tcdfZ8)

# D = 0.011449, p-value = 0.5286
# Nie ma podstaw do odrzucenia H0 o identycznosci rozkladow
ks.test(dataZ8, 'pt', 6)

### Zadanie 9

dataZ9 <- read.csv('PNA_Z08/exp.csv', sep=';', dec=',')$x

# Exponential, Ex = 1 / \lambda
meanZ9 <- mean(dataZ9)

lambdaZ9 <- 1 / meanZ9

### Zadanie 10
dataZ10 <- read.csv('PNA_Z08/zep.csv', sep = ",")$x

# Gamma with \alpha, \beta params. Ex = \alpha / \beta, Ex^2 = \alpha / \beta^2 + \alpha^2 / \beta^2
# mean = \alpha / \beta, var = \alpha / \beta^2
# \alpha = mean^2 / var, \beta = mean / var

ammZ10 <- mean(dataZ10)^2 / var(dataZ10)
bmmZ10 <- mean(dataZ10) / var(dataZ10)

