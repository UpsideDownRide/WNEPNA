maxLikWrapper <- function(data, logLikFun, iterStart, gradLikFun = NULL, hessLikFun = NULL, ...) {
  gradLikFun <- if (is.function(gradLikFun)) gradLikFun(data) else NULL
  hessLikFun <- if (is.function(hessLikFun)) hessLikFun(data) else NULL
  
  tryCatch(
    {
      result <- maxLik::maxLik(logLik = logLikFun(data), start = iterStart, grad = gradLikFun, hess = hessLikFun, ...)
    },
    error = \(condition) {
      message("Estimation process had an error")
      message(conditionMessage(condition))
    },
    warning = \(condition) {
      message("Estimation process had warning(s)")
      message(conditionMessage(condition))
    }
  )
  
  result
}

llSingleParamTest <- function(data, logLikFun, toTest, iterStart, gradLikFun = NULL, hessLikFun = NULL) {
  result <- maxLikWrapper(data, logLikFun, iterStart, gradLikFun, hessLikFun)  
  estimate <- result$estimate
  var <- -solve(result$hessian - 0) / sqrt(length(data))
  statistic <- (estimate - toTest) / sqrt(var)
  pvalue <- 2 * (1 - pnorm(abs(statistic)))
  names(var) <- names(statistic) <- names(pvalue) <- NULL
  
  returnList <- list(estimate = estimate, var = var, statistic = statistic, `p-value` = pvalue, maxLik = result) 
  returnList
}

lrTest <- function(data, logLikFun, toTest, iterStart, gradLikFun = NULL, hessLikFun = NULL) {
  result <- maxLikWrapper(data, logLikFun, iterStart, gradLikFun, hessLikFun)
  estimates <- result$estimate
  lr <- 2 * (result$maximum - logLikFun(data)(toTest))
  pvalue <- 1 - pchisq(lr, df = length(toTest))
  names(lr) <- names(pvalue) <- NULL
  
  returnList <- list(estimate = estimates, LR = lr, `p-value` = pvalue, maxLik = result) 
  returnList
}

### Zadanie 1

dataZ1 <- c(-1.13,-0.62,0.06,-1.82,-0.27,-0.74,0.49,-0.35,-0.41,-0.05,-0.20,0.06,-1.06,0.58,1.20, 1.59,0.20,0.65,-0.53,-0.73,-1.16,1.34,1.77,0.70,-0.10,-0.30,0.60,0.49,-0.45,-1.15,-0.31,1.30,0.91,0.47,-0.44,1.23,-0.15,1.22,-1.26,-0.18)

logLikNorm <- function(data) { function(params){
  n <- length(data)
  mu <- params[1]
  sigma <- params[2]
  
  -0.5 * n * log(2*pi) - n * log(sigma) - (1 / (2 * sigma^2)) * sum((data - mu)^2) 
}}

logLikNormSigma1 <- \(data) \(mu) logLikNorm(data)(c(mu = mu, sigma = 1))

resultsZ1 <- llSingleParamTest(dataZ1, logLikNormSigma1, toTest = 0, iterStart = 0)

# 0.03625
resultsZ1$estimate

# 0.577
resultsZ1$statistic

# 0.564 - Nie ma podstaw do odrzucenia H0 \mu = 0
resultsZ1$`p-value`

### Zadanie 2

dataZ2 <- c(0,0,3,1,3,11,0,7,2,3,4,2,0,1,2,1,3,2,4,2,1,4,6,4,0,6,0,0,4,2)

logLikGeom <- function(data) { function(param){
  n <- length(data)
  p <- param
  n * log(p) - n * log(1 - p) + log(1 - p) * sum(dataZ2)
}}

resultsZ2 <- llSingleParamTest(dataZ2, logLikGeom, toTest = 0.25, iterStart = 0.5)

# 0.3846154
resultsZ2$estimate

# 5.719264
resultsZ2$statistic

# ~0 - odrzucamy H0 p = 0.25
resultsZ2$`p-value`

### Zadanie 3

dataZ3 <- read.csv('./PNA_Z07/ratings_Musical_Instruments.csv', sep=';')$Ratings

logLikPoiss <- function(data) { function(l) {
  n <- length(data)
  
  logFactorialSum <- data |>
    factorial() |>
    (\(x) replace(x, x == Inf, .Machine$double.xmax))() |>
    log() |>
    sum()
  
  -n*l - n * log(1 - exp(-l)) + log(l) * sum(data) - logFactorialSum
}}

# llPois <- \(data, n, l) -n*l - n * log(1 - exp(-l)) + log(l) * sum(data) - sum(log(factorial(data)))
# Deriv::Deriv(llPois, 'l')
#
#gradLikPoiss <- function(data) { function(l) {
#  n <- length(data)
#  .e2 <- exp(-l)
#  
#  sum(data)/l - n * (1 + .e2/(1 - .e2))
#}}
#
# Deriv::Deriv(llPois, 'l', nderiv = 2)
#
#hessLikPoiss <- function(data) { function(l){
#  n <- length(data)
#  .e2 <- exp(-l)
#  .e3 <- 1 - .e2
#  
#  -(sum(data)/l^2 - n * (1 + .e2/.e3) * .e2/.e3)
#}}

resultsZ3 <- llSingleParamTest(dataZ3, logLikPoiss, toTest = 6, iterStart = 3)

# 6.008062
# Nienajgorzej versus sum(dataZ3) / length(dataZ3) = 6.022879
resultsZ3$estimate

# 1.986974e-07
resultsZ3$var

# 16.08899 ???
resultsZ3$statistic

# 0
# Odrzucamy (???) H0 \lambda = 6
resultsZ3$`p-value`

# Sanity check(?) - llSingleParamTest(dataZ3, logLikPoiss, toTest = 6.009, iterStart = 3)
# p-value - 0.0634933

### Zadanie 4

dataZ4 <- resampledata::Quakes$TimeDiff

logLikWeibull <- function(data) { function(params) {
  k <- params[1]
  l <- params[2]
  n <- length(data)
  
  n * log(k) - k * n * log(l) + (k - 1) * sum(log(data)) - sum((data / l)^k)
}}

# Estimates
# k: 0.9171893 \lambda: 17.3458319

# LR: 18.29242
# p: 0.000106623
# Odrzucamy H0 k:1 & \lambda = 20

lrTest(dataZ4, logLikWeibull, toTest = c(k = 1, l = 20), iterStart = c(0.5, 1))

### Zadanie 5

dataZ5 <- resampledata::Service$Times

logLikGamma <- function(data) { function(params){
  k <- params[1]
  theta <- params[2]
  n <- length(data)
  
  (k - 1) * sum(log(data)) - 1/theta * sum(data) - n * k * log(theta) - n * log(gamma(k))
}}

# LR: 219.9829
# p-value: 0
# Odrzucamy H0: k = 1 & \theta = 2

lrTest(dataZ5, logLikGamma, toTest = c(1,2), iterStart = c(0.5, 1))

### Zadanie 6

dataZ6 <- read.csv('./PNA_Z07/contest_data.csv')$prize

gradLikNorm <- function(data){ function(params) {
  mu <- params[1]
  sigma <- params[2]
  n <- length(data)
  normLogLik <- \(data, n, mu, sigma) -0.5 * n * log(2*pi) - n * log(sigma) - (1 / (2 * sigma^2)) * sum((data - mu)^2)
  
  c(Deriv::Deriv(normLogLik, 'mu')(data, n, mu, sigma), Deriv::Deriv(normLogLik, 'sigma')(data, n, mu, sigma))
}}

# Nie zbiega sie ladnie bez gradientu
# mu: 10211.16, sigma: 10179.37
nonZeroZ6 <- dataZ6[dataZ6 > 0]
resultZ6 <- maxLik::maxLik(logLikNorm(nonZeroZ6), gradLikNorm(nonZeroZ6), start = c(1, 1))

###  Zadanie 7

dataZ7 <- c(0.62,-2.52,-0.31,-0.73,2.54,-1.52,-1.18,2.06,2.53,2.52,0.66,0.02,-0.93,-0.09,0.81 -1.60,2.70,0.52,1.75,-0.79,3.66,-1.05,-1.32,-2.42,0.41,-2.09,2.67,1.36,0.94,0.58,-0.40,1.91,0.18,1.41,4.56,-0.01,-1.60,-0.07,1.79,2.23,0.52,-2.81,-1.74,0.71,2.09,2.25,1.33,0.37,-2.04,2.29)

logLikNormSigma2 <- \(data) \(mu) logLikNorm(data)(c(mu = mu, sigma = 2))

# mu: 0.4646939
# LR: 2.64527 
# p-value: 0.10386
# Nie ma podstaw do odrzucenia H0: \mu = 0
lrTest(dataZ7, logLikNormSigma2, toTest = 0, iterStart = 1)

### Zadanie 8

dataZ8 <- read.csv('./PNA_Z07/contest_data.csv')$prize

wrapLikZ8 <- function(data) { function(params) {
  mu <- params[1]
  sigma <- params[2]
  nZero <- length(data[data == 0])
  zeroFun <- nZero * log(1 - pnorm(mu / sigma))
  
  logLikNorm(data[data > 0])(params) + zeroFun
}}

wrapGradZ8 <- function(data) { function(params) {
  mu <- params[1]
  sigma <- params[2]
  ratio <- mu / sigma
  nZero <- length(data[data == 0])
  zeroFun <- c(nZero * -dnorm(ratio) / sigma * (1 - pnorm(ratio)), nZero * mu * dnorm(ratio) / (sigma^2 * (1 - pnorm(ratio))))
  
  gradLikNorm(data[data > 0])(params) + zeroFun
}}

### Wyszla mi bzdura :(
### mu: 8.2922645 sigma: 0.9999914
resultZ8 <- maxLik::maxLik(wrapLikZ8(dataZ8), wrapGradZ8(dataZ8), start = c(1, 1))

