# Zadanie 1
# a)
curve(2*x-4, ylab = 'y')

# b)
curve(x^3 - 2*x^2 + x - 6, from = 0, to = 1, ylab = 'y')

# c)
curve(sin((10 * pi/3) * x), from = 0, to = 2*pi, ylab = 'y')

# Zadanie 2
goalData <- read.csv('./EngScoResults.csv', sep = ";")

# a)
plot(goalData$Rok, goalData$Goals.England, xlab = 'Year', ylab = 'England goals', col = 'pink', pch = 17, cex = 2)

# b)
plotWithOptions <- function(...) plot(..., xlab = 'Year', ylab = 'Goals.Scotland', col = 'darkgreen', pch = 19, cex = 2)
goalData |> subset(Host == 'Scotland', select = c('Goals.Scotland', 'Rok')) |>
  (\(x) plotWithOptions(x$Rok, x$Goals.Scotland))()

# Zadanie 3
quantileData <- read.csv('./PNA_Z03.csv') |> subset(select = 2, drop = TRUE) |> as.vector()
qqnorm(quantileData)
qqline(quantileData, col = 'deeppink', lwd = 3) 

# Zadanie 4
tSt <- rt(n = 1000, df = 3) |> sort()

# a)
qqnorm(tSt)
qqline(tSt, col = 'darkorange2', lwd = 3)

# b)
qqplot(qt(ppoints(1000), df = 3), tSt, xlab = 'Theoretical quantiles', ylab = 'Empirical quantiles',
       main = 'Q-Q Plot T distribution df=3')
qqline(tSt, distribution = \(x) qt(x, df = 3), col = 'orchid1', lwd = 3)

# Zadanie 5

wyk <- rexp(n = 1000, rate = 1)

qqplot(qgamma(ppoints(1000), rate = 1, shape = 1), wyk, xlab = 'Theoretical quantiles', ylab = 'Empirical quantiles',
       main = 'Q-Q Plot Exp(1) versus Gamma(1,1)')
qqline(wyk, distribution = \(x) qgamma(x, rate = 1, shape = 1), col = 'orchid1', lwd = 3)

# Zadanie 6

factorialWithMemo <- (function() {
  results <- vector()
  factorial <- function(n) {
    stopifnot(n >=0)
    stopifnot(n %% 1 == 0)
    if (n == 0) return(1)
    else if (n <= length(results) && !is.na(results[[n]])) return(results[[n]])
    else {
      results[[n]] <<- n * factorial(n-1)
      return(results[[n]])
    }
  }
  factorial
})()

# Zadanie 7

procentZer <- function(mat) {
  stopifnot(is.matrix(mat))
  stopifnot(mat %in% c(0, 1) |> all())
  sum(mat) / length(mat)
}

# Zadanie 8

poleProstokata <- function(a, b) a * b

# a)
poleProstokataDefault10 <- function(a, b = 10) poleProstokata(a, b)

# b)
poleProstokataDefaultSquare <- function(a, b = a) poleProstokata(a, b)

# Zadanie 9

testJB <- function(data, alpha = 0.05) {
  stopifnot(is.vector(data))
  stopifnot(is.numeric(alpha) && alpha > 0 && alpha <= 1)
  if(length(data) < 1000) warning("JB test for small samples is prone to false positives")
  
  n <- length(data)
  centered <- data - mean(data)
  
  skewnessNumerator <- 1/n * sum(centered^3)
  skewnessDenominator <- (1/n * sum(centered^2))^(3/2)
  skewness <- skewnessNumerator / skewnessDenominator
  
  kurtosisNumerator <- 1/n * sum(centered^4)
  kurtosisDenominator <- (1/n * sum(centered^2))^2
  kurtosis <- kurtosisNumerator / kurtosisDenominator
  
  testValue <- n/6 * (skewness^2 + 1/4 * (kurtosis - 3)^2)
  
  pValue <- 1 - pchisq(testValue, df = 2)
  
  c(`Test Statistic` = testValue, `p-value` = pValue, reject = pValue < alpha)
}

#  Zadanie 10

testJB(quantileData)

#Test Statistic        p-value         reject 
#     309.0726         0.0000         1.0000 
#We reject H0 that the data is normally distributed

# Zadanie 11
sapply(1:10^4, \(x) rt(100, df = 12) |> testJB() |> _['reject']) |> (\(x) sum(x)/length(x))()

# Zadanie 12
rnorm(30, mean = 4, sd = 2) |> testJB()

#Test Statistic        p-value         reject 
#       0.7385833      0.6912238      0.0000000 

# Zadanie 13

testDW <- function(model){
  res <- model |> residuals()
  sum(diff(res)^2) / sum(res^2)
}

# Zadanie 14

testLB <- function(model, lag = 1, alpha = 0.05){
  res <- model |> residuals()
  n <- model$model[[1]] |> length()
  
  rho <- function(k, x = res) {
    meanx <- mean(x)
    normalized <- x - meanx
    laggedNormal <- c(rep(NA, k), x[1:(length(x) - k)]) - meanx
    sum(normalized * laggedNormal, na.rm = TRUE) / sum(normalized^2)
  }
  
  obs <- sapply(1:lag, rho)
  
  testValue <- n * (n+2) * sum(1/seq.int(n - 1, n - lag) * obs^2)
  pValue <- 1 - pchisq(testValue, lag)
  c(`Test Value` = testValue, `p-value` = pValue, reject = pValue < alpha)
}
