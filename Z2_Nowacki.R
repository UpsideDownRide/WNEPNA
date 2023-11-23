## Funkcje pomocnicze

model1CI <- function(mean, alpha, n, sd) {
  quantile <- qnorm(p = 1 - alpha / 2)
  delta <- quantile * sd / sqrt(n)
  c(mean - delta, mean + delta)
}

model2CI <- function(mean, alpha, n, sd) {
  quantile <- qt(1 - alpha / 2, n - 1)
  delta <- quantile * sd / sqrt(n)
  c(mean - delta, mean + delta)
}

model5CI <- function(proportion, alpha, n) {
  quantile <- qnorm(1 - alpha / 2)
  delta <- quantile * sqrt(proportion * (1 - proportion)) / sqrt(n)
  c(proportion - delta, proportion + delta)
}

testStatistic <- function(values, hypothesisMean) {
  sampleMean <- values |> mean()
  sampleSD <- values |> sd()
  n <- values |> length()
  sampleSE <- sampleSD / sqrt(n)
  (sampleMean - hypothesisMean) / sampleSE
}

biasedPooledVariance <- function(listOfVectors) {
  stopifnot(typeof(listOfVectors) == 'list')
  stopifnot(length(listOfVectors) >= 2)
  listOfVectors |> vapply(typeof, 'char') %in% c('double', 'integer') |> stopifnot()
  
  lengths <- vapply(listOfVectors, length, 1)
  variances <- vapply(listOfVectors, var, 1)
  
  sum(lengths * variances) / (sum(lengths - 1))
}

unbiasedPooledVariance <- function(listOfVectors) {
  stopifnot(typeof(listOfVectors) == 'list')
  stopifnot(length(listOfVectors) >= 2)
  listOfVectors |> vapply(typeof, 'char') %in% c('double', 'integer') |> stopifnot()
  
  lengths <- vapply(listOfVectors, length, 1) - 1
  variances <- vapply(listOfVectors, var, 1)
  
  sum(lengths * variances) / sum(lengths)
}

bartlettTestStatistic <- function(listOfVectors) {
  stopifnot(typeof(listOfVectors) == 'list')
  stopifnot(length(listOfVectors) >= 2)
  listOfVectors |> vapply(typeof, 'char') %in% c('double', 'integer') |> stopifnot()
  
  N <- listOfVectors |> vapply(length, 1) |> sum()
  k <- length(listOfVectors)
  pooledVariance <- unbiasedPooledVariance(listOfVectors)
  numerator <- (N - k) * log(pooledVariance) - (vapply(listOfVectors, \(x) (length(x) - 1) * log(var(x)), 1) |> sum())
  denominator <- 1 + (1 / (3 * (k - 1))) * (vapply(listOfVectors, \(x) (1 / (length(x) - 1)), 1) |> sum() - 1 / (N - k))
  numerator / denominator
}

# Zadanie 3.1

nZ31 <- 20
meanZ31 <- 14.5
sdZ31 <- 5.6
alphaZ31 <- 0.05

## 12.04574 16.95426
resultZ31 <- model1CI(mean = meanZ31, alpha = alphaZ31, sd = sdZ31, n = nZ31)

# Zadanie 3.2

nZ32 <- 10
meanZ32 <- 24.4
sdZ32 <- 3.5
alphaZ32 <- 0.1

## 22.37112 26.42888
resultZ32 <- model2CI(mean = meanZ32, alpha = alphaZ32, sd = sdZ32, n = nZ32)

# Zadanie 3.3

nZ33 <- 400
over60nZ33 <- 144
proportionZ33 <- over60nZ33 / nZ33
alphaZ33 <- 0.05

## 0.3129609 0.4070391
resultZ33 <- model5CI(proportion = proportionZ33, alpha = alphaZ33, n = nZ33)

# Zadanie 3.4

nZ34 <- 590
IBMZ34 <- 500
proportionZ34 <- IBMZ34 / nZ34
alphaZ34 <- 0.05

## 0.8184457 0.8764695
resultZ34 <- model5CI(proportion = proportionZ34, alpha = alphaZ34, n = nZ34)

# Zadanie 4.1

sdZ41 <- 50
meanZ41 <- 1025
alphaZ41 <- 0.02
nZ41 <- 25

testValueZ41 <- 1000

## z-score = 2.5, obszar krytyczny = [2.053749, +inf], odrzucamy H_0 
testStatisticZ41 <- (meanZ41 - testValueZ41) / (sdZ41 / sqrt(nZ41))
compareStatisticZ41 <- qnorm(1 - alphaZ41)

# Zadanie 4.2

valuesZ42 <- c(23.3, 22.1, 21.8, 19.9, 23.7, 22.3, 22.6, 21.5, 21.9, 22.8, 23.0, 22.2)
alphaZ42 <- 0.05
testValueZ42 <- 22.6

## t-score = -1.204037, obszar krytyczny = [1.795885, +inf], nie ma przesłanek do odrzucenia H_0
testStatisticZ42 <- testStatistic(valuesZ42, testValueZ42)
compareStatisticZ42 <- qt(p = 1 - alphaZ42, df = valuesZ42 |> length() - 1)

# Zadanie 4.3

valuesZ43 <- c(3570, 3700, 3650, 3590, 3720, 3710, 3550, 3720, 3580, 3630)
alphaZ43 <- 0.05
testValueZ43 <- 3600

## t-score = 1.978434, obszar krytyczny = [-inf, -2.262157] + [2.262157, +inf], nie ma przesłanek do odrzucenia H 
testStatisticZ43 <- testStatistic(valuesZ43, testValueZ43)
compareStatisticZ43 <- qt(p = 1 - alphaZ43 / 2, df = 9)

# Zadanie 4.4

valuesZ44 <- c(9, 12, 11, 10, 11, 9, 11, 12, 9, 10)
alphaZ44 <- 0.05
testValueZ44 <- 10

## t-score = 1.077632, obszar krytyczny = [1.833113, +inf], nie ma przesłanek do odrzucenia hipotezy o poziomie zawartości azotanu w workach w proporcji 10% 
testStatisticZ44 <- testStatistic(valuesZ44, testValueZ44)
compareStatisticZ44 <- qt(p = 1 - alphaZ44, df = valuesZ44 |> length() - 1)

# Zadanie 4.5

valuesZ45 <- c(4.5, 3.6, 6.0, 6.4, 7.9, 6.9, 6.1, 7.4, 9.0, 4.3, 6.1, 8.2, 4.9, 7.5, 5.8)
alphaZ45 <- 0.05
testValueZ45 <- 2
dfZ45 <- valuesZ45 |> length() - 1

## chi-score = 16.69467, obszar krytyczny = [23.68479, +inf], nie ma przesłanek do odrzucenia H
testStatisticZ45 <- dfZ45 * ( valuesZ45 |> var() ) / testValueZ45
compareStatisticZ45 <- qchisq(p = 1 - alphaZ45, df = dfZ45)

# Zadanie 5.1

valuesListZ51 <- list(first = c(7.60, 7.81, 8.01, 7.95, 7.15, 8.06, 7.90, 7.91, 7.56, 7.62, 7.85, 8.02),
                      second = c(7.50, 7.90, 8.00, 7.17, 7.28, 7.35, 7.73, 7.20, 7.98))
alphaZ51 <- 0.05

## bartlett K-squared = 4.256929, obszar krytyczny [0.5908592, +inf]
## nie ma przeslanek do odrzucenia hipotezy o rownosci wariancji

testStatisticZ51 <- bartlettTestStatistic(valuesListZ51)
compareStatisticZ51 <- qt(p = 1 - alphaZ51, df = length(valuesListZ51))

# Zadanie 5.2

valuesZ52 <- list(trained = c(18.6, 17.9, 18.1, 17.0, 18.7, 18.3),
                  untrained = c(17.3, 17.6, 17.1, 16.0, 17.8))
alphaZ52 <- 0.05

## male n, zakladam rozklad normalny, model B
## t-score = 2.137939, obszar krytyczny = [1.833113, +inf], odrzucamy H

testStatisticZ52 <- (mean(valuesZ52$trained) - mean(valuesZ52$untrained)) / 
  (sqrt(biasedPooledVariance(valuesZ52) * (valuesZ52 |> vapply(\(x) 1/length(x), 1) |> sum())))
compareStatisticZ52 <- qt(1 - alphaZ52, vapply(valuesZ52, length, 1) |> sum() - 2)

# Zadanie 5.3

valuesZ53 <- list(first = c(3.71, 4.28, 2.95, 3.20, 3.38, 4.05, 4.07, 4.98, 3.20, 3.43, 3.09, 4.50, 3.12, 3.68, 3.9),
                  second = c(3.10, 3.38, 4.06, 3.60, 3.81, 4.50, 4.00, 3.25, 4.11, 4.85, 2.80, 4.0))
alphaZ53 <- 0.05

## t-score = -0.360701, obszar krytyczny [-inf, -2.05954] + [2.05954, +inf]
## nie ma podstaw do odrzucenia hipotezy o jednakowości ocen
testStatisticZ53 <- (mean(valuesZ53$first) - mean(valuesZ53$second)) / 
  (sqrt(biasedPooledVariance(valuesZ53) * (valuesZ53 |> vapply(\(x) 1/length(x), 1) |> sum())))
compareStatisticZ53 <- qt(1 - alphaZ53 / 2, vapply(valuesZ53, length, 1) |> sum() - 2)

# Zadanie 5.4
valuesZ54 <- list(viscose = c(122.4, 118.0, 120.0, 116.0, 120.8),
                 polyamide = c(73.6, 73.4, 79.4, 73.9),
                 acrylic = c(254.7, 243.2, 248.6, 236.0, 245.6))
alphaZ54 <- 0.05

## bartlett K-squared = 4.256929, obszar krytyczny [5.991465, +inf]
## nie ma przeslanek do odrzucenia hipotezy o rownosci wariancji
testStatisticZ54 <- bartlettTestStatistic(valuesZ54)
compareStatisticZ54 <- qchisq(1 - alphaZ54, length(valuesZ54) - 1)


# Zadanie 5.5
valuesZ55 <- list(first = c(19, 16, 22, 20, 23, 18, 16),
                  second = c(24, 21, 18, 24, 35, 33, 15),
                  third = c(54, 74, 43, 47, 60, 67, 52))

# W zalozeniach jest mowa o rozkladzie lognormalnym liczby cykli - bierzemy logarytm aby
# spelnic zalozenia testu Bartletta

normalizedZ55 <- valuesZ55 |> lapply(log)
alphaZ55 <- 0.05

## bartlett K-squared = 3.279094, obszar krytyczny [5.991465, +inf]
## nie ma przeslanek do odrzucenia hipotezy o rownosci wariancji
testStatisticZ55 <- bartlettTestStatistic(normalizedZ55)
compareStatisticZ55 <- qchisq(1 - alphaZ55, length(normalizedZ55) - 1)
