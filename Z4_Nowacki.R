# Zadanie 1.1

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

z1DataDirectory <- "./PNA_Z04/"
z1DataNames <- c("vec1", "vec2", "vec3", "vec4")
z1Data <- lapply(z1DataNames, \(name) read.csv(paste0(z1DataDirectory, name, ".csv")))
names(z1Data) <- z1DataNames
z1Results <- lapply(z1Data, \(df) testJB(df$x))

# > z1Results
# $vec1
# Test Statistic        p-value         reject 
# 0.4686917      0.7910882      0.0000000 
# 
# $vec2
# Test Statistic        p-value         reject 
# 3.956425e+01   2.562909e-09   1.000000e+00 
# 
# $vec3
# Test Statistic        p-value         reject 
# 0.2759953      0.8711007      0.0000000 
# 
# $vec4
# Test Statistic        p-value         reject 
# 3427.966          0.000          1.000 

# Odrzucamy założenie o normalności rozkładu w przypadku danych vec2 i vec4, brak podstaw do odrzucenia w przypadku vec1 i vec3

# Zadanie 1.2
TO_COLUMN <- 2
rejectPercentage <- \(generator) replicate(10^4, generator()) |> apply(TO_COLUMN, \(x) testJB(x)["reject"]) |> mean()

# 0.228 
studentGenerator <- \() rt(n=100, df=12)
rejectPercentage(studentGenerator)

# Zadanie 1.3

# 0.0308
normalGenerator <- \() rnorm(n=30, mean=4, sd=2)
rejectPercentage(normalGenerator)

# Zadanie 2.2
dataZ2 <- data.frame(price = c(1, 2, 3), lpg = c(0, 1, 0))
modelZ2 <- lm(price ~ lpg)
summary(modelZ2)

# Zadanie 2.4

daneZ24 <- data.frame(x=c(1,2,3), y=c(1,2,1))

basicOLS <- function(x, y){
  xi <- cbind(intercept = 1, x) |> as.matrix()
  A <- solve(t(xi) %*% xi)
  
}
xi <- c(rep(1, length(daneZ24$x)), daneZ24$x) |>
  matrix(ncol = 2)
yi <- daneZ24$y

A <- solve(t(xi) %*% xi) 
beta <- A %*% t(xi) %*% yi
res <- yi - xi %*% beta
df <- nrow(xi) - (ncol(xi) - 1) - 1
resVar <- 1/df * sum(res^2)
covar <- resVar * A
covb0b1 <- covar[[1,2]] 

# Zadanie 2.5

daneZ25 <- read.csv('./PNA_Z04/vacation.csv')
modelZ25 <- lm(miles ~ income + age + kids, data = daneZ25)
betaslm <- modelZ25$coefficients

xii <- cbind(intercept = 1, daneZ25[2:4]) |> as.matrix()
yii <- daneZ25$miles
A <- solve(t(xii) %*% xii)
betas <- A %*% t(xii) %*% yii 

yhat <- xii %*% betas
residuals <- yii - yhat
varres <- sum(residuals^2) / (nrow(residuals) - ncol(xii))

Rsquared <- 1 - sum(residuals^2) / sum((yii-mean(yii))^2)

adjustedRsquared <- 1 - (nrow(yii) - 1) / (nrow(yii) - 4) * (1 - Rsquared)

#covariance
dfZ25 <- nrow(xii) - (ncol(xii) - 1) - 1
resVarZ25 <- 1/dfZ25 * sum(residuals^2)
covar <- resVarZ25 * A
covarb1b2 <- covar[2, 3]
covarb2b2 <- covar[3, 3]

#Zadanie 2.6

daneZ26 <- read.csv('./PNA_Z04/vacation.csv')
modelZ26 <- lm(miles - 10*age ~ income + kids, data = daneZ26)
summary(modelZ26)
