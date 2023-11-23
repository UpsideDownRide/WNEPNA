# Zadanie 2.4

daneZ24 <- data.frame(x=c(1,2,3), y=c(1,2,1))
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
betas <- A %*% t(xii) %*% daneZ25$miles

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
