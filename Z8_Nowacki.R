# Zadanie 1 

dataZ1 <- c(5,9,6,4,1)
meanZ1 <- mean(dataZ1)

betaZ1 <- mean(dataZ1) * 2 

# Zadanie 3

dataZ3 <- read.csv("PNA_Z08/led.csv")$x
lambdaZ3 <- 1 / mean(dataZ3)

stats::ks.test(dataZ3, 'pexp', lambdaZ3)

#homework - do using stats::ecdf

# Zadanie 4

dataZ4 <- read.csv('PNA_Z08/gret.csv')$x
muZ4 <- mean(dataZ4)
sigmaZ4 <- var(dataZ4)

statZ4 <- ks.test(dataZ4, 'pnorm', 0, sqrt(sigmaZ4))

# Zadanie 5

dataZ5 <- read.csv('PNA_Z08/king.txt')


# Zadanie 7 

dataZ7 <- read.csv('PNA_Z08/lhur2.csv', header = TRUE, sep = ";")$Lhur
nZ7 <- length(dataZ7)
tableZ7 <- table(dataZ7)
catZ7 <- tableZ7 |> names() |> as.numeric()
empiricalZ7 <- tableZ7 |> as.vector()

pmm <- mean(dataZ7) / var(dataZ7)
rmm <- mean(dataZ7) / (1 - pmm) * pmm

kZ7 <- length(catZ7)

theoreticalZ7 <- dnbinom(catZ7, rmm, pmm) * nZ7

testStatisticZ7 <- sum((empiricalZ7 - theoreticalZ7)^2 / theoreticalZ7)
pvalueZ7 <- 1 - pchisq(testStatisticZ7, df = kZ7 - 1) # 0.2392913

# Zadanie 9

dataZ9 <- read.csv('PNA_Z08/exp.csv', sep=';', dec=',')$x

meanZ9 <- mean(dataZ9)

lambdaZ9 <- 1 / meanZ9

