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
catZ7 <- tableZ7 |> as.numeric()
licz <- tableZ7 |> as.vector()

pmm <- mean(dataZ7) / var(dataZ7)
rmm <- mean(dataZ7) / (1 - pmm) * pmm