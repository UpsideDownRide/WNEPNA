options(scipen=999)

# Zadanie 2

a <- log(10) |> rep(7) |> matrix()
b <- c(1/3, 1/2, 1/6, 1/7) |> matrix()
c <- pi*c(1, 1/3, 1/5, 1/7) |> matrix()
A <- matrix(0, 3, 3)
B <- matrix(1, 4, 4)

d <- c(7,10,9,-5,6,-14,8,-19,-9,-19,-17,12) |> matrix()

# Zadanie 3

s <- c(1, 2, 3, 4, 5) + 100

# Zadanie 4

sum_z4 <- (1:300)^2 |> sum()

# Zadanie 5

zeroElementsOfD <- which(d == 0)
positiveElementsOfD <- which (d > 0)
negativeElementsOfD <- which (d < 0)
countOfNonNegativeInD <- which(d >= 0) |> length()
d2 <- d[d > 0]
d3 <- d[d < 0]
d4 <- replace(d, d < 0, 10^9-1)

# Zadanie 6

g <- c(1, 1, -1, 1, 1, 0, 2) |> matrix()
gSum <- g |> sum()
gProd <- g |> prod()
gWhichTwo <- which(g == 2)
gWhichMinusOne <- which(g == -1)
gNonPositiveIndices <- which(g <= 0)
gNonPositiveCount <- gNonPositiveIndices |> length()
gAllPositive <- all(g > 0)
gAllLessThan100 <- all(g < 100)
gAnyNA <- g |> is.na() |> any()

# Zadanie 7

Z7_cols <- 11
Z7_rows <- 11
Z7_matrix <- diag(1, Z7_rows - 1) |> cbind(matrix(0, nrow = Z7_rows - 1)) |>
  (\(x) rbind(matrix(-1, ncol = Z7_cols), x))()

# Zadanie 8

outerMatrix <- (1:10) %o% (1:10)

