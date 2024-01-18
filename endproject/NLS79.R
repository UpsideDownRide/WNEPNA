new_data <- read.table('./endproject/NLS79.dat', sep=' ')
names(new_data) <- c('R0000100',
'R0214800',
'T8089600',
'T8089800',
'T8218700',
'T8219400')

# Handle missing values

new_data[new_data == -1] = NA  # Refused
new_data[new_data == -2] = NA  # Dont know
new_data[new_data == -3] = NA  # Invalid missing
new_data[new_data == -4] = NA  # Valid missing
new_data[new_data == -5] = NA  # Non-interview


# If there are values not categorized they will be represented as NA

vallabels = function(data) {
  data$R0214800 <- factor(data$R0214800,
levels=c(1.0,2.0),
labels=c("MALE",
"FEMALE"))
  data$T8219400 <- factor(data$T8219400,
levels=c(40.0,41.0,42.0,43.0,44.0,45.0,46.0,47.0,48.0,49.0,50.0,51.0,52.0,53.0,54.0,55.0,56.0,57.0,58.0,59.0,60.0,61.0,62.0),
labels=c("40",
"41",
"42",
"43",
"44",
"45",
"46",
"47",
"48",
"49",
"50",
"51",
"52",
"53",
"54",
"55",
"56",
"57",
"58",
"59",
"60",
"61",
"62"))
return(data)
}


# If there are values not categorized they will be represented as NA

vallabels_continuous = function(data) {
data$T8089600[1.0 <= data$T8089600 & data$T8089600 <= 24.0] <- 1.0
data$T8089600[25.0 <= data$T8089600 & data$T8089600 <= 49.0] <- 25.0
data$T8089600[50.0 <= data$T8089600 & data$T8089600 <= 74.0] <- 50.0
data$T8089600[75.0 <= data$T8089600 & data$T8089600 <= 99.0] <- 75.0
data$T8089600[100.0 <= data$T8089600 & data$T8089600 <= 124.0] <- 100.0
data$T8089600[125.0 <= data$T8089600 & data$T8089600 <= 149.0] <- 125.0
data$T8089600[150.0 <= data$T8089600 & data$T8089600 <= 174.0] <- 150.0
data$T8089600[175.0 <= data$T8089600 & data$T8089600 <= 199.0] <- 175.0
data$T8089600[200.0 <= data$T8089600 & data$T8089600 <= 224.0] <- 200.0
data$T8089600[225.0 <= data$T8089600 & data$T8089600 <= 249.0] <- 225.0
data$T8089600[250.0 <= data$T8089600 & data$T8089600 <= 9.9999999E7] <- 250.0
data$T8089600 <- factor(data$T8089600,
levels=c(0.0,1.0,25.0,50.0,75.0,100.0,125.0,150.0,175.0,200.0,225.0,250.0),
labels=c("0",
"1 TO 24",
"25 TO 49",
"50 TO 74",
"75 TO 99",
"100 TO 124",
"125 TO 149",
"150 TO 174",
"175 TO 199",
"200 TO 224",
"225 TO 249",
"250 TO 99999999: 250+"))
data$T8089800[13.0 <= data$T8089800 & data$T8089800 <= 99.0] <- 13.0
data$T8089800 <- factor(data$T8089800,
levels=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0),
labels=c("0",
"1",
"2",
"3",
"4",
"5",
"6",
"7",
"8",
"9",
"10",
"11",
"12",
"13 TO 99"))
data$T8218700[1.0 <= data$T8218700 & data$T8218700 <= 999.0] <- 1.0
data$T8218700[1000.0 <= data$T8218700 & data$T8218700 <= 1999.0] <- 1000.0
data$T8218700[2000.0 <= data$T8218700 & data$T8218700 <= 2999.0] <- 2000.0
data$T8218700[3000.0 <= data$T8218700 & data$T8218700 <= 3999.0] <- 3000.0
data$T8218700[4000.0 <= data$T8218700 & data$T8218700 <= 4999.0] <- 4000.0
data$T8218700[5000.0 <= data$T8218700 & data$T8218700 <= 5999.0] <- 5000.0
data$T8218700[6000.0 <= data$T8218700 & data$T8218700 <= 6999.0] <- 6000.0
data$T8218700[7000.0 <= data$T8218700 & data$T8218700 <= 7999.0] <- 7000.0
data$T8218700[8000.0 <= data$T8218700 & data$T8218700 <= 8999.0] <- 8000.0
data$T8218700[9000.0 <= data$T8218700 & data$T8218700 <= 9999.0] <- 9000.0
data$T8218700[10000.0 <= data$T8218700 & data$T8218700 <= 14999.0] <- 10000.0
data$T8218700[15000.0 <= data$T8218700 & data$T8218700 <= 19999.0] <- 15000.0
data$T8218700[20000.0 <= data$T8218700 & data$T8218700 <= 24999.0] <- 20000.0
data$T8218700[25000.0 <= data$T8218700 & data$T8218700 <= 49999.0] <- 25000.0
data$T8218700[50000.0 <= data$T8218700 & data$T8218700 <= 9.9999999E7] <- 50000.0
data$T8218700 <- factor(data$T8218700,
levels=c(0.0,1.0,1000.0,2000.0,3000.0,4000.0,5000.0,6000.0,7000.0,8000.0,9000.0,10000.0,15000.0,20000.0,25000.0,50000.0),
labels=c("0",
"1 TO 999",
"1000 TO 1999",
"2000 TO 2999",
"3000 TO 3999",
"4000 TO 4999",
"5000 TO 5999",
"6000 TO 6999",
"7000 TO 7999",
"8000 TO 8999",
"9000 TO 9999",
"10000 TO 14999",
"15000 TO 19999",
"20000 TO 24999",
"25000 TO 49999",
"50000 TO 99999999: 50000+"))
return(data)
}

varlabels <- c("ID# (1-12686) 79",
"SEX OF R 79",
"HOW MUCH DOES R WEIGH 2018",
"R HEIGHT IN INCHES 2018",
"TOTAL NET FAMILY INCOME 2018",
"AGE AT INTERVIEW 2018"
)


# Use qnames rather than rnums

qnames = function(data) {
names(data) <- c("CASEID_1979",
"SAMPLE_SEX_1979",
"Q11-9_2018",
"Q11-10_B_2018",
"TNFI_TRUNC_2018",
"AGEATINT_2018")
return(data)
}


#********************************************************************************************************

# Remove the '#' before the following line to create a data file called "categories" with value labels.
categories <- vallabels(new_data)

# Remove the '#' before the following lines to rename variables using Qnames instead of Reference Numbers
new_data <- qnames(new_data)
categories <- qnames(categories)

# Remove the '#' before the following lines to produce summaries for the "categories" data file.
#categories <- vallabels(new_data)
#categories <- vallabels_continuous(new_data)
#summary(categories)

#************************************************************************************************************

weightData <- categories$`Q11-9_2018` |> na.omit() |> (\(pounds) pounds / 2.205)()

logLikLogNormal <- function(data) { function(param){
  n <- length(data)
  mu <- param[1]
  sigma <- param[2]
  -sum(log(data)) - 0.5 * n * log(2*pi) - n * log(sigma) - 0.5 * sum((log(data) - mu)^2/sigma^2) 
}}


resultWeight <- maxLik::maxLik(logLikLogNormal(weightData), start = c(2,2))
muWeight <- resultWeight$estimate[1]
sigmaWeight <- resultWeight$estimate[2]

qqplot(qlnorm(ppoints(length(weightData)), muWeight, sigmaWeight), weightData)
qqline(weightData, distribution = \(p) qlnorm(p, muWeight, sigmaWeight), col='red', lwd=2)

seqWeight <- seq(10, max(weightData), 0.1)
hist(weightData, breaks = 25, freq = FALSE)
lines(seqWeight, dlnorm(seqWeight, muWeight, sigmaWeight), col = "red", lwd = 2)

incomeDataUnfiltered <- categories$TNFI_TRUNC_2018 |> na.omit() 
incomeQuants <- quantile(incomeDataUnfiltered, seq(0, 1, length = 100)[1:95])
incomeData <- incomeDataUnfiltered |> Filter(\(income) income < max(incomeQuants) & income > 0, x = _)

resultIncome <- maxLik::maxLik(logLikLogNormal(incomeData), start = c(2,2))
muIncome <- resultIncome$estimate[1]
sigmaIncome <- resultIncome$estimate[2]

seqIncome <- seq(10, max(incomeData), 0.1)
hist(incomeData, breaks = 25, freq = FALSE)
lines(seqIncome, dlnorm(seqIncome, muIncome, sigmaIncome), col = "red", lwd = 2)

fitQuants <- incomeQuants |> names() |> gsub('%', '', x = _) |> as.numeric() |> (\(p) p / 100)()
fitIncome <- qlnorm(fitQuants, muIncome, sigmaIncome)
qqplot(fitIncome, incomeData)
qqline(incomeData, distribution = \(p) qlnorm(p, muIncome, sigmaIncome), col='red', lwd=2)
