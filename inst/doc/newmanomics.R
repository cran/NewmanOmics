## ----setup, include=FALSE, results="hide"--------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.width=7,fig.height=5,echo=TRUE)

## ----libs----------------------------------------------------------------
library(NewmanOmics)

## ----GSE6631-------------------------------------------------------------
data(GSE6631)
dim(GSE6631)
GSE6631[1:5, 1:4]

## ----HN, fig.cap="Box plot of log-transformed data."---------------------
HN <- log2(1 + GSE6631)
boxplot(HN, col=c("forestgreen", "dodgerblue"))

## ----HN1-----------------------------------------------------------------
HN1 <- HN[, 1:2]
result1 <- pairedStat(HN1, pairing = c(-1,1))
summary(result1@nu.statistics)
summary(result1@p.values)

## ----fig.cap="Histogram of empoirical p-values."-------------------------
hist(result1)

## ----fig.cap="Bland-Altman plot."----------------------------------------
plot(result1)

## ------------------------------------------------------------------------
result2 <- pairedStat(HN[, 1:6], pairing=c(-1, 1, -2, 2, -3, 3))
summary(result2@nu.statistics)
summary(result2@p.values)

## ----fig.cap="Bland-ALtman plots."---------------------------------------
plot(result2)

## ----fig.cap="P-value histograms."---------------------------------------
hist(result2)

## ----HN3-----------------------------------------------------------------
normals <- HN[, c(1,3,5)]
tumors <- HN[, c(2,4,6)]
result3 <- pairedStat(normals, tumors)
summary(result3@nu.statistics)
summary(result3@p.values)

## ----HN4-----------------------------------------------------------------
listOfPairs <- list(HN[,1:2], HN[,3:4], HN[,5:6])
result4 <- pairedStat(listOfPairs)
summary(result4@nu.statistics)
summary(result4@p.values)

## ----makeBank------------------------------------------------------------
normals <- HN[, seq(1, ncol(HN), 2)] # odds are normal
tumors <- HN[, seq(2, ncol(HN), 2)] # evens are tumor
bank <- createBank(normals)
result5 <- bankStat(bank, tumors[,1,drop=FALSE])
summary(result5$nu.statistics)
summary(result5$p.values)
hist(result5$p.values, breaks=101)

