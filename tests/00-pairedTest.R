library(NewmanOmics)

data(LungPair)
lung <- as.matrix(log2(1 + LungPair))
summary(lung)

set.seed(12345)
normal <- lung[, 1, drop=FALSE]
tumor  <- lung[, 2, drop=FALSE]

tic <- proc.time()
ps <- pairedStat(normal, tumor)
toc <- proc.time()
all(toc - tic < 10, na.rm=TRUE) # takes less than 7s on my PC
slotNames(ps) # should be five
dim(ps@nu.statistics)
dim(ps@p.values)

summary(ps@nu.statistics)
summary(ps@p.values)

head(ps@nu.statistics)
head(ps@p.values)

ps2 <- pairedStat(list(lung))
summary(ps2@nu.statistics)
summary(ps2@p.values)

summary(ps@nu.statistics - ps2@nu.statistics)
summary(pdiff <- ps@p.values - ps2@p.values)

plot(ps@nu.statistics, pdiff)
abline(h=0)

plot(ps)
hist(ps)