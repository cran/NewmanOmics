library(NewmanOmics)
## simulate a data set
set.seed(86420)
NR <- 5000
NC <- 20
N2 <- matrix(rnorm(NR*NC), nrow = NR, ncol = NC)
C2 <- matrix(rnorm(NR*NC), nrow = NR, ncol = NC)
## Compute stats theoretically
T1 <- pairedStat(N2, C2, ptype = "theo", ntype = "one")
T2 <- pairedStat(N2 ,C2, ptype = "theo", ntype = "two")
# compute stats empirically
E1 <- pairedStat(N2, C2, ptype = "emp", ntype = "one")
E2 <- pairedStat(N2, C2, ptype = "emp", ntype = "two")
## test agreement of nu.statistics
all(T1@nu.statistics > 0) # absolute value already computed
all(T2@nu.statistics == E2@nu.statistics) # signed values agree across methods
all(T1@nu.statistics == abs(T2@nu.statistics)) # theoretical agrees
all(E1@nu.statistics == abs(E2@nu.statistics)) # empirical agrees
## test agreement of p-values
fold <- function(p) { 1 - abs(1 - 2*p) } # convert two-sided to one-sided
all(T1@p.values == fold(T2@p.values))
range(E1@p.values - fold(E2@p.values)) # variability from simulations
range(T1@p.values - E1@p.values)       # variability from simulations
range(T2@p.values - E2@p.values)       # variability from simulations
