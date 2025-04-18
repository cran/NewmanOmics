\name{pairedStat}
\alias{pairedStat}
\title{Paired Newman Statistic}
\description{
The Paired Newman Statistic is used for one-to-one comparison of
paired individual samples. Commonly used to find differential expression
between tumor-normal pairs or before-after treatment pairs.
}
\usage{
pairedStat(baseData, perturbedData = NULL, pairing = NULL)
}
\arguments{
\item{baseData}{Either a list or a matrix. May contain data for just the
  base condition (for example, normal samples or samples before
  treatment) or for both the base condition and the perturbed condition
  (for example, tumor samples or samples after treatment). See details.}
\item{perturbedData}{An optional matrix containing data for the "perturbed"
  samples. May be NULL if the \code{baseData} argument is a list or a
  matrix containing all the data.}
\item{pairing}{An optional vector indicating the pairing between base
  and perturbed samples. Entries must be integers. Positive integers
  indicate perturbed samples and negative integers with the same
  absolute value indicate the paired base samples. See details.}
}
\value{
A list containing two marices: the \code{nu.statistics} and the \code{p.values}.
}
\details{
  In the simplest case, we have gene expression data on one "base"
  sample and one "perturbed" sample, and the goal is to identify genes
  whose expression changes between the two states. Our primary
  assumption is that the standard deviation (SD) of gene expression
  varies as a smooth function of the mean; fitting such a curve allows
  us to detect individual genes whose difference is large compared to
  the smoothed SD.

  Note that this assumption is most useful on the log-transformed
  scale  (\url{https://pubmed.ncbi.nlm.nih.gov/25092958/}).
  If your data is on a raw scale, then we recommend transforming
  it before computing the Newman paired statistic.

  The input arguments to the \code{pairedStats} function are moderately
  complicated in order to allow users to choose a convenient method for
  supplying data when they have multiple paired samples. The first
  posssibility is to have all the base samples in one matrix and all the
  perturbed samples in a second matrix. In this case, we assume (without
  checking) that the columns in the two matrices correspond to the
  paired samples, and that the genes-rows are in the same order.

  The second possibility is to put the data for both the base samples
  and the perturbed samples in the same matrix. In this case, the user
  must supply a \code{pairing} vector to explain how the samples should
  be matched. If the column order is ("base1", "perturbed1", "base2",
  "perturbed2", ...), then the pairiing vector should be written as
  \code{c(-1, 1, -2, 2, -3, 3, ...)}.

  The third possibility is to provide the paired samples in a list,
  each of whose entries is a matrix with two columns,with the first
  column being the base state and the second column being the
  perturbed state.

  This flexibility means that there are three equivalent ways to input
  the data even if you have only one base sample (with data in the
  one-column matrix B) and one perturbed sample (with data in the
  one-column matrix P). If we let \code{BP <- cbind(B, P) }, then we can
  choose (1) \code{pairedStats(B, P)}, or (2)
  \code{pairedStats(list(BP))}, or (3)  \code{pairedStats(BP,
  pairing = c(-1,1))}. 
}
\examples{
data(GSE6631)
Normal <- GSE6631[, c(1,3)]
Tumor <- GSE6631[, c(2,4)]

### input two separate matrices
ps1 <- pairedStat(Normal, Tumor)
summary(ps1@nu.statistics)
summary(ps1@p.values)

### input one combined matrix and a pairing vector
ps2 <- pairedStat(GSE6631, pairing=c(-1, 1, -2, 2))
summary(ps2@nu.statistics)
summary(ps2@p.values)

### input a list of matrix-pairs
ps3 <- pairedStat(list(One = GSE6631[, 1:2],
                       Two = GSE6631[, 3:4]))
summary(ps3@nu.statistics)
summary(ps3@p.values)
}
