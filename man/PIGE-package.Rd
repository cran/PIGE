\docType{package}
\name{PIGE-package}
\alias{PIGE}
\alias{PIGE-package}
\title{Gene and pathway p-values using the Adaptive Rank Truncated Product test}
\description{
  An R package for computing gene and pathway p-values
  using the Adaptive Rank Truncated test (ARTP). This
  package can be used to analyze pathways/genes based on a
  genetic association study, with a binary case-control
  outcome or a survival outcome. This package is an
  extension of the ARTP method developped by Kai Yu (Genet
  Epidemiol. 2009) for gene- and pathway-environment
  interaction analysis.
}
\details{
  The statistical significance of the pathway-level test
  statistics is evaluated using a highly efficient
  permutation algorithm that remains computationally
  feasible irrespective of the size of the pathway and
  complexity of the underlying test statistics for
  summarizing SNP- and gene-level associations. The
  function \code{\link{ARTP.GE}} is used to compute gene
  and pathway p-values provided that the observed and
  permutation p-values for each SNP already exist in files.
  The input files required for \code{\link{ARTP.GE}} could
  be obtained by calling the function
  \code{\link{permutation.snp}} and the function
  \code{\link{compute.p.snp.obs}}.
}
\author{
  Benoit Liquet
  \email{benoit.liquet@isped.u-bordeaux2.fr}\cr Therese
  Truong \email{therese.truong@inserm.fr}
}
\references{
  Yu K, Li Q, Berger AW, Pfeiffer R, Rosenberg P, Caporaso
  N, Kraft P, Chatterjee N (2009). Pathway analysis by
  adaptive combination of P-values. Genet Epidemiol
  33:700-709.
}
\seealso{
  \code{\link{ARTP.GE}}, \code{\link{permutation.snp}},
  \code{\link{compute.p.snp.obs}}
}
\keyword{package}

