\name{LR.cont.surv}
\alias{LR.cont.surv}
\title{Wald test for an adjusted model.}
\usage{
  LR.cont.surv(x, formula, data)
}
\arguments{
  \item{x}{name or numeric vector corresponding to the SNP
  tested.}

  \item{formula}{an object of class "formula" : a symbolic
  description of the model to be fitted without the
  interaction term.}

  \item{data}{a data frame containing the variables in the
  model.}
}
\value{
  p-value of the Wald test for a SNP effect.
}
\description{
  The \code{LR.cont.surv} function performs a Wald test for
  a SNP effect based on a cox model. The function returns
  the p-value of the Wald test. It is an Internal function
  used by the \link{permutation.snp} function.
}
\author{
  Benoit Liquet
  \email{benoit.liquet@isped.u-bordeaux2.fr}\cr Therese
  Truong \email{therese.truong@inserm.fr}
}

