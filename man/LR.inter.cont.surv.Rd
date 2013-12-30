\name{LR.inter.cont.surv}
\alias{LR.inter.cont.surv}
\title{Wald test for an interaction term}
\usage{
  LR.inter.cont.surv(x, formula, data, Z1)
}
\arguments{
  \item{x}{name or numeric vector corresponding to the SNP
  tested}

  \item{formula}{an object of class "formula" : a symbolic
  description of the model to be fitted without the
  interaction term}

  \item{data}{a data frame containing the variables in the
  model}

  \item{Z1}{name of the variable which is tested in
  interaction with x (x:Z1)}
}
\value{
  pvalue of the Wald test for the interaction term
}
\description{
  The \code{LR.inter.cont.surv} function performs a Wald
  test for an interaction term between a continuous
  variable and a SNP based on a cox model.. The function
  return the pvalue of the Wald test.
}
\author{
  Benoit Liquet
  \email{benoit.liquet@isped.u-bordeaux2.fr}\cr Therese
  Truong \email{therese.truong@inserm.fr}
}

