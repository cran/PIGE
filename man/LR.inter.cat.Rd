\name{LR.inter.cat}
\alias{LR.inter.cat}
\title{Likelihhod ratio test for an interaction term}
\usage{
  LR.inter.cat(x, formula, data, Z1)
}
\arguments{
  \item{x}{name or numeric vector corresponding to the SNP
  tested.}

  \item{formula}{an object of class "formula" : a symbolic
  description of the model to be fitted without the
  interaction term.}

  \item{data}{a data frame containing the variables in the
  model.}

  \item{Z1}{name of the variable which is tested in
  interaction with x (x:Z1).}
}
\value{
  p-value of the likelihood ratio test for the interaction
  term.
}
\description{
  The \code{LR.inter.cat} function performs a likelihood
  ratio test (LRT) for an interaction term between a
  categorical variable and a SNP (coded 0,1, 2) in a
  logistic model. The function returns the p-value of the
  likelihood ratio test.
}
\author{
  Benoit Liquet
  \email{benoit.liquet@isped.u-bordeaux2.fr}\cr Therese
  Truong \email{therese.truong@inserm.fr}
}

