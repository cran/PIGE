\name{bootstrap.snp}
\alias{bootstrap.snp}
\title{Parallel computing of the Likelihhod ratio test (or Wald test) for an interaction term (or a simple SNP effect) on bootstrap sample}
\usage{
  bootstrap.snp(model, Outcome.model = "binary", data,
    var.inter = NULL, indice.snp, class.inter = NULL,
    nbcpu = NULL, Nboot = 1000,file.out = "res-boot")
}
\arguments{
  \item{model}{an object of class "formula" : a symbolic
  description of the model to be fitted without the
  interaction term.}

  \item{Outcome.model}{a character string naming the type
  of outcome considered. The current version allows only "\code{binary}" (by default).}

  \item{data}{a data frame containing the variables in the
  model.}

  \item{var.inter}{name of the variable which is tested in
  interaction with the SNPs (SNP:E). By default
  var.inter=NULL correspond to a test on the SNPs (no
  interaction)}

  \item{indice.snp}{vector or character indicating the SNPs
  to be tested.}

  \item{class.inter}{class of the \code{var.inter}
  variable. By default, the variable is considered as
  continuous and a Wald test is performed. Use ("factor")
  to indicate categorical variable.}

  \item{nbcpu}{integer indicating the number of CPU of your
  computer (-1). By default, the function use only one
  cpu.}

  \item{Nboot}{number of bootstrap (1000 by default).}

  \item{file.out}{name of the output file where the result
  will be saved.}
}
\value{
  A matrix containing the p-value, for each bootstrap
  (row) and for each SNP (column), of the likelihood ratio
  test (or the Wald test) for the interaction term or the
  SNP effect. This matrix is also saved in a txt file
  (named by the argument \code{file.out}) located in the
  current directory.
}
\description{
  The \code{bootstrap.snp} function performs on
  bootstrap sample a Likelihood Ratio Test (or a Wald
  test) for an interaction term SNP*E (where E is an
  Environment variable) or for the effect of the SNP. This
  function uses the parallel computing on different CPU of
  the computer. This function returns a matrix containing
  for each bootstrap and SNP the p-value of the
  interaction term tested (SNP*E) or the p-value of the SNP
  effect tested. Note that this parametric bootsrap approach has been 
  only currently implemented in this package for Logistic model.
}
\examples{
data(data.pige)
data(data.pathway)
data(list.gene.snp)
res <-data.to.PIGE(data=data.pige,data.pathway=data.pathway,
list.gene.snp=list.gene.snp,choice.pathway=c(1,2))
formul <- formula(y~factor(cov1)+factor(cov2)+factor(cov3)+factor(cov4)
+var_int)
debut <- Sys.time()
p.snp.boot.ex <-  bootstrap.snp(model=formul,data=data.pige,
indice.snp=res$snp.selected,var.inter="var_int",class.inter=NULL,
nbcpu=2,Nboot=2,file.out="res-boot")
print(Sys.time()-debut)
}
\author{
  Benoit Liquet
  \email{benoit.liquet@isped.u-bordeaux2.fr}\cr Therese
  Truong \email{therese.truong@inserm.fr}
}

