\name{data.to.list.gene.snp}
\alias{data.to.list.gene.snp}
\title{Annotation gene/SNP}
\usage{
  data.to.list.gene.snp(file, header = TRUE, path = NULL)
}
\arguments{
  \item{file}{txt file containing the SNP/gene annotation
  (see Details for the format of the txt file).}

  \item{header}{a logical value indicating whether the file
  contains the names of the variables as its first line. By
  default \code{header} is set to TRUE.}

  \item{path}{linking to the directory containing the data
  (SNP/gene)}
}
\value{
  A list containing for each gene the names of the SNPs
  belonging to it. This list is required for using the
  function \link{data.to.PIGE} and the function
  \link{ARTP.GE}.
}
\description{
  The \code{data.to.list.gene.snp} function generates the
  list containing for each gene the corresponding SNP ids
  from a txt file containing the SNP/gene annotation data.
  This list is required for the functions
  \code{data.to.PIGE} and \code{ARTP.GE}.
}
\details{
  The txt file containing the annotation data for the
  SNP/gene is a two columns matrix. The first colums is the
  name of each SNP. The second columns indicates the genes
  which the corresponding SNP (same row) belongs. It will
  be noted "Gene1/Gene4/Gene5", if for example a SNP
  belongs to the genes: Gene1, Gene4, Gene5.
}
\examples{
##Example : case-control study data
data(data.pige)
data(data.pathway)
path.in <- paste(system.file("sampleData", package="PIGE"),"/",sep="")
file <- "SNP-GENE-annotation.txt"
list.gene.snp <- data.to.list.gene.snp(file,path=path.in)
##Example Survival data
data("data.surv")
data("data.pathway.surv")
path.in <- paste(system.file("sampleData", package="PIGE"),"/",sep="")
file="snp.gene.surv.txt"
list.gene.snp.surv <- data.to.list.gene.snp(file,path=path.in)
}
\author{
  Benoit Liquet
  \email{benoit.liquet@isped.u-bordeaux2.fr}\cr Therese
  Truong \email{therese.truong@inserm.fr}
}

