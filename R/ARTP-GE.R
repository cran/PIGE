#' Calculate gene- and pathway-environment interaction p-values using the Adaptive Rank Truncated Product method. This function uses mainly the function \code{ARTP_pathway} developped 
#' by Kai Yu (R package ARTP).
#' @export
#' @title Gene and pathway p-values using ARTP method
#' @name ARTP.GE
#' @param data.gene.pathway Data frame (Gene X Pathways) of 0 and 1 values.
#' The rownames (gene name considered) and the colnames (names of the studied pathways)
#'  have to be specified. The value 1 indicates that a gene is included in the corresponding pathway.   
#' @param list.gene.snp List containing for each gene the corresponding SNP ids. This list could be generated by \code{data.to.list.gene.snp} function.
#' @param p.snp.permut the output matrix from either \code{\link{permutation.snp}}, \code{\link{bootstrap.snp}} or a file
#' with the SNP ids and p-values (see details).
#' @param p.snp.obs The output data frame from \code{\link{compute.p.snp.obs}} or a file
#' with the SNP ids and p-values (see details).
#' @param inspect.snp.n The number of candidate truncation points to inspect the top SNPs
#'  in a gene. The default is 1.
#' @param inspect.snp.percent A value x between 0 and 1 such that a truncation point will be defined at every x percent of the top SNPs. 
#' The default is 0 so that the truncation points will be \code{1:inspect.snp.n}
#' @param inspect.gene.n The number of candidate truncation points to inspect the top genes in the pathway. The default is 10.
#' @param  inspect.gene.percent A value x between 0 and 1 such that a truncation point will be defined at every x percent of the top genes. The default is 0.05.
#' @param temp.dir A folder to keep temporary files that will be created.
#' @param nperm  Number of permutation used or number of parametric bootsrap used.
#' @return The returned value is a list with names "res.gene.list" and "res.pathway". 
#' \code{res.gene.list} is a list with length equals to the number of investigated
#' pathways. Each element of the list is a data frame containing the gene name, number of  SNPs belonging to  the gene that were included in the analysis, and the ARTP p-value for the gene. 
#' \code{res.pathway} contains the ARTP p-values for all the pathway analysed. The results contained in \code{res.pathway} are saved in a file named "ARTP-GEI.RData". 
#' The data frame containing all the gene analysed, the number of SNPs belonging to each gene and the ARTP p-value are saved in a file named "ARTP-GENE.RData". 
#' @author Benoit Liquet \email{benoit.liquet@@isped.u-bordeaux2.fr}\cr
#'  Therese Truong \email{therese.truong@@inserm.fr}
#' @references  Yu K, Li Q, Berger AW, Pfeiffer R, Rosenberg P, Caporaso N, Kraft P, Chatterjee N (2009). Pathway analysis by adaptive combination of P-values. Genet Epidemiol 33:700-709.
#' @details If the p-values are not computed using \code{\link{permutation.snp}}, \code{\link{bootstrap.snp}} and \code{\link{compute.p.snp.obs}} then the format for p.snp.obs and p.snp.permut should be as follows.
#'  Both files must be uncompressed, comma seperated files with the first row as the SNP ids in the same order. 
#'  Row 2 of obs.file has the observed p-values, and starting from row 2 in perm.file are the permuted p-values or the boostrap p-values.

#' A random seed should be set before calling \code{ARTP.GE.R} in order to reproduce results. 
#' The randomness is due to the ranking of p-values, where ties are broken randomly. 
#' @examples
#' data(data.pathway)
#' data(list.gene.snp)
#' \dontrun{
#' data(data.pige)
#' 
#' ###First example: compute observed p-value (orignal data) and permuted p-value
#' res <-data.to.PIGE(data=data.pige,data.pathway=data.pathway,
#' list.gene.snp=list.gene.snp,choice.pathway=c(1,2))
#' formul <- formula(y~factor(cov1)+factor(cov2)+factor(cov3)+factor(cov4)
#' +var_int)
#' p.snp.obs.ex <-  compute.p.snp.obs(data=data.pige,model=formul,
#' indice.snp=res$snp.selected,var.inter="var_int",class.inter=NULL) 
#' p.snp.permut.ex <-  permutation.snp(model=formul,data=data.pige,
#' indice.snp=res$snp.selected,var.inter="var_int",class.inter=NULL,
#' nbcpu=3,Npermut=9,file.out="res-permut") 
#' set.seed(10)
#' result.1 <- ARTP.GE(data.gene.pathway=data.pathway,
#' list.gene.snp=list.gene.snp,p.snp.permut=p.snp.permut.ex,
#' p.snp.obs=p.snp.obs.ex,inspect.snp.n=5,inspect.snp.percent=0.05
#' ,inspect.gene.n=10,inspect.gene.percent=0.05,temp.dir="TEMP/"
#' ,nperm=9)
#' result.1
#'    
#' ##Second example: observed and permuted p-values have already been computed
#' path.data <- paste(system.file("sampleData", package="PIGE"),"/",sep="")
#' res.permut <- read.table(file=paste(path.data,"res-permut.txt",sep="")
#' ,header=TRUE,sep=" ") 
#' res.obs   <- read.table(file=paste(path.data,"res-obs.txt",sep="")
#' ,header=TRUE,sep=" ") 
#' result.2 <- ARTP.GE(data.gene.pathway=data.pathway,
#' list.gene.snp=list.gene.snp, p.snp.permut=res.permut,
#' p.snp.obs=res.obs,inspect.snp.n=5,inspect.snp.percent=0.05,
#' inspect.gene.n=10,inspect.gene.percent=0.05,temp.dir="TEMP/",nperm=90)
#' result.2
#' 
#' ##Third example: Survival data 
#' ##observed and permuted p-values have already been computed
#' 
#' data(data.surv)
#' data(data.pathway.surv)
#' data(list.gene.snp.surv)
#' path.data <- paste(system.file("sampleData", package="PIGE"),"/",sep="")
#' res.permut <- read.table(file=paste(path.data,"res-permut-surv.txt",sep="")
#' ,header=TRUE,sep=" ") 
#' res.obs   <- read.table(file=paste(path.data,"res-obs-surv.txt",sep="")
#' ,header=TRUE,sep=" ") 
#' result.3 <- ARTP.GE(data.gene.pathway=data.pathway.surv,
#' list.gene.snp=list.gene.snp.surv, p.snp.permut=res.permut,
#' p.snp.obs=res.obs,inspect.snp.n=5,inspect.snp.percent=0.05,
#' inspect.gene.n=10,inspect.gene.percent=0.05,temp.dir="TEMP/",nperm=90)
#' result.3
#'} 

ARTP.GE <- function(data.gene.pathway,list.gene.snp,p.snp.permut,p.snp.obs,inspect.snp.n=1,inspect.snp.percent=0,inspect.gene.n=10,inspect.gene.percent=0.05,temp.dir="TEMP/",nperm){
npathway <- dim(data.gene.pathway)[2]
#############################
res.pathway <- NULL
res.gene <- NULL
res.gene.list <- NULL
#############################
if(is.character(p.snp.permut)) p.snp.permut <- read.table(file=p.snp.permut,header=TRUE,sep=" ") 
if(is.character(p.snp.obs)) p.snp.obs <- read.table(file=p.snp.obs,header=TRUE,sep=" ") 

for(k in 1:npathway){
#print(k)
if(dim(data.gene.pathway)[2]==1){gene <- rownames(data.gene.pathway)[data.gene.pathway[,1]==1]}else{   
gene <-  rownames(data.gene.pathway[data.gene.pathway[,k]==1,])}
SNP <- list.gene.snp[gene]
GENE <- rep(names(SNP),times=sapply(SNP,FUN=length))
tab <- cbind(SNP=unlist(SNP),GENE=GENE)
if(dim(tab)[1]>1) {
### write file SNP-->GENE for pathways k
file.name <- paste("data.snp.gene",colnames(data.gene.pathway)[k],".txt",sep="")
write.table(tab,row.names=FALSE,col.names=TRUE,sep="\t",file=file.name,quote=FALSE)
gene.list1 <- list(file=file.name,delimiter="\t",gene.var="GENE",snp.var="SNP",header=1)

### write p.value permutation for snp in pathway k
snp.pvalue <- p.snp.permut[,unique(tab[,1])]
name.perm <- paste("perm",colnames(data.gene.pathway)[k],".txt",sep="")
write.table(snp.pvalue,row.names=FALSE,file=name.perm,quote=FALSE,sep=",")
###

### write p.value observ for snp in pathway k
snp.pvalue.obs <- p.snp.obs[unique(tab[,1])]
name.obs <- paste("obs",colnames(data.gene.pathway)[k],".txt",sep="")
write.table((as.matrix((snp.pvalue.obs))),row.names=FALSE,file=name.obs,quote=FALSE,sep=",")
###

###compute ARTP
opt.pathway <- list(inspect.snp.n=inspect.snp.n,inspect.snp.percent=inspect.snp.percent,inspect.gene.n=inspect.gene.n,inspect.gene.percent=inspect.gene.percent)
res <- ARTP_pathway(obs.file=name.obs, perm.file=name.perm, nperm=nperm, temp.dir=temp.dir, gene.list=gene.list1,op=opt.pathway)
###
}else{###case where the pathway have only one gene
  snp.pvalue.obs <- p.snp.obs[unique(tab[,1])]
  name.obs <- paste("obs",colnames(data.gene.pathway)[k],".txt",sep="")
  write.table((as.matrix((snp.pvalue.obs))),row.names=FALSE,file=name.obs,quote=FALSE,sep=",")  
  res <- list(pathway.pvalue=as.numeric(snp.pvalue.obs),gene.table=data.frame(Gene=tab[,2],N.SNP=1,Pvalue=as.numeric(snp.pvalue.obs)))  
  
}

print(paste(colnames(data.gene.pathway)[k],"=",signif(res$pathway.pvalue,digits=3),sep=""))
res.pathway <- c(res.pathway,res$pathway.pvalue)
res.gene <- rbind(res.gene,res$gene.table)
res.gene.list <- c(res.gene.list,list(path=res$gene.table))
}

names(res.gene.list) <- colnames(data.gene.pathway)
                                 
##need to check the output                                 
res.gene[match(unique(res.gene[,1]),res.gene[,1]),]-> res.gene.result
save(res.gene.result,file="ARTP-GENE.RData")
#res.gene.result[which(res.gene.result[,3]<=0.05),]->tab
#titre <- "Gene pvalue less than 0.05 with"
#res3 <- xtable(tab,digits=c(0,0,0,3),caption=titre)
#print(res3,file="gene-pvalue.tex",include.rownames=FALSE,caption.placement="top",type="latex")
names(res.pathway) <- colnames(data.gene.pathway)
save(res.pathway,file="ARTP-GEI.RData")
result <- list(res.gene.list=res.gene.list,res.pathway=res.pathway)
}

