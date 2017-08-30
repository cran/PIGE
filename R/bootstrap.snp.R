#' The \code{bootstrap.snp} function performs on parametric bootstrap sample a Likelihood Ratio Test (or a Wald test) for an interaction 
#' term SNP*E (where E is an Environment variable) or for the effect of the SNP. This function uses the parallel computing on different CPU of the computer. 
#' This function returns a matrix containing for each bootstrap sample and SNP the p-value of the interaction
#' term tested (SNP*E) or the p-value of the SNP effect tested.
#' @export
#' @title Parallel computing of the Likelihhod ratio test (or Wald test) for an interaction term (or a simple SNP effect) on bootstrap sample
#' @name bootstrap.snp
#' @param model an object of class "formula" : a symbolic description of the model to be fitted
#' without the interaction term.
#' @param Outcome.model a character string naming the type of outcome considered. The current version allows only "\code{binary}" (by default). 
#' @param data a data frame containing the variables in the model. 
#' @param var.inter name of the variable which is tested in interaction with the SNPs (SNP:E). By default var.inter=NULL correspond to a test on the SNPs (no interaction)
#' @param indice.snp vector or character indicating the SNPs to be tested. 
#' @param class.inter class of the \code{var.inter} variable. By default, the variable is considered as continuous and a Wald test is performed. Use ("factor") to indicate categorical variable.
#' @param nbcpu integer indicating the number of CPU of your computer (-1). By default, the function use only
#' one cpu.
#' @param Nboot number of bootstrap sample (1000 by default).
#' @param file.out name of the output file where the result will be saved.
#' @return A matrix containing the p-value, for each bootstrap (row) and for each SNP (column), of the likelihood ratio test (or the Wald test) for the interaction term or the SNP effect.
#' This matrix is also saved in a txt file (named by the argument \code{file.out}) located in the current directory.
#' 
#' @author Benoit Liquet \email{benoit.liquet@@isped.u-bordeaux2.fr}\cr
#'  Therese Truong \email{therese.truong@@inserm.fr}
#' 
#' @examples data(data.pige)
#' data(data.pathway)
#' data(list.gene.snp)
#' res <-data.to.PIGE(data=data.pige,data.pathway=data.pathway,
#' list.gene.snp=list.gene.snp,choice.pathway=c(1,2))
#' formul <- formula(y~factor(cov1)+factor(cov2)+factor(cov3)+factor(cov4)
#' +var_int)
#' debut <- Sys.time() 
#' p.snp.permut.ex <-  bootsrap.snp(model=formul,data=data.pige,
#' indice.snp=res$snp.selected,var.inter="var_int",class.inter=NULL,nbcpu=3,
#' Nboot=9,file.out="res-boot") 
#' print(Sys.time()-debut)


bootstrap.snp <- function(model,Outcome.model="binary",data,var.inter=NULL,indice.snp,class.inter=NULL,nbcpu=NULL,Nboot=1000,file.out="res-boot"){
  mat.snp <- data[,indice.snp]
  if(is.null(nbcpu)){sfInit(parallel=FALSE, cpus=1)}else{
    sfInit(parallel=TRUE, cpus=nbcpu)
    sfLibrary(package="PIGE",pos=4,character.only=TRUE)
  }
  list.snp <- as.list(mat.snp)
  sfExportAll()
#  print(model)
  
  
  if(is.null(var.inter)) {
     #if(method=="YX"){
      result <- sfLapply(list.snp,bootstrap.wrapper.cont.Y.and.X,data=data,model=model,Outcome.model=Outcome.model,Nboot=Nboot)
     # }else{
     #  result <- sfLapply(1:Npermut,permutation.wrapper.cont,mat=mat.snp,data=data,model=model,Outcome.model=Outcome.model)
     #}
  }else{
  if(is.null(class.inter)){
    var.inter <- data[,var.inter]
    #if(method=="YX"){
    result <- sfLapply(list.snp,bootstrap.wrapper.cont.inter.Y.and.X,data=data,model=model,var.inter=var.inter,Outcome.model=Outcome.model,Nboot=Nboot)
    #}else{
    #result <- sfLapply(1:Npermut,permutation.wrapper.cont.inter,mat=mat.snp,data=data,model=model,var.inter=var.inter,Outcome.model=Outcome.model)
    #}
    }else{
    var.inter <- factor(data[,var.inter])
    #if(method=="YX"){
    result <- sfLapply(list.snp,bootstrap.wrapper.cat.inter.Y.and.X,data=data,model=model,var.inter=var.inter,Outcome.model=Outcome.model,Nboot=Nboot)
    #}else{
    #result <- sfLapply(1:Npermut,permutation.wrapper.cat.inter,mat=mat.snp,data=data,model=model,var.inter=var.inter,Outcome.model=Outcome.model)  
    #}
    }}  
    sfStop()
  mat.result <- matrix(unlist(result),ncol=length(indice.snp),nrow=Nboot,byrow=FALSE)
  colnames(mat.result) <- colnames(mat.snp)
  write.table(mat.result,file=paste(file.out,".txt",sep=""),row.names=FALSE,col.names=TRUE)
  return(mat.result)
}


