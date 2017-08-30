#' Internal function used for the parallel computation on the permutation sample
#' @export
#' @keywords internal
permutation.wrapper.cat.inter.Y.within.X <- function(x,mat,data,model,var.inter,Outcome.model){
  indix<- 1:(dim(data)[1])
  for (i in levels(var.inter))
  {
    indix[which(var.inter==i)] <- sample(which(var.inter==i))
  }
  nameY.X <- all.vars(model)
  data.perm <- data[indix,nameY.X]
  var.inter <- var.inter[indix]
  # print(data.perm)
  # print(model)
  
  if(Outcome.model=="binary"){
    p.test <- apply(mat,MARGIN=2,FUN=LR.inter.cat,formula=model,data=data.perm,Z1=var.inter)#,class.Z=class.inter)
  }else{
    p.test <- apply(mat,MARGIN=2,FUN=LR.inter.cat.surv,formula=model,data=data.perm,Z1=var.inter)#,class.Z=class.inter) 
  }
  return(p.test)
}
