#' Internal function used for the parallel computation on the permutation sample
#' @export
#' @keywords internal
permutation.wrapper.cont.inter.Y.within.X <- function(x,mat,data,model,var.inter,Outcome.model){
  Y.castemoin <- all.vars(model,max.names=1)
  Y <- data[,Y.castemoin]
  var.inter.temp <- as.factor(unlist(var.inter))
  Y.perm <- 1:length(var.inter.temp)
  for (i in levels(var.inter.temp))
  {
    Y.perm[which(var.inter.temp==i)] <- sample(Y[which(var.inter.temp==i)] )
  }
  data.perm <- data.frame(data,Y.perm=Y)
  model.perm <- update(model,Y.perm~.)
  if(Outcome.model=="binary"){
  p.test <- apply(mat,MARGIN=2,FUN=LR.inter.cont,formula=model.perm,data=data.perm,Z=var.inter)#,class.Z=class.inter)
  }else{
  p.test <- apply(mat,MARGIN=2,FUN=LR.inter.cont.surv,formula=model.perm,data=data.perm,Z=var.inter)#,class.Z=class.inter)  
  }
  return(p.test)
}


