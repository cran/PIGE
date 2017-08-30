#' Internal function used for the parallel computation on the bootstrap sample
#' @export
#' @keywords internal
bootstrap.wrapper.cont.inter.Y.and.X <- function(x,data,model,var.inter,Outcome.model,Nboot){
  data <- data.frame(data,x=x)
  model.boot <- glm(formula=update(model,~.+x),data=data,family=binomial(link="logit"))
  y.boot <- matrix(rbinom(n=Nboot*nrow(model.boot$data),1,predict(model.boot,type="response")),ncol=Nboot,nrow=nrow(model.boot$data),byrow=FALSE)
  
  if(Outcome.model=="binary") {
  p.test <- apply(y.boot,MARGIN=2,FUN=LR.inter.cont.boot,formula=model,data=data,Z1=var.inter)
  }else{
  stop("only binary outcome has been implemented")
  }    
  return(p.test)
}