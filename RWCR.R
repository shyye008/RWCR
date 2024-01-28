rm(list=ls())

library(ncvreg); library(mvtnorm); library(splitstackshape)
library(caret); library(lme4)
#size: number of WCR data sets
#y: response variable, can be continuous or binary.
#X: covariate matrix
#id: cluster id
#type: type of penalty function, either "lasso", "SCAD, or "MCP"
#fm: error distribution, either "binomial" or "gaussian"
#lambda: initial set of tuning parameter values
#SIS: logical value indicating whether the SIS step is used
#Output: matrix of selection probability from the "WCR" function
#pi: candidate values of the threshold value of the selection probability
#K: number of folds for cross-validation
WCR<-function(size,y,X,id,type,fm,lambda,SIS=TRUE){
  dat<-data.frame(y,id,X)
  rate<-0
  p<-1
  while (p <= size) {
    datp<-stratified(dat,"id",size=1)
    y_p<-as.matrix(datp[,1]); y_p<-as.vector(y_p); X_p<-as.matrix(datp[,3:dim(datp)[2]])
    if(SIS==TRUE){
      sisc<-c()
      for (i in 1:ncol(X_p)) {
        sisci<-glm(as.factor(y_p)~X_p[,i],family=fm)
        sisc<-c(sisc,coef(sisci)[-1])
      }
      selen<-tail(order(abs(sisc)),round(length(y_p)/log(length(y_p))))
      sele<-rep(0,ncol(X_p));sele[selen]<-1
      X_p<-X_p[,sele==1]
    }else{
      sele<-rep(1,ncol(X_p))
    }
    
    fit_p<-ncvreg(X_p,y_p,penalty=type,family=fm,lambda=lambda)
    beta.hat_p<-as.matrix(coef(fit_p, mode="step")[-1,-1])
    candi_p<-t(ifelse(beta.hat_p!=0,1,0))
    all_p<-matrix(0,nrow=nrow(candi_p),ncol=length(sele))
    all_p[,which(sele==1)]<-candi_p
    rate_new<-try(rate+all_p, silent=T)
    if((class(rate_new)=="try-error")){
      rate<-rate; p<-p
    } else{
      rate<-rate_new; p<-p+1
    }
  }
  rate<-rate/size
  return(rate)
}

select<-function(output,pi){
  dec<-(output>pi)
  if(is.vector(dec)==T){
    out<-as.numeric(dec)
  } else{
    pos<-which.max(rowSums(dec)); out<-as.numeric(dec[pos,])
  }
  return(out)
}

K_CV<-function(y,X,id,K=5,fm,output,pi){
  flds<-createFolds(id,K,list=TRUE,returnTrain=FALSE)
  dat<-data.frame(y,id,X)
  err_c<-c()
  for (i in pi) {
    sele<-select(output,i)
    xnam <- paste("X", which(sele==1), sep="")
    fmla <- as.formula(paste(paste("y ~ ", paste(xnam, collapse= "+")),"+(1|id)"))
    err<-0
    for (k in 1:K) {
      training<-dat[-flds[[k]],]
      testing<-dat[flds[[k]],]
      fit_k<-glmer(fmla, data=training, family=fm)
      pd<-predict(fit_k,newdata=testing,type="response")
      if(fm=="binomial"){
        pd<-ifelse(pd>0.5,1,0)
        err_k<-sum(pd!=testing$y)/length(pd)
      }else{
        err_k<-mean((testing$y-pd)^2)
      }
      err<-err+err_k
    }
    err<-err/K
    err_c<-c(err_c,err)
  }
  opt<-which.min(err_c)
  return(pi[opt])
}

RWCR<-function(size,y,X,id,type,fm,lambda,pi,SIS=TRUE,K=5){
  dec_WCR<-WCR(size,y,X,id,type,fm,lambda,SIS)
  sec<-K_CV(y,X,id,K,fm,dec_WCR,pi)
  out<-select(dec_WCR,sec)
  return(out)
}
