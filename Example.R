n<-150; n.p<-100
#Generate cluster sizes
sub.size<-rnbinom(n,mu=200,size=4); id<-rep(1:n,times=sub.size)
#Generate the covariate matrix from random-intercept models
X<-c()
for (i in 1:n.p) {
  xi<-c()
  for (j in unique(id)) {
    sizej<-length(id[which(id==j)])
    muj<-rnorm(n=1,mean=0,sd=0.5)
    xij<-rnorm(n=sizej,mean=muj)
    xi<-c(xi,xij)
  }
  X<-cbind(X,xi)
}

beta<-c(0.7,-0.7,-0.4,0.6,-0.6,rep(0,95))
a<-0.4

resamplesize<-2000
pi_all<-c(0.35,0.37,0.4,0.45)

dat2<-data.frame(X,id)
#Generate the binary outcomes based on logistic model
y<-c()
for (j in unique(dat2$id)) {
  datj<-dat2[which(dat2$id==j),]
  d<-dim(datj)[1]
  logitpj<-as.vector(as.matrix(datj[,-101])%*%beta)+rnorm(1,mean=0,sd=a)
  pj<-exp(logitpj)/(1+exp(logitpj))
  yj<-rbinom(d,1,pj)
  y<-c(y,yj)
}
X<-dat2[,-101]
name<-c()
for (i in 1:100) {
  name<-c(name,paste("X",i,sep=""))
}
names(X)<-name
X<-as.matrix(X)

lambda<-seq(0.15,0.0015,length.out=100)
#RWCR with LASSO penalty and SIS step
out<-RWCR(resamplesize,y,X,id,"lasso","binomial",lambda,pi_all)



