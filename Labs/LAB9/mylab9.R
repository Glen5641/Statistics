#Task 1

getwd()



#Task 2

set.seed(35)
sam = round(rnorm(20,mean=10,sd=4),2)
unique(sample(sam,20,replace=TRUE))
unique(sample(sam,20,replace=TRUE))
unique(sample(sam,20,replace=TRUE))
unique(sample(sam,20,replace=TRUE))
unique(sample(sam,20,replace=TRUE))

unique(sample(sam,20,replace=FALSE))
unique(sample(sam,20,replace=FALSE))
unique(sample(sam,20,replace=FALSE))
unique(sample(sam,20,replace=FALSE))
unique(sample(sam,20,replace=FALSE))


             
#Task 3

myboot2<-function(iter=10000,x,fun="mean",alpha=0.05,cx=1.5){ 
  n=length(x)
  y=sample(x,n*iter,replace=TRUE)
  rs.mat=matrix(y,nr=n,nc=iter,byrow=TRUE)
  xstat=apply(rs.mat,2,fun)
  ci=quantile(xstat,c(alpha/2,1-alpha/2))
  para=hist(xstat,freq=FALSE,las=1, main=paste("Histogram of Bootstrap sample statistics",
                "\n","alpha=",alpha," iter=",iter,sep=""))
  mat=matrix(x,nr=length(x),nc=1,byrow=TRUE)
  pte=apply(mat,2,fun)
  abline(v=pte,lwd=3,col="Black")
  segments(ci[1],0,ci[2],0,lwd=4)   
  text(ci[1],0,paste("(",round(ci[1],2),sep=""),col="Red",cex=cx)
  text(ci[2],0,paste(round(ci[2],2),")",sep=""),col="Red",cex=cx)
  text(pte,max(para$density)/2,round(pte,2),cex=cx)
  return(list(ci=ci,fun=fun,x=x, y=y))
}
set.seed(39); sam=rnorm(25,mean=25,sd=10)
x = myboot2(10000,x=sam,fun="mean",alpha=0.05,xlab="mean",col="Purple",cx=1.5)
mean(x$ci) - mean(sam)
set.seed(30); sam=rchisq(20,df=3)
x = myboot2(10000,x=sam,fun="mean",alpha=0.05,xlab="mean",col="Purple",cx=1.5)
mean(x$ci) - mean(sam)
set.seed(40); sam=rgamma(30,shape=2,scale=3)
x= myboot2(10000,x=sam,fun="mean",alpha=0.05,xlab="mean",col="Purple",cx=1.5)
mean(x$ci) - mean(sam)
set.seed(10); sam=rbeta(20,shape1=3,shape2=4)
x=myboot2(10000,x=sam,fun="mean",alpha=0.05,xlab="mean",col="Purple",cx=1.5)
mean(x$ci) - mean(sam)


set.seed(39); sam=rnorm(25,mean=25,sd=10)
myboot2(10000,x=sam,fun="var",alpha=0.2,xlab="Variance",col="Purple",cx=1.5)
set.seed(30); sam=rchisq(20,df=3)
myboot2(10000,x=sam,fun="var",alpha=0.2,xlab="Variance",col="Purple",cx=1.5)
set.seed(40); sam=rgamma(30,shape=2,scale=3)
myboot2(10000,x=sam,fun="var",alpha=0.2,xlab="Variance",col="Purple",cx=1.5)
set.seed(10); sam=rbeta(20,shape1=3,shape2=4)
myboot2(10000,x=sam,fun="var",alpha=0.2,xlab="Variance",col="Purple",cx=1.5)



#Task 4

mybootmed<-function(iter=10000,x,fun="median",alpha=0.05,cx=1.5){ 
  n=length(x)
  y=sample(x,n*iter,replace=TRUE)
  rs.mat=matrix(y,nr=n,nc=iter,byrow=TRUE)
  xstat=apply(rs.mat,2,fun)
  ci=quantile(xstat,c(alpha/2,1-alpha/2))
  para=hist(xstat,freq=FALSE,las=1, main=paste("Histogram of Bootstrap sample statistics",
                                               "\n","alpha=",alpha," iter=",iter,sep=""))
  mat=matrix(x,nr=length(x),nc=1,byrow=TRUE)
  pte=apply(mat,2,fun)
  abline(v=pte,lwd=3,col="Black")
  segments(ci[1],0,ci[2],0,lwd=4)   
  text(ci[1],0,paste("(",round(ci[1],2),sep=""),col="Red",cex=cx)
  text(ci[2],0,paste(round(ci[2],2),")",sep=""),col="Red",cex=cx)
  text(pte,max(para$density)/2,round(pte,2),cex=cx)
  return(list(xstat=xstat,ci=ci,fun=fun,x=x))
}

sam=c(1,1,1,2,2,2,2,3,3,3,4,4)
x = mybootmed(x=sam,fun="median")
barplot(x$xstat)


#Task 5

mybootdiv<-function(iter=10000,x,fun="mean",alpha=0.05,cx=1.5, xlab = "Mean/Median"){ 
  n=length(x)
  y=sample(x,n*iter,replace=TRUE)
  rs.mat=matrix(y,nr=n,nc=iter,byrow=TRUE)
  xstat=apply(rs.mat,2,fun)
  ci=quantile(xstat,c(alpha/2,1-alpha/2))
  para=hist(xstat,freq=FALSE,las=1, main=paste("Histogram of Bootstrap sample statistics",
                                               "\n","alpha=",alpha," iter=",iter,sep=""))
  mat=matrix(x,nr=length(x),nc=1,byrow=TRUE)
  pte=apply(mat,2,fun)
  abline(v=pte,lwd=3,col="Black")
  segments(ci[1],0,ci[2],0,lwd=4)   
  text(ci[1],0,paste("(",round(ci[1],2),sep=""),col="Red",cex=cx)
  text(ci[2],0,paste(round(ci[2],2),")",sep=""),col="Red",cex=cx)
  text(pte,max(para$density)/2,round(pte,2),cex=cx)
  return(list(xstat=xstat,ci=ci,fun=fun,x=x))
}
set.seed(39); sam=rnorm(25,mean=25,sd=10)
mybootdiv(10000,x=sam, alpha=0.05,xlab="mean/median",cx=1.5)
set.seed(30); sam=rchisq(20,df=3)
mybootdiv(10000,x=sam,alpha=0.05,xlab="mean/median",cx=1.5)
set.seed(40); sam=rgamma(30,shape=2,scale=3)
mybootdiv(10000,x=sam,alpha=0.05,xlab="mean/median",cx=1.5)
set.seed(10); sam=rbeta(20,shape1=3,shape2=4)
mybootdiv(10000,x=sam,alpha=0.05,xlab="mean/median",cx=1.5)

set.seed(39); sam=rnorm(25,mean=25,sd=10)
mybootdiv(10000,x=sam,alpha=0.3,xlab="mean/median",cx=1.5)
set.seed(30); sam=rchisq(20,df=3)
mybootdiv(10000,x=sam,alpha=0.3,xlab="mean/median",cx=1.5)
set.seed(40); sam=rgamma(30,shape=2,scale=3)
mybootdiv(10000,x=sam,alpha=0.3,xlab="mean/median",cx=1.5)
set.seed(10); sam=rbeta(20,shape1=3,shape2=4)
mybootdiv(10000,x=sam,alpha=0.3,xlab="mean/median",cx=1.5)



#Task 6
?Distributions
set.seed(39); sam=rcauchy(20,location = 0, scale = 2)
x = myboot2(10000,x=sam,fun="mean",alpha=0.2,xlab="mean",col="Purple",cx=1.5)
set.seed(39); sam=rpois(20, lambda = 4)
x = myboot2(10000,x=sam,fun="mean",alpha=0.2,xlab="mean",col="Purple",cx=1.5)
set.seed(39); sam=rf(20,df1 = 1,df2 = Inf)
x = myboot2(10000,x=sam,fun="mean",alpha=0.2,xlab="mean",col="Purple",cx=1.5)
set.seed(39); sam=rlnorm(20, meanlog = 0, sdlog = 1)
x = myboot2(10000,x=sam,fun="mean",alpha=0.2,xlab="mean",col="Purple",cx=1.5)



#Task 7
set.seed(68); sam=rnorm(20,mean=10,sd=4)
x = myboot2(10000,x=sam,fun="sd",alpha=0.05,xlab="sd",col="Purple",cx=1.5)
x = myboot2(10000,x=x$x,fun="sd",alpha=0.05,xlab="sd",col="Purple",cx=1.5)