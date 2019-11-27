#task1
getwd()

#task2
a=0
b=5
n=10
y=runif(n,a,b)
mu=(a+b)/2
sigma=(b-a)^2/12
X_bar=sum(y/n)
S_sq=sum(y-(mu))^2/(n-1)
T=sum(y)
Y_bar=T/n

myclt=function(n,iter,a=0,b=5){
  y=runif(n*iter,a,b)
  data=matrix(y,nr=n,nc=iter,byrow=TRUE)
  sm=apply(data,2,sum)
  h=hist(sm,plot=FALSE)
  hist(sm,col=rainbow(length(h$mids)),freq=FALSE,main="Distribution of the sum of uniforms")
  curve(dnorm(x,mean=n*(a+b)/2,sd=sqrt(n*(b-a)^2/12)),add=TRUE,lwd=2,col="Blue")
  sm
}
w=myclt(n=10,iter=10000,a=0,b=5)
mean(w)
var(w)

mymean=function(n,iter,a=0,b=5){
  y=runif(n*iter,a,b)
  data=matrix(y,nr=n,nc=iter,byrow=TRUE)
  sm=apply(data,1,mean)
  h=hist(sm,plot=FALSE)
  hist(sm,col=rainbow(length(h$mids)),freq=FALSE,main="Distribution of the means")
  curve(dnorm(x,mean=n*(a+b)/2,sd=sqrt(n*(b-a)^2/12)),add=TRUE,lwd=2,col="Blue")
  sm
}
wmeans=mymean(n=10,iter=10000,a=0,b=5)
mean(wmeans)
var(wmeans)

#Task3
mycltu=function(n,iter,a=0,b=10){
  y=runif(n*iter,a,b)
  data=matrix(y,nr=n,nc=iter,byrow=TRUE)
  w=apply(data,2,mean)
  param=hist(w,plot=FALSE)

  ymax=max(param$density)
  ymax=1.1*ymax
  hist(w,freq=FALSE,  ylim=c(0,ymax), main=paste("Histogram of sample mean",
                        "\n", "sample size= ",n,sep=""),xlab="Sample mean")
  lines(density(w),col="Blue",lwd=3) # add a density plot
  curve(dnorm(x,mean=(a+b)/2,sd=(b-a)/(sqrt(12*n))),add=TRUE,col="Red",lty=2,lwd=3) 
  curve(dunif(x,a,b),add=TRUE,lwd=4)
  
}
mycltu(n=1,iter=10000, a=0, b=10)
mycltu(n=2,iter=10000, a=0, b=10)
mycltu(n=3,iter=10000, a=0, b=10)
mycltu(n=5,iter=10000, a=0, b=10)
mycltu(n=10,iter=10000, a=0, b=10)
mycltu(n=30,iter=10000, a=0, b=10)

#Task 4
mycltb=function(n,iter,p=0.5){
  y=rbinom(n*iter,size=n,prob=p)
  data=matrix(y,nr=n,nc=iter,byrow=TRUE)
  w=apply(data,2,mean)
  param=hist(w,plot=FALSE)
  
  ymax=max(param$density)
  ymax=1.1*ymax
  hist(w,freq=FALSE,  ylim=c(0,ymax),
       main=paste("Histogram of sample mean","\n", "sample size= ",n,sep=""),
       xlab="Sample mean",...)
  curve(dnorm(x,mean=n*p,sd=sqrt(p*(1-p))),add=TRUE,col="Red",lty=2,lwd=3) 
  
}

mycltb(n=4,iter=10000,p=0.3)
mycltb(n=5,iter=10000,p=0.3)
mycltb(n=10,iter=10000,p=0.3)
mycltb(n=20,iter=10000,p=0.3)
mycltb(n=4,iter=10000,p=0.7)
mycltb(n=5,iter=10000,p=0.7)
mycltb(n=10,iter=10000,p=0.7)
mycltb(n=20,iter=10000,p=0.7)
mycltb(n=4,iter=10000,p=0.5)
mycltb(n=5,iter=10000,p=0.5)
mycltb(n=10,iter=10000,p=0.5)
mycltb(n=20,iter=10000,p=0.5)

#Task 5
mycltp=function(n,iter,lambda=10){
  y=rpois(n*iter,lambda=lambda)
  data=matrix(y,nr=n,nc=iter,byrow=TRUE)
  w=apply(data,2,mean)
  param=hist(w,plot=FALSE)
  ymax=max(param$density)
  ymax=1.1*ymax
  layout(matrix(c(1,1,2,3),nr=2,nc=2, byrow=TRUE))
  hist(w,freq=FALSE,  ylim=c(0,ymax), col=rainbow(max(w)),
       main=paste("Histogram of sample mean","\n", "sample size= ",n," iter=",iter," lambda=",lambda,sep=""),
       xlab="Sample mean",...)
  curve(dnorm(x,mean=lambda,sd=sqrt(lambda/n)),add=TRUE,col="Red",lty=2,lwd=3)
  barplot(table(y)/(n*iter),col=rainbow(max(y)), main="Barplot of sampled y", ylab ="Rel. Freq",xlab="y" )
  x=0:max(y)
  plot(x,dpois(x,lambda=lambda),type="h",lwd=5,col=rainbow(max(y)),
       main="Probability function for Poisson", ylab="Probability",xlab="y")
}
mycltp(n=2,iter=10000, lambda = 4)
mycltp(n=3,iter=10000, lambda = 4)
mycltp(n=5,iter=10000, lambda = 4)
mycltp(n=10,iter=10000, lambda = 4)
mycltp(n=20,iter=10000, lambda = 4)
mycltp(n=2,iter=10000, lambda = 10)
mycltp(n=3,iter=10000, lambda = 10)
mycltp(n=5,iter=10000, lambda = 10)
mycltp(n=10,iter=10000, lambda = 10)
mycltp(n=20,iter=10000, lambda = 10)