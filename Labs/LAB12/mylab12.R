#Task1

getwd()

#Task2

##Set Data
set.seed(55);x1=rnorm(30,mean=25,sd=5)

##TTests
t.test(x1,mu=22)
t.test(x1,mu=23)
t.test(x1,mu=24)
t.test(x1,mu=25)
t.test(x1,mu=26)

##Boxplot with Confidence intervals and Mean
boxplot(x1, main="Sample x1")
ci=t.test(x1,mu=22)$conf.int
ci
abline(h=c(ci,mean(x1)),col=c("Red","Red","Green"))

##TCalc
tcalc=(mean(x1)-24)/(sd(x1)/sqrt(30))
tcalc

##P-Value
mypvalue=function(t0,xmax=4,n=20, alpha=0.05){
  
  va=round(pt(-t0,df=n-1),4)
  pv=2*va
  
  curve(dt(x,df=n-1),xlim=c(-xmax,xmax),ylab="T Density",xlab=expression(t),
        main=substitute(paste("P-value=", pv, " alpha=", alpha)))
  
  xcurve=seq(t0,xmax,length=1000)
  ycurve=dt(xcurve,df=n-1)
  
  xlcurve=seq(-t0,-xmax,length=1000)
  ylcurve=dt(xcurve,df=n-1)
  
  polygon(c(t0,xcurve,xmax),c(0,ycurve,0),col="green")
  polygon(c(-t0,xlcurve,-xmax),c(0,ylcurve,0),col="green")
 
  q=qt(1-alpha/2,n-1)
  abline( v=c(q,-q),lwd=2) 
  axis(3,c(q,-q),c(expression(abs(t[alpha/2])),expression(-abs(t[alpha/2]))))
  
  text(0.5*(t0+xmax),max(ycurve),substitute(paste(area, "=",va)))
  text(-0.5*(t0+xmax),max(ycurve),expression(area))
  
  return(list(q=q,pvalue=pv))
}

##PValue with TCalc of 24
mypvalue(tcalc,n=30,alpha=0.05)

##Bootstrap
bootpval<-function(x,conf.level=0.95,iter=3000,mu0=0, test="two"){
  n=length(x)
  y=x-mean(x)+mu0  
  rs.mat<-c()    
  xrs.mat<-c()
  for(i in 1:iter){ 
    rs.mat<-cbind(rs.mat,sample(y,n,replace=TRUE)) 
    xrs.mat<-cbind(xrs.mat,sample(x,n,replace=TRUE)) 
    
  }
  
  tstat<-function(z){ 
    sqrt(n)*(mean(z)-mu0)/sd(z)
  }
  
  tcalc=tstat(x) 
  ytstat=apply(rs.mat,2,tstat) 
  xstat=apply(xrs.mat,2,mean)  
  alpha=1-conf.level 
  ci=quantile(xstat,c(alpha/2,1-alpha/2))
  pvalue=ifelse(test=="two",length(ytstat[ytstat>abs(tcalc) | ytstat < -abs(tcalc)])/iter,
                ifelse(test=="upper",length(ytstat[ytstat>tcalc])/iter,
                       length(ytstat[ytstat<xstat])/iter))
  
  h=hist(ytstat,plot=FALSE)
  mid=h$mid
  if(test=="two"){
    ncoll=length(mid[mid<= -abs(tcalc)])
    ncolr=length(mid[mid>=  abs(tcalc)])
    col=c(rep("Green",ncoll),rep("Gray",length(mid)-ncoll-ncolr),rep("Green",ncolr))
  }
  if(test=="upper"){
    ncolr=length(mid[mid>=  abs(tcalc)])
    col=c(rep("Gray",length(mid)-ncolr),rep("Green",ncolr))
  }
  
  if(test=="lower"){
    ncoll=length(mid[mid<=  -abs(tcalc)])
    col=c(rep("Green",ncoll),rep("Gray",length(mid)-ncoll))
  }
  hist(ytstat,col=col,freq=FALSE,las=1,main="",xlab=expression(T[stat]))
  segments(ci[1],0,ci[2],0,lwd=2)
  pround=round(pvalue,4)
  title(substitute(paste(P[value],"=",pround)))
  return(list(pvalue=pvalue,tcalc=tcalc,n=n,x=x,test=test,ci=ci))
}


bootpval(x=x1,mu0=22,test="two")
bootpval(x=x1,mu0=23,test="two")
bootpval(x=x1,mu0=24,test="two")
bootpval(x=x1,mu0=25,test="two")
bootpval(x=x1,mu0=26,test="two")


#Task3

set.seed(30);x=rnorm(15,mean=10,sd=7)   
set.seed(40);y=rnorm(20,mean=12,sd=4)

boxplot(list(x=x,y=y))

var.test(x,y)

t.test(x,y, mu=0,var.equal=TRUE)
t.test(x,y, mu=2,var.equal=TRUE)

#Task4

set.seed(30);x=rnorm(15,mean=10,sd=4)   
set.seed(40);y=rnorm(20,mean=12,sd=4)

var.test(x,y)

t.test(x,y, mu=0,var.equal=FALSE)
t.test(x,y, mu=2,var.equal=FALSE)

#Task5

boot2pval<-function(x1,x2,conf.level=0.95,iter=3000,mudiff=0, test="two"){
  n1=length(x1)
  n2=length(x2)
  y1=x1-mean(x1)+mean(c(x1,x2))  # transform the data so that it is centered at the NULL
  y2=x2-mean(x2)+mean(c(x1,x2))
  y1rs.mat<-c()    #rs.mat will be come a resample matrix -- now it is an empty vector
  x1rs.mat<-c()
  y2rs.mat<-c()
  x2rs.mat<-c()
  for(i in 1:iter){ # for loop - the loop will go around iter times
    y1rs.mat<-cbind(y1rs.mat,sample(y1,n1,replace=TRUE)) #sampling from y cbind -- column bind -- binds the vectors together by columns
    y2rs.mat<-cbind(y2rs.mat,sample(y2,n2,replace=TRUE))
    
  }
  x1rs.mat<-y1rs.mat+mean(x1)-mean(c(x1,x2))
  x2rs.mat<-y2rs.mat+mean(x2)-mean(c(x1,x2))
  
  xbar1=mean(x1)
  xbar2=mean(x2)
  sx1sq=var(x1)
  sx2sq=var(x2)
  
  tcalc=(xbar1-xbar2-mudiff)/sqrt(sx1sq/n1+sx2sq/n2)
  
  sy1sq=apply(y1rs.mat,2,var)
  sy2sq=apply(y2rs.mat,2,var) 
  y1bar=apply(y1rs.mat,2,mean)
  y2bar=apply(y2rs.mat,2,mean)
  
  tstat=(y1bar-y2bar-mudiff)/sqrt(sy1sq/n1+sy2sq/n2)
  
  
  alpha=1-conf.level
  pvalue=ifelse(test=="two",length(tstat[tstat>abs(tcalc) | tstat < -abs(tcalc)])/iter,
                ifelse(test=="upper",length(tstat[tstat>tcalc])/iter,
                       length(ytstat[tstat<tcalc])/iter))
  
  h=hist(tstat,plot=FALSE)
  mid=h$mid
  if(test=="two"){
    ncoll=length(mid[mid<= -abs(tcalc)])
    ncolr=length(mid[mid>=  abs(tcalc)])
    col=c(rep("Green",ncoll),rep("Gray",length(mid)-ncoll-ncolr),rep("Green",ncolr))
  }
  hist(tstat,col=col,freq=FALSE)

  return(list(pvalue=pvalue))
}

set.seed(30);x=rnorm(15,mean=10,sd=7)   
set.seed(40);y=rnorm(20,mean=12,sd=4)
boot2pval(x1=x,x2=y)

#Task6

set.seed(30);x=rnorm(15,mean=10,sd=4)   
set.seed(40);y=rnorm(20,mean=12,sd=4)
boot2pval(x1=x,x2=y)

#Task7

