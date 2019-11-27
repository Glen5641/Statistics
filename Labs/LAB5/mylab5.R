#Task1

#Get working directory
getwd()


#Task2

#Show probability
mybin = function(iter = 100, n = 10, p = 0.7){ 
  #make a matrix to hold the samples
  #initially filled with NA's
  sam.mat = matrix(NA, nr = n, nc = iter, byrow = TRUE)
  #Make a vector to hold the number of successes in each trial
  succ = c()
  for( i in 1:iter){
    #Fill each column with a new sample
    sam.mat[, i] = sample(c(1, 0), n, replace = TRUE, prob = c(p, 1-p))
    #Calculate a statistic from the sample (this case it is the sum)
    succ[i] = sum(sam.mat[, i])
  }
  #Make a table of successes
  succ.tab = table(factor(succ, levels = 0:n))
  #Make a barplot of the proportions
  barplot(succ.tab / (iter), col = rainbow(n+1), 
          main = sprintf("Binomial simulation of %d Iterations", iter), 
          xlab = "Number of successes", ylab = "Probability of Success")
  succ.tab / iter
}
mybin(iter = 100,   n = 10, p = 0.7)
mybin(iter = 200,   n = 10, p = 0.7)
mybin(iter = 500,   n = 10, p = 0.7)
mybin(iter = 1000,  n = 10, p = 0.7)
mybin(iter = 10000, n = 10, p = 0.7)

#Check binomial plots for accuracy
?dbinom
dbinom(x = 0:10, size = 10, prob = 0.7)

#Task3

#Use sample to create a 12:8 marble scenario
sample(rep(c(1,0),c(8,12)),5,replace=FALSE)
sample(rep(c(1,0),c(8,12)),5,replace=TRUE)

#Hypergeometric function
myhyper=function(iter=100,N=20,r=12,n=5){
  # make a matrix to hold the samples
  #initially filled with NA's
  sam.mat=matrix(NA,nr=n,nc=iter, byrow=TRUE)
  #Make a vector to hold the number of successes over the trials
  succ=c()
  for( i in 1:iter){
    #Fill each column with a new sample
    sam.mat[,i]=sample(rep(c(1,0),c(r,N-r)),n,replace=FALSE)
    #Calculate a statistic from the sample (this case it is the sum)
    succ[i]=sum(sam.mat[,i])
  }
  #Make a table of successes
  succ.tab=table(factor(succ,levels=0:n))
  #Make a barplot of the proportions
  barplot(succ.tab/(iter), col=rainbow(n+1), 
          main=sprintf("HYPERGEOMETRIC simulation of %d Iterations", iter), 
          xlab="Number of successes", ylab = "Probability of Success")
  succ.tab/iter
}
myhyper(iter=100,n=5, N=20,r=12)
myhyper(iter=200,n=5, N=20,r=12)
myhyper(iter=500,n=5, N=20,r=12)
myhyper(iter=1000,n=5, N=20,r=12)
myhyper(iter=10000,n=5, N=20,r=12)

#Check the HyperGeom Plots
?dhyper
dhyper(x=0:5, m=12, n=8, k=5)


#Task 4

#Show 30 iterations of plots with 1 sec time lag
mysample=function(n, iter=10,time=0.5){
  for( i in 1:iter){
    #make a sample
    s=sample(1:10,n,replace=TRUE)
    # turn the sample into a factor
    sf=factor(s,levels=1:10)
    #make a barplot
    barplot(table(sf)/n,beside=TRUE,col=rainbow(10), 
            main=paste("Example sample()", " iteration ", i, " n= ", n,sep="") ,
            ylim=c(0,0.2)
    )
    
    #release the table
    Sys.sleep(time)
  }
}
mysample(n=1000, iter=30, time=1)

#Task5
#8 choose 4
choose(8,4)

#Poisson function
ppois(4, lambda=2)

#neg binom function
dnbinom(10,3,0.4)

#sum of vectors 0:8 of binom function
sum(dbinom(0:8,15,0.4))



#Task6
#Negative Binomial Function without any generic function calls
mynbin=function(y,r,p){
  num=1
  denum1=1
  denum2=1
  pr = 1
  tail = 1
  for(i in 1:(y-1)){
    num = num * i
  }
  for(i in 1:(r-1)){
    denum1 = denum1 * i
  }
  for(i in 1:((y-1)-(r-1))){
    denum2 = denum2 * i
  }
  for(i in 1:r){
    pr = pr * p
  }
  for(i in 1:(y-r)){
    tail = tail * (1-p)
  }
  num/denum1/denum2*pr*tail
}
mynbin(10,3,0.4)

