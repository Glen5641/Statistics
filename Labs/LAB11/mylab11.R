#Task 1

getwd()



#Task 2

##Initiallize data and empty array

d=c(5.0581, 4.9707, 5.0893, 4.9334, 4.9777, 5.0285, 4.8555, 4.9565, 
    4.9769, 4.9722, 4.999, 4.9925, 4.9686, 5.0662, 4.9239, 4.9781, 
    5.0485, 5.0014, 4.9957, 5.0195, 5.0118, 4.9928, 5.0361, 5.0185, 
    4.9879)
ci=c()

##95% Interval for Mean

t=qt(0.95/2,24)
ci[1]=mean(d)-t*sd(d)/sqrt(25)
ci[2]=mean(d)+t*sd(d)/sqrt(25)
ci

##90% Interval for Mean

t=qt(0.90/2,24)
ci[1]=mean(d)-t*sd(d)/sqrt(25)
ci[2]=mean(d)+t*sd(d)/sqrt(25)
ci

##80% Interval for Mean

t=qt(0.80/2,24)
ci[1]=mean(d)-t*sd(d)/sqrt(25)
ci[2]=mean(d)+t*sd(d)/sqrt(25)
ci

##50% Interval for Mean

t=qt(0.50/2,24)
ci[1]=mean(d)-t*sd(d)/sqrt(25)
ci[2]=mean(d)+t*sd(d)/sqrt(25)
ci

##80% T-test Interval for Mean

t.test(d, conf.level = 0.80)$conf.int

##95% Interval for Variance

chia = qchisq(0.95/2,24)
chib = qchisq(1-0.95/2,24)
ci[1]=(24*var(d))/(chib^2)
ci[2]=(24*var(d))/(chia^2)
ci

##90% Interval for Variance

chia = qchisq(0.90/2,24)
chib = qchisq(1-0.90/2,24)
ci[1]=(24*var(d))/(chib^2)
ci[2]=(24*var(d))/(chia^2)
ci

##80% Interval for Variance

chia = qchisq(0.80/2,24)
chib = qchisq(1-0.80/2,24)
ci[1]=(24*var(d))/(chib^2)
ci[2]=(24*var(d))/(chia^2)
ci

##50% Interval for Variance

chia = qchisq(0.50/2,24)
chib = qchisq(1-0.50/2,24)
ci[1]=(24*var(d))/(chib^2)
ci[2]=(24*var(d))/(chia^2)
ci



#Task 3

##Data

blue=c(21.65, 17.48, 20.1, 21.57, 14.82, 19.17, 21.08, 18.23, 22.93, 15.66, 
       20.89, 21.66, 18.5, 20.59, 18.63, 18.91, 19.53, 17.7, 16.5, 19.03)
snapper=c(31.65, 27.48, 30.1, 31.57, 24.82, 29.17, 31.08, 28.23, 32.93, 
          25.66, 30.89, 31.66, 28.5, 30.59, 28.63)

##95% confidence interval for muS - muB

n1=length(blue)
n2=length(snapper)
spsq=((n1-1)*var(blue)+(n2-1)*var(snapper))/(n1+n2-2)
t=qt(0.95,n1+n2-2)
ci=c()
ci[1]=mean(blue)-mean(snapper)-t*sqrt(spsq/(n1+n2)) 
ci[2]=mean(blue)-mean(snapper)+t*sqrt(spsq/(n1+n2))
ci

##Ttest for confidence interval of mean

t.test(x = blue, y = snapper, conf.level = 0.95, var.equal=TRUE)$conf.int
t.test(x = blue, y = snapper, conf.level = 0.85, var.equal=TRUE)$conf.int
t.test(x = blue, y = snapper, conf.level = 0.75, var.equal=TRUE)$conf.int
t.test(x = blue, y = snapper, conf.level = 0.25, var.equal=TRUE)$conf.int



#Task 4

##Data

Exam1=c(40.98, 59.36, 46.69, 41.8, 61.63, 65.31, 62.96, 60.21, 56.89, 
        78.41, 53.44, 75.2, 60.54, 52.43, 41.41, 70.79, 73.55, 55.65, 
        61.43, 63.84, 58.07, 53.79, 54.45, 67.18, 44.46)

Exam2=c(50.22, 66.19, 58.75, 51.88, 66.61, 70.86, 74.25, 70.23, 69.55, 
        87.18, 63.62, 81.7, 70.5, 66.02, 51.35, 80.92, 85.65, 65.44, 
        74.37, 75.28, 67.86, 59.92, 64.42, 73.57, 57.15)

##95% confidence interval for mu1 - mu2

n1=length(Exam1)
n2=length(Exam2)
ci[1]=mean(Exam1)-mean(Exam2)-0.065*sd(c(Exam1, Exam2))*sqrt((n1+n2)) 
ci[2]=mean(Exam1)-mean(Exam2)+0.065*sd(c(Exam1, Exam2))*sqrt((n1+n2))
ci

##T tests for Exam1 and Exam 2 means

t.test(x = Exam1, y = Exam2, conf.level = 0.90, var.equal=FALSE)$conf.int
t.test(x = Exam1, y = Exam2, conf.level = 0.80, var.equal=FALSE)$conf.int
t.test(x = Exam1, y = Exam2, conf.level = 0.70, var.equal=FALSE)$conf.int
t.test(x = Exam1, y = Exam2, conf.level = 0.60, var.equal=FALSE)$conf.int
t.test(x = Exam1, y = Exam2, conf.level = 0.10, var.equal=FALSE)$conf.int



#Task 5

##Data

birds.df <- read.csv("NZBIRDS.csv", header = TRUE, sep = ",")
with(birds.df, table(Extinct,Flight))

##95% confidence interval for proportion

phat1 <- 21/28
phat2 <- 7/28
ci[1] <- phat1 - phat2 - .065*sqrt(21*7/28+7*21/28)
ci[2] <- phat1 - phat2 + .065*sqrt(21*7/28+7*21/28)
ci



#Task 6

##Data 

set.seed(35)
sam1=rnorm(25,mean=10,sd=5)
sam1
set.seed(45)
sam2=rnorm(34,mean=40,sd=8)

##95% confidence interval for sigma2 of 1 / sigma2 of 2

t = qt(0.95, 33+24)
ci[1]=var(sam1)/(var(sam2)*t)
ci[2]=var(sam1)*t/(var(sam2))
ci

##Variance test for the 2 random samples with multiple confidence intervals

var.test(sam1, sam2, ratio = 1/2, conf.level = 0.80)$conf.int
var.test(sam1, sam2, ratio = 1/2, conf.level = 0.70)$conf.int
var.test(sam1, sam2, ratio = 1/2, conf.level = 0.60)$conf.int
var.test(sam1, sam2, ratio = 1/2, conf.level = 0.50)$conf.int
