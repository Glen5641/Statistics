#Task 1 - Get Working Directory
getwd()

#Task 2 - Read Data File and show head
fin.df=read.table(file.choose(),header=TRUE,sep=",")
head(fin.df)

#Task 3 - Show Scatter Plot, TrendScatters, find Regression Line, create Linear Model
with(fin.df, 
     plot(HEAT~RATIO,bg="Blue",pch=21,ylim=c(0,1.1*max(HEAT)),xlim=c(0,1.1*max(RATIO)))
)
library(s20x)
trendscatter(HEAT~RATIO,f=0.7, data=fin.df)
ht.lm=with(fin.df, lm(HEAT~RATIO))
abline(ht.lm)
with(fin.df, 
     plot(HEAT~RATIO,bg="Blue",pch=21,ylim=c(0,1.1*max(HEAT)),xlim=c(0,1.1*max(RATIO)))
)

#Task 4 - Find RSS, TSS, MSS, Layout and create 4 plots with titles
yhat=with(fin.df,predict(ht.lm,data.frame(RATIO)))
RSS=with(fin.df,sum((HEAT-yhat)^2))
MSS=with(fin.df,sum((yhat-mean(HEAT))^2))
TSS=with(fin.df,sum((HEAT-mean(HEAT))^2))
layout(matrix(1:4,nr=2,nc=2,byrow=TRUE))
with(fin.df, 
     plot(HEAT~RATIO,bg="Blue",pch=21,ylim=c(0,1.1*max(HEAT)),xlim=c(0,1.1*max(RATIO)))
)
abline(ht.lm)
mtext("Dr Stewart's plot",side=3)
segments(RATIO,HEAT,RATIO,yhat)
with(fin.df, segments(RATIO,HEAT,RATIO,mean(HEAT),col="Green"))
with(fin.df, segments(RATIO,mean(HEAT),RATIO,yhat,col="Red"))
RSS + MSS
MSS / TSS

#Task 5 - Summarize the Linear Model
summary(ht.lm)
coef(ht.lm)
predict(ht.lm, data.frame(RATIO=c(2,2.3,2.5)))
anova(ht.lm)

#Task 6 - Create GGPLOT of the data file with regression line
library(ggplot2)
g=ggplot(fin.df, aes(x=RATIO,y=HEAT,colour=RATIO))
g=g+geom_point() + geom_line()+ geom_smooth(method="lm")
g+ggtitle("HEAT Vs RATIO")