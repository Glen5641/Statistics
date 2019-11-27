#Working Directory
getwd()

# Question 2
ddt = read.csv("DDT.csv")
m=with(ddt, as.numeric(levels(factor(MILE)))) # A
colm=c()
for(i in 1:length(ddt$MILE)){
  colm[i]=which(ddt$MILE[i]==m) #B
}
coplot(LENGTH ~ WEIGHT | RIVER*SPECIES, ddt, col = colm, pch = colm)
with(ddt, as.numeric(levels(factor(MILE))))
 which(ddt$MILE[i]==m)
with(ddt[ddt$RIVER=="FCM" & ddt$SPECIES=="CCATFISH",],{
  mean(DDT)
})

# Question 5
mtbe = read.csv("MTBE.csv")
mtbeo=na.omit(mtbe)
i=sample(1:223,5,replace=FALSE)
mtbe[i,]
sd(mtbeo[mtbeo$Aquifier=="Bedrock",]$Depth)

# Question 6
eq = read.csv("EARTHQUAKE.csv")
eqsam=sample(1:2929,30,replace=FALSE)
eq[eqsam,]
plot(ts(eq$MAG))
median(eq$MAGNITUDE)

#Question 8
15/106
8/106
63/106
20/106
freq=c(15,8,63,20)
RL=c("None","Both","LegsO","WheelsO")
x=rep(RL,freq)
 
# Question 9
mpfreq=c(32,6,12)
mpt=c("Windows","Explorer","Office")
pie(mpfreq, mpt)
pareto<-function(mpfreq,mn="Microsoft Security",...){
  mpfreq.tab<-table(mpfreq)
  xx.tab<-sort(mpfreq.tab, decreasing=TRUE,index.return=FALSE)
  cs<-cumsum(as.vector(xx.tab))
  lenx<-length(mpfreq.tab)
  bp<-barplot(xx.tab,ylim=c(0,max(cs)),las=2)
  lb<-seq(0,cs[lenx],l=11)
  axis(side=4,at=lb,labels=paste(seq(0,100,length=11),"%",
                                 sep =""),las=1,line=-1,col="Blue",col.axis="Red")
  for(i in 1:(lenx-1)){
    segments(bp[i],cs[i],bp[i+1],cs[i+1],col=i,lwd=2)
  }
  title(main=mn,...)
}
plot(pareto)

# Question 10
swd=read.csv("SWDEFECTS.csv", header=TRUE)
head(swd)
library(plotrix)
tab=table(swd$defect)
rtab=tab/sum(tab)
round(rtab,2)
pie3D(rtab,labels=list("OK","Defective"),main="SWD")

# Question 11
voltage.df<-read.csv("VOLTAGE.csv", header=TRUE)
old<-subset(voltage.df,subset=LOCATION=="OLD")
new<-subset(voltage.df,subset=LOCATION=="NEW")
old$VOLTAGE->vtn
lept<-min(vtn)-0.05
rept<-max(vtn)+0.05
rnge<-rept-lept
inc<-rnge/9
seq(lept, rept,by=inc)->cl
cvtn<-cut(vtn,breaks=cl)
new.tab=table(cvtn)
barplot(new.tab,space=0,main="Frequency Histogram(OLD)",las=2)
stem(vtn)
new$VOLTAGE->vtn
lept<-min(vtn)-0.05
rept<-max(vtn)+0.05
rnge<-rept-lept
inc<-rnge/9
seq(lept, rept,by=inc)->cl
cvtn<-cut(vtn,breaks=cl)
new.tab=table(cvtn)
barplot(new.tab,space=0,main="Frequency Histogram(NEW)",las=2)
mean(old$VOLTAGE)
median(old$VOLTAGE)
which.max(old$VOLTAGE)
mean(new$VOLTAGE)
median(new$VOLTAGE)
which.max(new$VOLTAGE)
(10.5-mean(old$VOLTAGE))/sd(old$VOLTAGE)
(10.5-mean(new$VOLTAGE))/sd(new$VOLTAGE)
windows()
with(old,boxplot(VOLTAGE,ylab="Old Process",col="Blue",notch=TRUE))
old[  (old$VOLTAGE-mean(old$VOLTAGE))/sd(old$VOLTAGE) <= -2 |
        (old$VOLTAGE-mean(old$VOLTAGE))/sd(old$VOLTAGE) >= 2,]
windows()
with(new,boxplot(VOLTAGE,ylab="New Process",col="Red",notch=TRUE))
new[  (new$VOLTAGE-mean(new$VOLTAGE))/sd(new$VOLTAGE) <= -2 |
        (new$VOLTAGE-mean(new$VOLTAGE))/sd(new$VOLTAGE) >= 2,]
windows()
layout(matrix(c(1,2),nr=1,nc=2))
with(old,boxplot(VOLTAGE,ylab="Old Process",col="Blue",notch=TRUE))
with(new,boxplot(VOLTAGE,ylab="New Process",col="Red",notch=TRUE))

# Question 12
RP <- c(1.72,2.5,2.16,2.13,1.06,2.24,2.31,2.03,1.09,1.4,2.57,2.64,1.26,2.05,1.19,2.13,1.27,1.51,2.41,1.95)
mean(RP) - sd(RP)*2
mean(RP) + sd(RP)*2

# Question 13
gobi.df<-read.csv("GOBIANTS.CSV", header=TRUE)
dry.df = within(gobi.df, {
  reg <- ifelse(Region == "Gobi Desert", "GS","DS")
  reg<-factor(reg)
})
des.df = subset(gobi.df,subset=Region=="Gobi Desert")
mean(gobi.df$AntSpecies)
median(gobi.df$AntSpecies)
which.max(gobi.df$AntSpecies)
mean(dry.df[dry.df$reg == "DS",]$PlantCov)
median(dry.df[dry.df$reg == "DS",]$PlantCov)
which.max(dry.df[dry.df$reg == "DS",]$PlantCov)
mean(des.df$PlantCov)
median(des.df$PlantCov)
which.max(des.df$PlantCov)

# Question 14
gal.df<-read.csv("GALAXY2.CSV", header=TRUE)
low.df = gal.df[gal.df$VELOCITY < 21000,]
high.df = gal.df[gal.df$VELOCITY > 21000,]
windows()
plot(gal.df)
mean(low.df)
sd(low.df)
mean(high.df)
sd(high.df)

# Question 15
library(ggplot2)
gg <- ggplot(ddt, aes(x=RIVER, y=LENGTH, color=SPECIES, fill = SPECIES))
gg <- gg + geom_boxplot(colour = "#1F3552")
gg <- gg + ggtitle("Clayton Glenn")
show(gg)
 