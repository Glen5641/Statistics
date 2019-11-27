# Working Directory
getwd()

# Data of Electric Prices from 1960 - 2011
ep.df <- read.csv("ep.csv")
head(ep.df)

# Variables of the data file
names(ep.df)

# Plot of Electric Prices from 1960 - 2011
library(ggplot2)
x = 0:(length(ep.df$YEAR)-1)
ep.lm <- lm(TOT ~ x, data = ep.df)
g = ggplot(ep.lm, aes(x = x, y = TOT, color = x)) + geom_point()
g = g + ylab("Electric Prices (Cents per Kilowatthour)") + xlab("Year") + ggtitle("Electric Price ~ Year", subtitle = NULL)
g = g + geom_smooth(method = "loess")
g

# SLR Theory
mysummary = function(x=x,y=y){
  ssxx  = sum((x - mean(x)) ^ 2)
  ssxy  = sum((x - mean(x)) * (y - mean(y)))
  ssyy  = sum((y - mean(y)) ^ 2)
  b1hat = ssxy / ssxx
  b0hat = mean(y) - b1hat * mean(x)
  yhat  = b0hat + b1hat * x
  rss   = sum((y - yhat) ^ 2)
  mss   = sum((yhat - mean(y)) ^ 2)
  tss   = sum((y - mean(y)) ^ 2)
  yerr  = y - yhat
  rerr  = y - b0hat - b1hat*x
  sdhat = sd(y) / ssxx
  r     = ssxy / sqrt(ssxx*ssyy)
  return(list(ssxx = ssxx, ssxy = ssxy, ssyy = ssyy, b1hat = b1hat, b0hat = b0hat, yhat = yhat, rss = rss, mss = mss, tss = tss, yerr = yerr, rerr = rerr, sdhat = sdhat, r = r))
}
ep.summary = mysummary(x, ep.df$TOT)

# Sum of Squares of X
ep.summary$ssxx
var(x) * (length(x) - 1)

# Sum of Squares of XY
ep.summary$ssxy
cov(x,ep.df$TOT) * (length(x) - 1)

# Slope of Linear Model
ep.summary$b1hat

# Intercept of Linear Model
ep.summary$b0hat

# Residual of Xi, Error
ep.summary$yerr

# Residual Sum of Squares or Error Sum of Squares
ep.summary$rss
sum(ep.summary$yerr^2)

# Model Sum of Squares
ep.summary$mss

# Total Sum of Squares or Sum of Squares of Y
ep.summary$tss
var(ep.df$TOT) * (length(ep.df$TOT) - 1)
ep.summary$mss + ep.summary$rss

# Equation of Linear Model
ep.summary$yhat

# Equation of Data
ep.summary$b0hat+ep.summary$b1hat*x+ep.summary$rerr
ep.df$TOT

# Electric Price Linear Model
plot(ep.df$TOT~x, ylim = c(0, max(ep.df$TOT) + 1), xlim = c(0, max(x)), ylab = "Electric Prices (Cents per Kilowatthour)", xlab = "Year", main = "Plot of Theoretical Model with Data")
abline(ep.summary$b0hat, ep.summary$b1hat, lwd = 2, col = "Blue")
with(ep.df, {
  plot(TOT~x,bg="Blue",pch=21,ylim=c(0,1.1*max(TOT)),xlim=c(min(x)-1,max(x)+1), ylab = "Electric Prices (Cents per Kilowatthour)", xlab = "Year", main = "Plot of Theoretical Model with Data")
  abline(ep.lm)
})

# Error Plot from Linear Model
with(ep.df, {
  plot(TOT~x,bg="Blue",pch=21,ylim=c(0,1.1*max(TOT)),xlim=c(min(x),max(x)), ylab = "Electric Prices (Cents per Kilowatthour)", xlab = "Year")
  segments(x,TOT,x,ep.summary$yhat)
  abline(ep.lm)
  mtext("Diff between Point and Expected Point")
})

# Plot of the Difference between our Theoretical Model and the Mean of the Electric Prices
with(ep.df, {
  plot(TOT~x,bg="Blue",pch=21,ylim=c(0,1.1*max(TOT)),xlim=c(min(x),max(x)), ylab = "Electric Prices (Cents per Kilowatthour)", xlab = "Year")
  abline(ep.lm)
  abline(h=mean(TOT))
  segments(x,mean(TOT),x,ep.summary$yhat,col="Red")
  mtext("Diff between Linear Model and Mean")
})

# Plot of the Difference between each Datum and the Mean of Electric Prices
with(ep.df, {
  plot(TOT~x,bg="Blue",pch=21,ylim=c(0,1.1*max(TOT)),xlim=c(min(x),max(x)), ylab = "Electric Prices (Cents per Kilowatthour)", xlab = "Year")
  abline(h=mean(TOT))
  segments(x,TOT,x,mean(TOT),col="Green")
  mtext("Diff between Point and Mean")
})

# Trendscatter of Electric Prices
library(s20x)
trendscatter(ep.df$TOT~ep.df$YEAR, f = 1, ylim = c(0,11), main = "Energy Prices ~ Years", xlab = "Year", ylab = "Energy Prices")

# Plotted Data with Linear Model
plot(ep.df$TOT~x, ylim = c(0, max(ep.df$TOT) + 1), xlim = c(0, max(x)), ylab = "Electric Prices (Cents per Kilowatthour)", xlab = "Year")
abline(ep.summary$b0hat, ep.summary$b1hat, lwd = 2, col = "Blue")

# Residual vs Fitted Values
plot(ep.lm, which =1, pch = 19)
abline(mean(ep.lm$residuals)+1.5*sd(ep.lm$residuals),0)
abline(mean(ep.lm$residuals)-1.5*sd(ep.lm$residuals),0)

# Shapiro-wilk
library(s20x)
normcheck(ep.lm, shapiro.wilk = TRUE)

# trendscatter on Residual Vs Fitted
trendscatter(ep.lm$residuals~ep.lm$fitted.values, f = 0.5, main = "Residuals ~ Fitted Values", ylab = "Residuals", xlab = "Fitted Values")

# Zero mean value of $\epsilon$
mean(ep.summary$yerr)
t.test(ep.summary$yerr, mu=0, conf.level = 0.95)

# Independence of data
1-.9272^2

# Homoscedasticity
plot(ep.df$TOT~x, pch = 17, col = "Purple", ylab = "Electric Prices (Cents per Kilowatthour)", xlab = "Year")
abline(ep.summary$b0hat, ep.summary$b1hat)
abline(ep.summary$b0hat+2*sd(ep.lm$residuals), ep.summary$b1hat, h = 0, lty = 2, col = "Red")
abline(ep.summary$b0hat-2*sd(ep.lm$residuals), ep.summary$b1hat, h = 0, lty = 2, col = "Red")

# Summary lm object
summary(ep.lm)

# Calculate cis for $\beta$ parameter estimates
ci=c()
t=abs(qt(0.05/2,length(x)-1))
ci[1]=ep.summary$b1hat-t*(0.006801)
ci[2]=ep.summary$b1hat+t*(0.006801)
ci

t=abs(qt(0.05/2,length(x)-1))
ci[1]=ep.summary$b0hat-t*(0.201234)
ci[2]=ep.summary$b0hat+t*(0.201234)
ci

# ci's for Beta1 and Beta0
library(s20x)
ciReg(ep.lm)

# Predictions
mypred = function(y, ep.lm){
  predict(ep.lm)[y - 1960]
}
mypred(1961, ep.lm)
mypred(1991, ep.lm)
mypred(2011, ep.lm)

# Outliers using cooks plots
cooks20x(ep.lm)

# Plot of residuals
plot(y=ep.lm$residuals,x=ep.df$YEAR, col= ifelse(ep.lm$residuals >= 0.9, "Red", ifelse(ep.lm$residuals <= -0.9, "red", "Black")), pch = 10)
abline(0.9,0)
abline(-0.9,0)

outliers = c()
ctr = 1
for(i in 1:length(ep.lm$residuals)){
  if(ep.lm$residuals[i] >= 0.9){
    outliers[ctr] = ep.df$YEAR[i]
    ctr = ctr + 1
  }
  if(ep.lm$residuals[i] <= -0.9){
    outliers[ctr] = ep.df$YEAR[i]
    ctr = ctr + 1
  }
}
outliers