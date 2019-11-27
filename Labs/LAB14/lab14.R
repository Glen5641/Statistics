#Task1
getwd()

#Task2
mylsq=function(x,y){
  ssxx=sum((x-mean(x))^2 )
  ssxy=sum((x-mean(x))*(y-mean(y)))
  b1hat=ssxy/ssxx
  b0hat=mean(y)-b1hat*mean(x)
    return(list(b0hat=b0hat,b1hat=b1hat))
}

x=1:20
set.seed(29);y=4+6*x + rnorm(20,0,5)
line = mylsq(x=x,y=y)

plot(y~x, pch = 16)
abline(line$b0hat, line$b1hat, lwd = 2, col = "Blue")

line$b0hat
line$b1hat
slr=lm(y~x); summary(slr)

#Task3
mypred = function(x, b0hat, b1hat){
  y=b0hat+b1hat*x
  y
}
mypred(15.5, line$b0hat, line$b1hat)
points(x=15.5,y=mypred(15.5, line$b0hat, line$b1hat), cex=3 ,col="Green",pch=19)


oj = read.csv("OJUICE.csv")
line = mylsq(x = oj$Pectin, y = oj$SweetIndex)
plot(oj$SweetIndex~oj$Pectin, pch = 16, ylab = "Sweetness Index", xlab = "Pectin", main = "OJ")
abline(line$b0hat, line$b1hat, lwd = 2, col = "Blue")
line$b0hat
line$b1hat
mypred(300, line$b0hat, line$b1hat)


rock = read.csv("DRILLROCK.csv")
line = mylsq(x = rock$DEPTH, y = rock$TIME)
plot(rock$TIME~rock$DEPTH, pch = 16, ylab = "Time", xlab = "Depth", main = "Drilling rock")
abline(line$b0hat, line$b1hat, lwd = 2, col = "Blue")
line$b0hat
line$b1hat


#Task4
mysq=function(x,y){
  n=length(x)
  ssxx=sum((x-mean(x))^2 )
  ssxy=sum((x-mean(x))*(y-mean(y)))
  b1hat=ssxy/ssxx
  b0hat=mean(y)-b1hat*mean(x)
  yhat=b0hat+b1hat*x
  ssr=sum((y-yhat)^2)
  sq= ssr/(n-2)
  return(list(b1hat = b1hat, b0hat = b0hat, ssr=ssr,sq=sq, yhat=yhat))
}
x=1:20
set.seed(29);y=4+6*x + rnorm(20,0,5)
mysq(x=x,y=y)
var(slr$residuals)

carb = read.csv("CARBON.csv")
line = mysq(carb$LabFurnace, carb$PilotPlant)

plot(carb$PilotPlant~carb$LabFurnace, ylab = "Pilot Test", xlab = "Lab Furnace", main = "CARBON")
abline(line$b0hat, line$b1hat, lwd = 2, col = "Blue")
line$ssr
line$sq
sqrt(line$sq)
