#Task 1
##Get Working Directory
getwd()


#Task 2
##Read Spruce Data File and Show Tail of Data
spruce.df <- read.csv("SPRUCE.csv", header=TRUE) 
tail(spruce.df)


#Task 3
##load s20x library and trendscatter plot with height v bhd of spruce data file
library(s20x)
trendscatter(Height ~ BHDiameter, f = 0.5, data = spruce.df)
##Now make the linear model, Residuals, and Fitted Values
spruce.lm <- lm(Height ~ BHDiameter, data = spruce.df)
height.res <- residuals(spruce.lm)
height.fit <- fitted(spruce.lm)
#plot the residuals v fitted values
plot(height.res ~ height.fit, ylab = "Residuals", xlab = "Fitted Values")
##load s20x library and trendscatter plot with height v bhd of spruce data file
library(s20x)
trendscatter(height.res ~ height.fit)
#Plot The Residuals v Fitted Values with quadratic model and zero template
plot(spruce.lm, which =1)
#Check the normality of linear model along with Shapiro
library(s20x)
normcheck(spruce.lm,shapiro.wilk = TRUE)


#Task 4
#Create a quadratic function with coefficients of quad.lm
quad.lm=lm(Height~BHDiameter + I(BHDiameter^2),data=spruce.df)
myplot=function(x){
  quad.lm$coef[1] +quad.lm$coef[2]*x  + quad.lm$coef[3]*x^2
}
#Add quadratic line to a regular height v bhd of Spruce.df
plot(Height~BHDiameter, data = spruce.df)
curve(myplot, lwd=2, col="steelblue",add=TRUE)
#Store Residual and fitted values
quad.fit <- fitted(quad.lm)
quad.res <- residuals(quad.lm)
#Plot the Residuals and Fitted Values of the quadratic model
plot(quad.res ~ quad.fit)
#Check the normality and Shapiro
normcheck(quad.lm,shapiro.wilk = TRUE)


#Task 5
#Summarize Quadratic Model
summary(quad.lm)
#Show the C, X, and X^2 Coefficients
quad.lm$coef[1]
quad.lm$coef[2]
quad.lm$coef[3]
#Predict the height at 15,18, and 20 with the linear model
predict(spruce.lm, data.frame(BHDiameter=c(15,18,20)))
#Predict the height at 15,18, and 20 with the Quadratic model
predict(quad.lm, data.frame(BHDiameter=c(15,18,20)))
#Predict the height at all whole values with the linear model
with(spruce.df,predict(spruce.lm,data.frame(BHDiameter)))
##Compare Quad.lm and Spruce.lm with Anova
anova(spruce.lm,quad.lm)
anova(quad.lm)
anova(spruce.lm)
##ResSS, ModSS, TotSS, and ModSS/TotSS Calculations
RSS <- with(spruce.df, sum((Height-quad.fit)^2))
MSS <- with(spruce.df, sum((quad.fit-mean(Height))^2))
TSS <- with(spruce.df, sum((Height-mean(Height))^2))
RSS
MSS
TSS
MSS/TSS
mean(spruce.df$Height)


#Task 6
#Load s20x and make a cooks distance plot of the quadratic model
library(s20x)
cooks20x(quad.lm)
#Create a quadratic model excluding datum 24
quad2.lm=lm(Height~BHDiameter + I(BHDiameter^2) , data=spruce.df[-24,])
#Summarize it
summary(quad2.lm)