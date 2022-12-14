#This is a demo to just show you some useful tool.

#You must ALWAYS set your working directory FIRST!!!!
#Look under the 'Session' tab.

setwd("H:/Applied Statistics/2020/Unit 2/R Introduction/Assignment #13")
data("mtcars")
head(mtcars)

#If you are managing multiple data sets:
plot(mtcars$wt,mtcars$mpg)


#A short cut, if you are dealing entirely with one data set:
attach(mtcars)

#Basic Scatter plot:
plot(wt,mpg)

#Add a title:
plot(wt,mpg,main="MPG as a Function of Weight")

#Add Labels for your x-and y-axis:
plot(wt,mpg,main="MGP as a Function of Weight",xlab="Weight in lbs",ylab="miles per gallon")

#Add Symbols:
plot(wt,mpg,main="MGP as a Function of Weight",xlab="Weight in lbs",ylab="miles per gallon",pch=22)

#Add colors to your symbols:
plot(wt,mpg,main="MGP as a Function of Weight",xlab="Weight in lbs",ylab="miles per gallon",
     pch=22,col="red",bg="grey")

#Add colors to your labels:
plot(wt,mpg,main="MGP as a Function of Weight",xlab="Weight in lbs",ylab="miles per gallon",
     pch=22,col="red",bg="grey",col.main="blue",col.lab="chocolate1")

#Calculate a linear model:
lm(mpg~wt)


#You need to store your linear model as a variable/object:
regres1<-lm(mpg~wt)

#To look at a summary of your model:
summary(regres1)
anova(regres1)

#Plot your line:
abline(regres1)

#Or, look at or plot your fitted values for your given data:
fitted(regres1)
head(mtcars)

predict(regres1)

lines(wt,fitted(regres1))
segments(wt,fitted(regres1),wt,mpg)


#If you want the residuals:
resid(regres1)
plot(wt,resid(regres1))
abline(0,0)
plot(wt,resid(regres1), main="Residual Plot: Weight versus Residuals")
abline(0,0)


#Short cut:
plot(regres1)
#hit 'enter' four times


qqnorm(resid(regres1))
qqline(resid(regres1))
shapiro.test(resid(regres1))


#Make sure to detach data
detach(mtcars)