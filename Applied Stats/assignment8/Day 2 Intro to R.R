#Applied Stats

#import excel sheets filled with data (.csv)
# summarizing single and bi-variate date
#t-test and 2 sample t difference of means, or 2 sample z difference of proportions
#boxplots, histograms, probability plots


#Lets start. Always start by setting your working directory (WD)

setwd("H:/Applied Statistics/2022/Ch 1 Linear Regression/Introduction to R")


#Go to Google Classroom and download the excel (csv) file and save it to your working directory.

#Import your data called 'range'
range <- read.csv("H:/Applied Statistics/2020/Unit 2/R Introduction/Assignment #13/range.csv")
View(range)

#The command head(range) shows the first six lines of data and titles to the columns

head(range)

range

#Find the summary statistics on the Pre column

summary(range$Pre)

#You can also use the attach command

attach(range)

summary(Pre)

#Lets compile

#mean, sd, var, min, max, median, range

mean(Pre)

sd(Pre)

min(Pre)

#Plot bi-variate data

plot(Pre,Post)

plot(Pre,Post,main="Range of Motion (Pre and Post Treatment",sub="Year 2015",xlab="Range of Motion (Pre)",ylab="Range of Motion (Post)")

#Pick ponits pch=, then add color..For symbols 21 through 25, specify border color (col=) and fill color (bg=)

plot(Pre,Post,main="Range of Motion (Pre and Post Treatment",
     sub="Year 2015",xlab="Range of Motion (Pre)",ylab="Range of Motion (Post)", pch=22, col="red",bg="grey")

#Correlation?

cor(Pre,Post)

#Regression model

lm(Pre~Post)

regression<-lm(Pre~Post)

plot(regression)

summary(regression)


#Plots

boxplot(Pre)
boxplot(Pre, horizontal = TRUE)
boxplot(Pre,Post)
boxplot(Pre,Post, horizontal = TRUE)
boxplot(Pre,Post,xlab="Pre        Range of Motion         Post")



#Histograms

hist(Pre)
hist(Pre, breaks=10)


#t.tests and 2 sample t.tests

t.test(Pre,mu=43)
t.test(Pre,mu=43,alternative = c("greater"))
t.test(Pre,Post)

