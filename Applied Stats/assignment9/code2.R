setwd("C:/Users/torrealb000/Desktop/Applied Stats/assignment9")
attach(Brushing.Method)


View(Brushing.Method)

tapply(percent, method, summary)
boxplot(percent~method, horizontal=TRUE, main="Percent Difference in Weight vs Method Used", xlab = "Percent difference of weight before & after cleaning", ylab = "Method used", col = "skyblue1", bg = "snow")
tapply(percent, method, sd)
plot(dw, cw, main = "Dirty Weight as a function of Clean Weight (in grams)", xlab = "Dirty Weight", ylab = "Clean Weight",pch = 22, col = "black", bg = "skyblue1")
abline(lm(cw~dw))
summary(lm(cw~dw))

lm(formula = cw ~ dw)
regres1<-lm(cw~dw)
plot(regres1)  
plot(dw, resid(regres1), main = "Residuals vs Dirty Weight", xlab = "Dirty Weight", ylab = "Residuals")
abline(0,0)

attach(brushing)
qqnorm(perc, main = "Normal Q-Q Plot of the Brushing Method")
attach(calgon)
qqnorm(percentage, main = "Normal Q-Q Plot of the Calgon Method")
