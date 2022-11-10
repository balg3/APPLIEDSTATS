
data("USArrests")
attach(USArrests)
head(USArrests)

plot(Assault, Murder, main = "Murder as a Function of Assault", xlab = "Assaults per 100k", ylab = "Murders per 100k", pch =22, col="black", bg="pink")

lm(Murder~Assault)

regres1<-lm(Murder~Assault)
summary(regres1)
anova(regres1)
abline(regres1)

plot(Assault,resid(regres1), main = "Residual Plot of Assault vs Residuals", xlab = "Assaults per 100k", ylab = "Residuals", pch =2, col="black")
abline(0,0) 
sd(resid(regres1))

help("confint.lm")
