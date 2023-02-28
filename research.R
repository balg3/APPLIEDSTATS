setwd("C:/Users/torrealb000/Desktop/Applied Stats/number5")
Exercise <- read.csv("C:/Users/torrealb000/Desktop/Applied Stats/number5/Researcher Exercise_Data Set, general.csv")
attach(Exercise)
library(ggplot2)

#NUMBA1

tapply(overweight, sex, mean)
tapply(overweight, sex, sd)
tapply(smoke, sex, mean)
tapply(smoke, sex, sd)

#NUMBA2 

fig1<-ggplot(Exercise, aes(smoke, overweight))+
  geom_point()+
  labs(
    x = "Smoking Prevalence",
    y = "Overweightness Prevalence",
    title = "Smoking Against Being Overweight"
  )+
  theme(
    plot.title = element_text(face = "bold")
  )

fig1

#NUMBA3

lm1 <- lm(overweight~smoke)
summary(lm1)

#NUMBA4
