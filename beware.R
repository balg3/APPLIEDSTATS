setwd("C:/Users/torrealb000/Desktop/Applied Stats")
library(readxl)
library(dplyr)
library(ggplot2)
library(GGally)
titanic <- read_excel("titanic.xls")


attach(titanic)
faremean<-tapply(Fare, Pclass, mean)
faremean
sd <-tapply(Fare, Pclass, sd)
sd

ggplot(titanic, aes(factor(Survived), fill=factor(Sex)))+
  geom_bar(position = "fill")+
  labs(
    y = "Percent %",
    x = "Survived (545 vs. 342)",
    fill = "Gender"
  )+
  scale_fill_manual(values = c("#EC46F5", "#46F5C3"))

ggplot(titanic, aes(factor(Survived), Age))+
  coord_flip()+
  geom_boxplot()+
  labs(
    y = "Age",
    x = "Survived"
  )
