setwd("C:/Users/upist/Desktop/Applied Stats/snkaesk")
library(ggplot2)
library(tidyverse)
library(GGally)
library(dplyr)
snake <- read.csv("C:/Users/upist/Desktop/Applied Stats/snkaesk/snakes.csv", row.names=1)
attach(snake)

#NUMBER ONE

ggplot(snake, aes(x=Lat.Index))+
  geom_density(color="blue", size=0.75)+
  geom_vline(
    xintercept=0.5, size=0.75
  )+
  geom_vline(
    aes(xintercept=mean(Lat.Index)),
    color = "blue", linetype="dashed", size=0.75
  )+
  labs(
    x = "Laterality Index",
    y = "Count",
    title = "Laterality Index Density Plot"
  )+
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.background = element_rect(fill = "white")
  )

t.test(Lat.Index, alternative = c("two.sided"), mu = 0.5)

sd(Lat.Index)

#NUMBER TWO 

w <- ggplot(snake, aes(x=Length, y=Lat.Index, color = Gender))+
  geom_point()+
  theme_light()+
  labs(
    title = "Laterality Index as a function of Length with Gender",
    x = "Length",
    y = "Laterality Index"
  )+
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.background = element_rect(fill = "white")
  )+
  scale_color_manual(values = c('#e34685', '#466ee3'), Gender)+
  guides(color=guide_legend("Gender"))
w

d <- ggplot(snake, aes(x=Length, y=Lat.Index, color = Age))+
  geom_point()+
  theme_light()+
  labs(
    title = "Laterality Index as a function of Length with Age",
    x = "Length",
    y = "Laterality Index"
  )+
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.background = element_rect(fill = "white")
  )+
  scale_color_manual(values = c('#8f42d6', '#3acf55'), Age)+
  guides(color=guide_legend("Age"))
d

len <- lm(Lat.Index~Length)
summary(len)

age <- lm(Lat.Index~Age)
summary(age)

gen <- lm(Lat.Index~Gender)
summary(gen)

lenage <- lm(Lat.Index~Length+Age)
summary(lenage)

lengen <- lm(Lat.Index~Length+Gender)
summary(lengen)

lenagegen <- lm(Lat.Index~Length+Age+Gender)
summary(lenagegen)

agegen <- lm(Lat.Index~Age+Gender)
summary(agegen)

bruh <- lm(Lat.Index~Length+Age+Length:Gender)
summary(bruh)

ggpairs(snake)

#NUMBER THREE

t.test(Length ~ Age, alternative = "two.sided")
t.test(Length ~ Gender, alternative = "two.sided")

aggregate(Lat.Index, list(Age), FUN=sd) 
aggregate(Lat.Index, list(Gender), FUN=sd) 
aggregate(Lat.Index, list(Age), FUN=length) 
aggregate(Lat.Index, list(Gender), FUN=length) 


qq1 <- ggplot(snake, aes(sample = Length, color = factor(Age)))+
  stat_qq()+
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.background = element_rect(fill = "white")
  )+
  scale_color_manual(values = c('#8f42d6', '#3acf55'), Age)+
  labs(
    x = "Theoretical",
    y = "Sample",
    title = "Q-Q Probability Plot for Age"
  )+
  guides(color=guide_legend("Age"))
qq1

qq2 <- ggplot(snake, aes(sample = Length, color = factor(Gender)))+
  stat_qq()+
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.background = element_rect(fill = "white")
  )+
  scale_color_manual(values = c('#e34685', '#466ee3'), Gender)+
  labs(
    x = "Theoretical",
    y = "Sample",
    title = "Q-Q Probability Plot for Gender"
  )+
  guides(color=guide_legend("Gender"))
qq2
