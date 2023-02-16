setwd("C:/Users/torrealb000/Desktop/Applied Stats/Snakes")
library(ggplot2)
library(tidyverse)
library(GGally)
snake <- read.csv("C:/Users/torrealb000/Desktop/Applied Stats/Snakes/snake.csv", row.names=1)
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
  )+
  teme_light()

t.test(Lat.Index, alternative = c("two.sided"), mu = 0.5)

sd(Lat.Index)

#NUMBER TWO 

pairs(snake)

ggplot(snake, aes(x=Length, y=Lat.Index, color = Gender))+
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
  scale_color_manual(values = c('#e34685', '#466ee3'), Gender)


d <- ggplot(snake, aes(x=Length, y=Lat.Index, color = Age))+
  geom_point()+
  theme_light()+
  labs(
    title = "Laterality Index as a function of Length with Age",
    x = "Length",
    y = "Laterality Index",
    color = "Age"
  )+
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.background = element_rect(fill = "white")
  )+
  scale_color_manual(values = c('#8f42d6', '#3acf55'), Age)
d


lm(Lat.Index~Length)

ggpairs(snake)
