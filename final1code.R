fishdata <- read.csv("C:/Users/torrealb000/Downloads/Untitled spreadsheet - Sheet1.csv", row.names=1)
View(fishdata)
attach(fishdata)
library(ggplot2)
library(tidyverse)
ggplot(fishdata)+
  coord_flip()+
  geom_boxplot(width = .25, aes(y=ChosBodLen), fill='blue')+
  geom_boxplot(width = .25, aes(y=RejBodLen), position= position_nudge(x=-.5), fill='red')+
  theme_light()+
  labs(
    y = "Lenght of Fish"
  )+
  theme(
    axis.text.y = (element_blank())
  )

