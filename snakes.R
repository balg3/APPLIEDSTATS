setwd("C:/Users/torrealb000/Desktop/Applied Stats/Snakes")
library(ggplot2)
library(tidyverse)
snake <- read.csv("C:/Users/torrealb000/Desktop/Applied Stats/Snakes/snake - Sheet1.csv", row.names=1)



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
  theme_light()

t.test()
  
  