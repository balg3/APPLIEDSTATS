fishdata <- read.csv("C:/Users/torrealb000/Downloads/Untitled spreadsheet - Sheet1.csv", row.names=1)
View(fishdata)
attach(fishdata)
library(ggplot2)
library(tidyverse)
library(dplyr)


findoutlier <- function(x) {
  return(x < quantile(x, .25) - 1.5*IQR(x) | x > quantile(x, .75) + 1.5*IQR(x))
}

df1 <- data.frame(ChosBodLen, RejBodLen)

  first <- ggplot(df1)+
  coord_flip()+
  scale_fill_manual(values = c("Chosen" = "#42d4f5","Rejected" = "#ed736d"))+
  geom_boxplot(width = .25, aes(y=ChosBodLen, fill = "Chosen"))+
  geom_boxplot(width = .25, aes(y=RejBodLen, fill = "Rejected"), position = position_nudge(x=-.5))+
  theme_light()+
  labs(
    y = "Centimeters",
    title = "Length of Fish",
    fill = "Mating"
  )+
  theme(
    axis.text.y = (element_blank()),
    plot.title = (element_text(hjust = 0.5))
  )

first

