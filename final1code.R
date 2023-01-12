fishdata <- read.csv("C:/Users/torrealb000/Downloads/Untitled spreadsheet - Sheet1.csv", row.names=1)
View(fishdata)
attach(fishdata)
library(ggplot2)
library(tidyverse)
library(dplyr)



df1 <- data.frame(ChosBodLen, RejBodLen)

first1 <- ggplot(df1)+
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
    plot.title = (element_text(hjust = 0.5)),
    legend.position = c(0.9, 0.5)
  )

first1

lapply(fishdata, mean)
lapply(fishdata, sd)

second1 <- ggplot(fishdata)+
  coord_flip()+
  scale_fill_manual(values = c("Chosen" = "#42d4f5","Rejected" = "#ed736d"))+
  geom_boxplot(width = .25, aes(y=ChosAmplitude, fill = "Chosen"))+
  geom_boxplot(width = .25, aes(y=RejAmplitude, fill = "Rejected"), position = position_nudge(x=-.5))+
  theme_light()+
  labs(
    y = "miliVolts",
    title = "Amplitude of Fish",
    fill = "Mating"
  )+
  theme(
    axis.text.y = (element_blank()),
    plot.title = (element_text(hjust = 0.5)),
    legend.position = c(0.9, 0.5)
  )

second1 



third1 <- ggplot(fishdata)+
  coord_flip()+
  scale_fill_manual(values = c("Chosen" = "#42d4f5","Rejected" = "#ed736d"))+
  geom_boxplot(width = .25, aes(y=ChosDuration, fill = "Chosen"), outlier.size = 2)+
  geom_boxplot(width = .25, aes(y=RejDuration, fill = "Rejected"), position = position_nudge(x=-.5), outlier.size = 2)+
  theme_light()+
  labs(
    y = "miliseconds",
    title = "Duration of Fish",
    fill = "Mating"
  )+
  theme(
    axis.text.y = (element_blank()),
    plot.title = (element_text(hjust = 0.5)),
    legend.position = c(0.9, 0.5)
  )

third1
