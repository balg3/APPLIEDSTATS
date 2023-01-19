fishdata <- read.csv("C:/Users/upist/Desktop/Applied Stats/final1/Untitled spreadsheet - Sheet1.csv", row.names=1)
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
    legend.position = c(0.9, 0.5),
    axis.text.x = element_text(size = 15),
    axis.title = element_text(size = 20),
    title = element_text(size = 20),
    legend.title = element_text(size = 15)
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
    title = "EOD Amplitude of Fish",
    fill = "Mating"
  )+
  theme(
    axis.text.y = (element_blank()),
    legend.position = c(0.9, 0.5),
    axis.text.x = element_text(size = 15),
    axis.title = element_text(size = 20),
    title = element_text(size = 20),
    legend.title = element_text(size = 15)
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
    title = "EOD Duration of Fish",
    fill = "Mating"
  )+
  theme(
    axis.text.y = (element_blank()),
    legend.position = c(0.9, 0.5),
    axis.text.x = element_text(size = 15),
    axis.title = element_text(size = 20),
    title = element_text(size = 20),
    legend.title = element_text(size = 15)
  )

third1

#problem 2
 
t.test(ChosBodLen, RejBodLen)
t.test(ChosDuration, RejDuration)
t.test(ChosAmplitude, RejAmplitude)

#Comparisons >:) Problem 3

#LENGTH VS DURATION
reglvdC <- lm(ChosDuration ~ ChosBodLen)
summary(reglvdC)
reglvdR <- lm(RejDuration ~ RejBodLen)
summary(reglvdR)

lenvdur <- ggplot(fishdata)+
  geom_point(size = 2.5, aes(ChosBodLen, ChosDuration, color = "1"))+
  geom_point(size = 2.5, aes(RejBodLen, RejDuration, color = "0"))+
  theme_light()+
  labs(
    title = "Body Length vs. EOD Duration",
    x = "Body Length",
    y = "EOD Duration",
    color = "Mating Status"
  )+
  scale_x_continuous(limits = c(14, 30)) +
  scale_y_continuous(limits = c(1, 2.5)) +
  theme(
    legend.position = c(0.92, 0.1),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    axis.title = element_text(size = 20),
    title = element_text(size = 20),
    legend.title = element_text(size = 15)
  )+
  scale_color_manual(
    values = c("1" = "#42d4f5","0" = "#ed736d"),
    labels = c("Chosen", "Rejected")
    )+
  geom_abline(aes(slope = 0.02421, intercept = 1.11736, color = "1"), size = 1)+
  geom_abline(aes(slope = 0.03319, intercept = 0.87955, color = "0"), size = 1)

lenvdur

#LENGTH VS AMPLITUDE
reglvaC <- lm(ChosAmplitude ~ ChosBodLen)
summary(reglvaC)
reglvaR <- lm(RejAmplitude ~ RejBodLen)
summary(reglvaR)


lenvamp <- ggplot(fishdata)+
  geom_point(size = 2.5, aes(ChosBodLen, ChosAmplitude, color = "1"))+
  geom_point(size = 2.5, aes(RejBodLen, RejAmplitude, color = "0"))+
  theme_light()+
  labs(
    title = "Body Length vs. EOD Amplitude",
    x = "Body Length",
    y = "EOD Amplitude",
    color = "Mating Status"
  )+
  scale_x_continuous(limits = c(14, 30)) +
  scale_y_continuous(limits = c(1, 10)) +
  theme(
    legend.position = c(0.92, 0.1),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    axis.title = element_text(size = 20),
    title = element_text(size = 20),
    legend.title = element_text(size = 15)
  )+
  scale_color_manual(
    values = c("1" = "#42d4f5","0" = "#ed736d"),
    labels = c("Chosen", "Rejected")
  )+
  geom_abline(aes(slope = 0.30844, intercept = -1.27921, color = "1"), size = 1)+
  geom_abline(aes(slope = 0.43495, intercept = -3.70646, color = "0"), size = 1)

lenvamp

#DURATION VS AMPS
regdvaC <- lm(ChosAmplitude ~ ChosDuration)
summary(regdvaC)
regdvaR <- lm(RejAmplitude ~ RejDuration)
summary(regdvaR)

durvamp <- ggplot(fishdata)+
  geom_point(size = 2.5, aes(ChosDuration, ChosAmplitude, color = "1"))+
  geom_point(size = 2.5, aes(RejDuration, RejAmplitude, color = "0"))+
  theme_light()+
  labs(
    title = "EOD Duration vs. EOD Amplitude",
    x = "EOD Duration",
    y = "EOD Amplitude",
    color = "Mating Status"
  )+
  scale_x_continuous(limits = c(1, 2.5)) +
  scale_y_continuous(limits = c(1, 10)) +
  theme(
    legend.position = c(0.92, 0.1),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    axis.title = element_text(size = 20),
    title = element_text(size = 20),
    legend.title = element_text(size = 15)
  )+
  scale_color_manual(
    values = c("1" = "#42d4f5","0" = "#ed736d"),
    labels = c("Chosen", "Rejected")
  )+
  geom_abline(aes(slope = 0.8653, intercept = 4.1968, color = "1"), size = 1)+
  geom_abline(aes(slope = 3.8100, intercept = -0.8657, color = "0"), size = 1)

durvamp

###-----###

lenvdur

reglvdC <- lm(ChosDuration ~ ChosBodLen)
summary(reglvdC)
reglvdR <- lm(RejDuration ~ RejBodLen)
summary(reglvdR)

lenvamp

reglvaC <- lm(ChosAmplitude ~ ChosBodLen)
summary(reglvaC)
reglvaR <- lm(RejAmplitude ~ RejBodLen)
summary(reglvaR)
durvamp

regdvaC <- lm(ChosAmplitude ~ ChosDuration)
summary(regdvaC)
regdvaR <- lm(RejAmplitude ~ RejDuration)
summary(regdvaR)
