setwd("C:/Users/torrealb000/Desktop/Applied Stats/day2log")
library(readxl)
library(ggplot2)
FirstYearGPA <- read_excel("FirstYearGPA.xlsx", 
                           col_types = c("numeric", "numeric", "numeric", 
                                         "numeric", "text", "text", "text"))
attach(FirstYearGPA)

#a

GPAfirstgen <-t.test(GPA~FirstGen)
GPAfirstgen

tapply(GPA,FirstGen, FUN = sd)
tapply(GPA,FirstGen, FUN = length)

ggplot(FirstYearGPA, aes(x = factor(FirstGen), y = GPA)) +
  geom_boxplot()+
  theme_bw()+
  labs(
    title = "GPA Distribution by First Gen",
    y = "GPA",
    x = "First Gen"
  )
