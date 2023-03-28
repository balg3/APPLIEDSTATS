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
######################################
setwd("C:/Users/upist/Desktop/Applied Stats/otherstuff")
library(readxl)
library(ggplot2)
FirstYearGPA <- read_excel("FirstYearGPA.xls", 
                           col_types = c("numeric", "numeric", "numeric", 
                                         "numeric", "text", "text", "text"))
FirstYearGPA2 <- read_excel("FirstYearGPA.xls", 
                            col_types = c("numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", "numeric"))
attach(FirstYearGPA)
attach(FirstYearGPA2)

#a

baller <-t.test(GPA~FirstGen)
baller

jager <-lm(GPA~factor(FirstGen))
summary(jager)
summary(lm(GPA~factor(FirstGen)))

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

ggplot(jager, aes(x = .fitted, y = .resid)) +
  geom_point()+
  theme_bw()+
  labs(
    title = "Residual Plot of FirstGen to Predict GPA",
    y = "Residual",
    x = "Predicted GPA"
  )

summary(glm(FirstGen ~ GPA, family = binomial(link = "logit")))

####################################################################
one <- lm(GPA ~ HSGPA + factor(FirstGen))
summary(one)
two <- lm(GPA ~ HSGPA + factor(FirstGen) + SATV)
summary(two)
three <- lm(GPA ~ HSGPA + factor(FirstGen) + SATM)
summary(three)
four <- lm(GPA ~ HSGPA + factor(FirstGen) + SATM +SATV)
summary(four)
five <- lm(GPA ~ HSGPA + factor(FirstGen) + SATV + White + Male)
summary(five)
six <- lm(GPA ~ HSGPA + factor(FirstGen) + SATV + White)
summary(six)
