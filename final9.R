setwd("C:/Users/upist/Desktop/Applied Stats/FINAL2")
library(haven)
GUKR71FL <- read_dta("GUKR71FL.DTA")
library(ggplot2)
attach(GUKR71FL)

hemowealeth <- ggplot(GUKR71FL, aes(v191, hw56, color = factor(v131)))+
  geom_point(alpha=0.4)+
  labs(
    y = "Hemoglobin in Decigram/dl",
    x = "Wealth Index",
    title = "Hemoglobin as a function of Wealth Index",
    color = "Ethnicity"
  )+
  scale_color_manual(values = c("1"="#34ebe2",
                                "2"="#eb34c3",
                                "3"="#5634eb",
                                "4"="#eb5334",
                                "6"="#40eb34",
                                "8"="#2b30ad"))+
  theme_bw()+
  geom_abline(intercept = 1.147e+02, slope = 1.267e-05, color="#34ebe2")+
  geom_abline(intercept = 1.144e+02, slope = 2.627e-05, color="#eb34c3")+
  geom_abline(intercept = 1.073e+02, slope = 4.824e-05, color="#5634eb")+
  geom_abline(intercept = 1.129e+02, slope = -1.500e-06, color="#eb5334")
hemowealeth

GUKR71FL$Maya <- NA
GUKR71FL$Maya <- ifelse(GUKR71FL$v131 == 1, GUKR71FL$hw56, NA)
GUKR71FL$Mestizo <- NA
GUKR71FL$Mestizo <- ifelse(GUKR71FL$v131 == 2, GUKR71FL$hw56, NA)
GUKR71FL$Garifuna <- NA
GUKR71FL$Garifuna <- ifelse(GUKR71FL$v131 == 3, GUKR71FL$hw56, NA)
GUKR71FL$Xinca <- NA
GUKR71FL$Xinca <- ifelse(GUKR71FL$v131 == 4, GUKR71FL$hw56, NA)
GUKR71FL$extra <- NA
GUKR71FL$extra <- ifelse(GUKR71FL$v131 == 6, GUKR71FL$hw56, NA)
GUKR71FL$extra2 <- NA
GUKR71FL$extra2 <- ifelse(GUKR71FL$v131 == 8, GUKR71FL$hw56, NA)

summary(lm(Maya~v191))
summary(lm(Mestizo~v191))
summary(lm(Garifuna~v191))
summary(lm(Xinca~v191))

#graphs

hemowealbox <- ggplot(GUKR71FL, aes(factor(v131), hw56, fill = factor(v190)))+
  geom_boxplot() +
  facet_wrap(~factor(v131), scale="free") +
  labs(
    x = "Ethnicity",
    y = "Hemoglobin in Decigram/dl",
    fill = "Wealth Quintile"
  )+
  theme_bw()
hemowealbox                        


hemowealwat <- ggplot(GUKR71FL, aes(factor(v113), hw56, fill = factor(v190)))+
  geom_boxplot() +
  facet_wrap(~factor(v113), scale="free") +
  labs(
    x = "Water Source",
    y = "Hemoglobin in Decigram/dl",
    fill = "Wealth Quintile"
  )+
  theme_bw()
hemowealwat                        

hemowealmeat <- ggplot(GUKR71FL, aes(factor(v414h), hw56, fill = factor(v190)))+
  geom_boxplot() +
  facet_wrap(~factor(v414h), scale="free") +
  labs(
    x = "Ate Meat",
    y = "Hemoglobin in Decigram/dl",
    fill = "Wealth Quintile"
  )+
  theme_bw()
hemowealmeat                      


hemowealtoil <- ggplot(GUKR71FL, aes(factor(v205), hw56, fill = factor(v190)))+
  geom_boxplot() +
  facet_wrap(~factor(v205), scale="free") +
  labs(
    x = "Improved Toilet",
    y = "Hemoglobin in Decigram/dl",
    fill = "Wealth Quintile"
  )+
  theme_bw()
hemowealtoil

hemowealdiar <- ggplot(GUKR71FL, aes(factor(h11), hw56, fill = factor(v190)))+
  geom_boxplot() +
  facet_wrap(~factor(h11), scale="free") +
  labs(
    x = "Diarrhea",
    y = "Hemoglobin in Decigram/dl",
    fill = "Wealth Quintile"
  )+
  theme_bw()
hemowealdiar

hemowealfev <- ggplot(GUKR71FL, aes(factor(h22), hw56, fill = factor(v190)))+
  geom_boxplot() +
  facet_wrap(~factor(h22), scale="free") +
  labs(
    x = "Fever",
    y = "Hemoglobin in Decigram/dl",
    fill = "Wealth Quintile"
  )+
  theme_bw()
hemowealfev

hemowealfloor <- ggplot(GUKR71FL, aes(factor(v127), hw56, fill = factor(v190)))+
  geom_boxplot() +
  facet_wrap(~factor(v127), scale="free") +
  labs(
    x = "Flooring Material",
    y = "Hemoglobin in Decigram/dl",
    fill = "Wealth Quintile"
  )+
  theme_bw()+
  coord_flip()
hemowealfloor

#ANOVA/TukeyHSD

GUKR71FL$v131x <- NA
GUKR71FL$v131x <- ifelse(GUKR71FL$v131 == 1, 1, NA)
GUKR71FL$v131x <- ifelse(GUKR71FL$v131 == 2, 0, GUKR71FL$v131x)
factor(GUKR71FL$v131)

attach(GUKR71FL)

summary(aov(hw56 ~ v190 + v131x)) 
summary(aov(hw56 ~ v113 + v131x))
summary(aov(hw56 ~ v414h + v131x))
summary(aov(hw56 ~ v414h + v131x))
TukeyHSD()



#MULTIPLE REGRESSION HEMOGLOBIN PREDICTION

lm1<-lm(hw56~)

#LOGISTIC REGRESSION ETHNICITY PREDICTION

glm<-lm(v131x~)
