setwd("C:/Users/upist/Desktop/Applied Stats/FINAL2")
library(haven)
GUKR71FL <- read_dta("GUKR71FL.DTA")
library(ggplot2)
attach(GUKR71FL)
library(rdhs)




#Graphical Stuff
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
  geom_abline(intercept = 1.129e+02, slope = -1.500e-06, color="#eb5334")+
  xlim(-250000)
hemowealeth

GUKR71FL$Maya <- NA
GUKR71FL$Maya <- ifelse(GUKR71FL$v131 == 1, GUKR71FL$hw56, NA)
GUKR71FL$Mestizo <- NA
GUKR71FL$Mestizo <- ifelse(GUKR71FL$v131 == 2, GUKR71FLz$hw56, NA)
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

hemowealbox <- ggplot(GUKR71FL, aes(factor(v131x), hw56, fill = factor(v190)))+
  geom_violin() +
  facet_wrap(~factor(v131x), scale="free") +
  labs(
    x = "Ethnicity",
    y = "Hemoglobin in Decigram/dl",
    fill = "Wealth Quintile"
  )+
  theme_bw()+
  ylim(25,225)+
  scale_fill_manual(values = c("#F54671", "#F546C8", "#CA46F5", "#7346F5", "#46C8F5"))
hemowealbox                        






###############################################



hemowealwat <- ggplot(GUKR71FL, aes(factor(v113x), hw56, fill = factor(v190)))+
  geom_violin() +
  facet_wrap(~factor(v113x), scale="free") +
  labs(
    x = "Water Source",
    y = "Hemoglobin in Decigram/dl",
    fill = "Wealth Quintile"
  )+
  theme_bw()+
  ylim(25,225)+
  scale_fill_manual(values = c("#F54671", "#F546C8", "#CA46F5", "#7346F5", "#46C8F5"))
hemowealwat                        

hemoethwat <- ggplot(GUKR71FL, aes(factor(v113x), hw56, fill = factor(v131x)))+
  geom_violin() +
  facet_wrap(~factor(v113x), scale="free") +
  labs(
    x = "Water Source",
    y = "Hemoglobin in Decigram/dl",
    fill = "Ethnicity"
  )+
  theme_bw()+
  ylim(25,225)+
  scale_fill_manual(values = c("#F546C8", "#7346F5"))
hemoethwat                        



#######################################3

hemowealmeat <- ggplot(GUKR71FL, aes(factor(v414hx), hw56, fill = factor(v190)))+
  geom_violin() +
  facet_wrap(~factor(v414hx), scale="free") +
  labs(
    x = "Ate Meat",
    y = "Hemoglobin in Decigram/dl",
    fill = "Wealth Quintile"
  )+
  theme_bw()+
  ylim(25,225)+
  scale_fill_manual(values = c("#F54671", "#F546C8", "#CA46F5", "#7346F5", "#46C8F5"))
hemowealmeat                      

hemoethmeat <- ggplot(GUKR71FL, aes(factor(v414hx), hw56, fill = factor(v131x)))+
  geom_violin() +
  facet_wrap(~factor(v414hx), scale="free") +
  labs(
    x = "Ate Meat",
    y = "Hemoglobin in Decigram/dl",
    fill = "Ethnicity"
  )+
  theme_bw()+
  ylim(25,225)+
  scale_fill_manual(values = c("#F546C8", "#7346F5"))
hemoethmeat                      

########################################


factor(v116)
hemowealtoil <- ggplot(GUKR71FL, aes(factor(v116), hw56, fill = factor(v190)))+
  geom_violin() +
  facet_wrap(~factor(v116), scale="free") +
  labs(
    x = "Improved Toilet",
    y = "Hemoglobin in Decigram/dl",
    fill = "Wealth Quintile"
  )+
  theme_bw()+
  ylim(25,225)+
  scale_fill_manual(values = c("#F54671", "#F546C8", "#CA46F5", "#7346F5", "#46C8F5"))
hemowealtoil

hemoethtoil <- ggplot(GUKR71FL, aes(factor(v116), hw56, fill = factor(v131x), na.rm = TRUE))+
  geom_violin() +
  facet_wrap(~factor(v116), scale="free") +
  labs(
    x = "Improved Toilet",
    y = "Hemoglobin in Decigram/dl",
    fill = "Ethnicity"
  )+
  theme_bw()+
  ylim(25,225)+
  scale_fill_manual(values = c("#F546C8", "#7346F5"))
hemoethtoil
###################################33333

hemowealdiar <- ggplot(GUKR71FL, aes(factor(h11x), hw56, fill = factor(v190)))+
  geom_violin() +
  facet_wrap(~factor(h11x), scale="free") +
  labs(
    x = "Diarrhea",
    y = "Hemoglobin in Decigram/dl",
    fill = "Wealth Quintile"
  )+
  theme_bw()+
  ylim(25,225)+
  scale_fill_manual(values = c("#F54671", "#F546C8", "#CA46F5", "#7346F5", "#46C8F5"))

hemowealdiar

hemoethdiar <- ggplot(GUKR71FL, aes(factor(h11x), hw56, fill = factor(v131x)))+
  geom_violin() +
  facet_wrap(~factor(h11x), scale="free") +
  labs(
    x = "Diarrhea",
    y = "Hemoglobin in Decigram/dl",
    fill = "Ethnicity"
  )+
  theme_bw()+
  ylim(25,225)+
  scale_fill_manual(values = c("#F546C8", "#7346F5"))
hemoethdiar





#######################################
hemowealfev <- ggplot(GUKR71FL, aes(factor(h22x), hw56, fill = factor(v190)))+
  geom_violin() +
  facet_wrap(~factor(h22x), scale="free") +
  labs(
    x = "Fever",
    y = "Hemoglobin in Decigram/dl",
    fill = "Wealth Quintile"
  )+
  theme_bw()+
  ylim(25,225)+
  scale_fill_manual(values = c("#F54671", "#F546C8", "#CA46F5", "#7346F5", "#46C8F5"))

hemowealfev

hemoethfev <- ggplot(GUKR71FL, aes(factor(h22x), hw56, fill = factor(v131x)))+
  geom_violin() +
  facet_wrap(~factor(h22x), scale="free") +
  labs(
    x = "Fever",
    y = "Hemoglobin in Decigram/dl",
    fill = "Ethnicity"
  )+
  theme_bw()+
  ylim(25,225)+
  scale_fill_manual(values = c("#F546C8", "#7346F5"))

hemoethfev

####################################
hemowealfloor <- ggplot(GUKR71FL, aes(factor(v127x), hw56, fill = factor(v190)))+
  geom_violin(trim = FALSE) +
  stat_summary(fun=mean, geom="point", shape=23, size=2)+
  facet_wrap(~factor(v127x), scale="free") +
  labs(
    x = "Flooring Material",
    y = "Hemoglobin in Decigram/dl",
    fill = "Wealth Quintile"
  )+
  theme_bw()+
  ylim(25,225)+
  scale_fill_manual(values = c("#F54671", "#F546C8", "#CA46F5", "#7346F5", "#46C8F5"))
hemowealfloor

hemoethfloor <- ggplot(GUKR71FL, aes(factor(v127x), hw56, fill = factor(v131x)))+
  geom_violin(trim = FALSE) +
  facet_wrap(~factor(v127x), scale="free") +
  labs(
    x = "Flooring Material",
    y = "Hemoglobin in Decigram/dl",
    fill = "Wealth Quintile"
  )+
  theme_bw()+
  ylim(25,225)+
  scale_fill_manual(values = c("#F546C8", "#7346F5"))
hemoethfloor

#VARIABLES

GUKR71FL$v131x <- NA
GUKR71FL$v131x <- ifelse(GUKR71FL$v131 == 1, 1, GUKR71FL$v131x)
GUKR71FL$v131x <- ifelse(GUKR71FL$v131 == 2, 0, GUKR71FL$v131x)
factor(GUKR71FL$v131)

GUKR71FL$v414hx <- NA
GUKR71FL$v414hx <- ifelse(GUKR71FL$v414h == 1, 1, GUKR71FL$v414hx)
GUKR71FL$v414hx <- ifelse(GUKR71FL$v414h == 0, 0, GUKR71FL$v414hx)
factor(GUKR71FL$v414h)

GUKR71FL$h11x <- NA
GUKR71FL$h11x <- ifelse(GUKR71FL$h11 == 2, 1, GUKR71FL$h11x)
GUKR71FL$h11x <- ifelse(GUKR71FL$h11 == 0, 0, GUKR71FL$h11x)
factor(GUKR71FL$h11)

GUKR71FL$h22x <- NA
GUKR71FL$h22x <- ifelse(GUKR71FL$h22 == 1, 1, GUKR71FL$h22x)
GUKR71FL$h22x <- ifelse(GUKR71FL$h22 == 0, 0, GUKR71FL$h22x)
factor(GUKR71FL$h22)

GUKR71FL$v113x <- NA
GUKR71FL$v113x <- ifelse(GUKR71FL$v113 == 11, 1, GUKR71FL$v113x)
GUKR71FL$v113x <- ifelse(GUKR71FL$v113 == 12, 1, GUKR71FL$v113x)
GUKR71FL$v113x <- ifelse(GUKR71FL$v113 == 13, 1, GUKR71FL$v113x)
GUKR71FL$v113x <- ifelse(GUKR71FL$v113 == 14, 1, GUKR71FL$v113x)
GUKR71FL$v113x <- ifelse(GUKR71FL$v113 == 31, 3, GUKR71FL$v113x)
GUKR71FL$v113x <- ifelse(GUKR71FL$v113 == 32, 3, GUKR71FL$v113x)
GUKR71FL$v113x <- ifelse(GUKR71FL$v113 == 41, 4, GUKR71FL$v113x)
GUKR71FL$v113x <- ifelse(GUKR71FL$v113 == 42, 4, GUKR71FL$v113x)
GUKR71FL$v113x <- ifelse(GUKR71FL$v113 == 43, 4, GUKR71FL$v113x)
GUKR71FL$v113x <- ifelse(GUKR71FL$v113 == 44, 4, GUKR71FL$v113x)
GUKR71FL$v113x <- ifelse(GUKR71FL$v113 == 51, 5, GUKR71FL$v113x)
GUKR71FL$v113x <- ifelse(GUKR71FL$v113 == 61, 6, GUKR71FL$v113x)
GUKR71FL$v113x <- ifelse(GUKR71FL$v113 == 71, 6, GUKR71FL$v113x)


GUKR71FL$v127x <- NA
GUKR71FL$v127x <- ifelse(GUKR71FL$v127 == 11, 1, GUKR71FL$v127x)
GUKR71FL$v127x <- ifelse(GUKR71FL$v127 == 21, 2, GUKR71FL$v127x)
GUKR71FL$v127x <- ifelse(GUKR71FL$v127 == 23, 2, GUKR71FL$v127x)
GUKR71FL$v127x <- ifelse(GUKR71FL$v127 == 31, 3, GUKR71FL$v127x)
GUKR71FL$v127x <- ifelse(GUKR71FL$v127 == 32, 3, GUKR71FL$v127x)
GUKR71FL$v127x <- ifelse(GUKR71FL$v127 == 33, 3, GUKR71FL$v127x)
GUKR71FL$v127x <- ifelse(GUKR71FL$v127 == 34, 3, GUKR71FL$v127x)
GUKR71FL$v127x <- ifelse(GUKR71FL$v127 == 35, 3, GUKR71FL$v127x)


GUKR71FL$v116x <- NA
GUKR71FL$v116x <- ifelse(GUKR71FL$v116 == 11, 1, GUKR71FL$v116x)



factor(GUKR71FL$v113)

factor(GUKR71FL$v127)

factor(GUKR71FL$v205)


attach(GUKR71FL)
summary(aov(hw56 ~ factor(v190) + factor(v131x))) 

summary(aov(hw56 ~ factor(v113x) + factor(v131x)))

summary(aov(hw56 ~ factor(v414hx) + factor(v131x)))

summary(aov(hw56 ~ factor(v205) + factor(v131x)))

summary(aov(hw56 ~ factor(h11x) + factor(v131x)))

summary(aov(hw56 ~ factor(h22x) + factor(v131x))) 

summary(aov(hw56 ~ factor(v127x) + factor(v131x)))


summary(aov(hw56 ~ factor(v190)))

summary(aov(hw56 ~ factor(v113x)))

summary(aov(hw56 ~ factor(v414hx)))

summary(aov(hw56 ~ factor(v205)))

summary(aov(hw56 ~ factor(h11x)))

summary(aov(hw56 ~ factor(h22x)))

summary(aov(hw56 ~ factor(v127x)))


TukeyHSD()

#MULTIPLE REGRESSION HEMOGLOBIN PREDICTION
attach(GUKR71FL)


lm1<-lm(hw56~factor(v131x) + factor(v113x) + factor(h11))
summary(lm1)
#LOGISTIC REGRESSION ETHNICITY PREDICTION

glm<-glm(v131x~factor(v414h), family = binomial(link = "logit"))
summary(glm)
