setwd("C:/Users/torrealb000/Desktop/Applied Stats/FINAL2")
library(haven)
library(ggplot2)
library(GGally)
GUKR71FL <- read_dta("GUKR71FL.DTA")
View(GUKR71FL)
attach(GUKR71FL)

hemowealeth <- ggplot(GUKR71FL, aes(v191, hw56, color = factor(v131)))+
  geom_point(alpha = 0.45)+
  labs(
    y = "Hemoglobin in Decigram/Deciliter",
    x = "Wealth Index",
    title = "Hemoglobin as a Function of Wealth Index",
    color = "Ethnicity"
  )+
  scale_color_manual(values = c("1"="#34ebe2",
                                "2"="#ed4edd",
                                "3"="#5634eb",
                                "4"="#eb5334",
                                "6" ="orange",
                                "8"="#40eb34"))+
  theme_bw()
  
hemowealeth

GUKR71FL$Maya <- NA
GUKR71FL$Maya <- ifelse(GUKR71FL$v131 == 1, GUKR71FL$hw56, NA)
GUKR71FL$Mestizo <- NA
GUKR71FL$Mestizo <- ifelse(GUKR71FL$v131 == 2, GUKR71FL$hw56, NA)
GUKR71FL$Garifuna <- NA
GUKR71FL$Garifuna <- ifelse(GUKR71FL$v131 == 3, GUKR71FL$hw56, NA)
GUKR71FL$Xinca <- NA
GUKR71FL$Xinca <- ifelse(GUKR71FL$v131 == 4, GUKR71FL$hw56, NA)




hemoweal <- ggplot(GUKR71FL, aes(v191, hw56, color = factor(v131)))+
  geom_point(alpha = 0.4)+
  labs(
    y = "Hemoglobin in Decigram/Deciliter",
    x = "Wealth Index",
    title = "Hemoglobin as a Function of Wealth Index",
    color = "Ethnicity"
  )+
  scale_color_manual(values = c("1"="#34ebe2",
                                "2"="#5634eb",
                                "3"="#40eb34",
                                "4"="#eb5334",
                                "6" ="orange",
                                "8"="#a134eb"))

hemoweal
        