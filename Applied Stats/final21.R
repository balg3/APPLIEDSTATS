setwd("C:/Users/torrealb000/Desktop/Applied Stats/FINAL2")
library(haven)
library(ggplot2)
library(GGally)
GUKR71FL <- read_dta("GUKR71FL.DTA")
View(GUKR71FL)
attach(GUKR71FL)

hemoweal <- ggplot(GUKR71FL, aes(v191, hw56, color = factor(v131)))+
  geom_point()+
  labs(
    x = "Hemoglobin in centigram/dl",
    y = "Wealth Index",
    title = "Hemoglobin as a function of Wealth Index",
    color = "Ethnicity"
  )+
  scale_color_manual(values = c("1"="#a134eb",
                                "2"="#5634eb",
                                "3"="#eb34c3",
                                "4"="#eb5334",
                                "5"="#40eb34",
                                "8"="#34ebe2",))
hemoweal


