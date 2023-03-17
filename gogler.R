setwd("C:/Users/torrealb000/Desktop/Applied Stats/day2log")
library(readxl)
FirstYearGPA <- read_excel("FirstYearGPA.xlsx", 
                           col_types = c("numeric", "numeric", "numeric", 
                                         "numeric", "text", "text", "text"))
attach(FirstYearGPA)

#a

baller <-t.test(GPA~FirstGen)
baller

  