setwd("C:/Users/torrealb000/Desktop/Applied Stats/poster contest")
library(ggplot2)
attach(accident)
#need to remove all 9s from injury severity, since they do not tell us anything

accident <- read.csv("C:/Users/torrealb000/Desktop/Applied Stats/poster contest/datasets/CRSS2020CSV/accident.csv", row.names=1, stringsAsFactors=TRUE)

edited <- replace(accident$MAX_SEV, accident$MAX_SEV>8, NA)
#forget that^^ this is bettervv

accident$MAX_SEV[accident$MAX_SEV == 9] <- NA

