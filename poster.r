setwd("C:/Users/upist/Desktop/Applied Stats/poster")
accidentCRSS <- read.csv("C:/Users/upist/Desktop/Applied Stats/poster/CRSS/accident.csv", row.names=1)
accidentFARS <- read.csv("C:/Users/upist/Desktop/Applied Stats/poster/FARS/accident2.CSV")

library(ggplot2)
library(GGally)
library(dplyr)

accidentCRSS$MAX_SEV[accident$MAX_SEV == 9] <- NA
accidentCRSS$ALCOHOL[accident$ALCOHOL == 9] <- NA

weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

ggplot(accidentCRSS, aes(fill = NUM_INJ , x = factor(DAY_WEEKNAME))) +
  scale_x_discrete(limits = weekdays) +
  geom_bar(show.legend = TRUE) +
  scale_fill_brewer(palette = "GnBu") +
  labs(
    title = "Weekday vs. Number of Injuries",
    x = "Day of The Week",
    y = "Number of Reported Injuries",
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust = .5)
  ) 
