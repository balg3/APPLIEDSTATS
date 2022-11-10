data("mtcars")
library(ggplot2)
#first one

setwd("C:/Users/upist/Desktop/Applied Stats/assignment14")
ggplot(mtcars, aes(x = factor(am), y = qsec, fill = factor(am))) +
  geom_boxplot(show.legend = FALSE) +
  labs(
    title = "Transmission Types vs Quarter Mile in Seconds", 
    y = "Quarter Mile in (S)",
    x = "Transmission Type") +
  theme_light()+
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5) )+
  coord_flip() +
  scale_x_discrete(
    labels = c("Automatic", "Manual")) +
  scale_fill_manual(
    values = c("52, 107, 235","235, 52, 201"))
#number 4

ggplot(data = mtcars, aes(x= factor(carb), fill=factor(carb))) +
  geom_bar(show.legend = FALSE) +
  labs(
    title = "Number of Cars by Carburator type",
    x = "Carb Type",
    y = "Number" )+
  scale_y_continuous(breaks = c(1:10))+
  theme_light() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  scale_fill_brewer(palette = "GnBu")

#numba 5

ggplot(mtcars, aes(x =factor(gear), fill = factor(cyl))) +
  geom_bar(show.legend = TRUE) +
  labs( title = "Number of Cars by Gear Type",
    subtitle = "Stacked by Cyl Type",
    x = "Gear Type",
    y = "Number of Cars",
    fill = "Cyl Type") +
  theme_light()+
  theme(
    plot.title = element_text(face = "bold", hjust = .5,),
    plot.subtitle = element_text(hjust = .5)) +
  scale_y_continuous(breaks = c(1:20)) +
  scale_fill_manual(values=c("#64eb34","#34e5eb","#3734eb"))

#numbea 6

ggplot(data =mtcars, aes(x = wt, y =mpg)) +
  geom_point() +
  labs(
    title="MPG as a Function of Weight in 1000s Pounds",
    x="Weight in 1000s Pounds",
    y="Miles Per Gallon") +
  theme_light() +
  theme(
    plot.title = element_text(face = "bold", hjust = .5))

#number 7

ggplot(data =mtcars, aes(x = hp, y = mpg))
