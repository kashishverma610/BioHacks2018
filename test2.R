# Library
library(tidyverse)


vector1 <- c("dif", "apo" )
vector2 <- c(25,13)

#result <- array(c(vector1,vector2),dim = c(3,3,2))

# Create data
#data=data.frame(x=LETTERS[1:26], y=abs(rnorm(26)))
data=data.frame(x=vector1, y=vector2)
library(ggplot2)

# Horizontal 
ggplot2(data, aes(x=x, y=y)) +
  geom_segment( aes(x=x, xend=x, y=0, yend=y), color="skyblue") +
  geom_point( color="blue", size=4, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )

 