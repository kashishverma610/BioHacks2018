# Library
library(tidyverse)
library(ggplot2)

# Create data
set.seed(1000)
vector1 <- LETTERS[16:1]
vector2 <- c(34.4,9.4,6.25,6.25,6.25,6.25,3.125,3.125,3.125,3.125,3.125,3.125,3.125,3.125,3.125,3.125)
#data=data.frame(x=LETTERS[1:26], y=(abs(rnorm(26))))
data=data.frame(x=vector1, y=vector2)


# Reorder the data
data=data %>%
  arrange(y) %>%
  mutate(x=factor(x,x))

# Plot
png("~/Dropbox/R_GG/R_GRAPH/#304_Lollipop_with_highlighted_group.png", height = 480, width=520)
p = ggplot(data, aes(x=x, y=y)) +
  geom_segment( aes(x=x, xend=x, y=0, yend=y ), color=ifelse(data$x %in% c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z"), "blue", "black"), size=ifelse(data$x %in% c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z"), 1, 0.5) ) +
  geom_point( color=ifelse(data$x %in% c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z"), "blue", "black"), size=ifelse(data$x %in% c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z"), 2, 1) ) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) + 
  xlab("Function")+
  ylab("Probability")+
  labs(title="Gene:FAM217B, CABLES2, KIAA1755, LKAAEAR1",subtitle="Cerebral Cortex")+
  p

dev.off()

# You can even add a few annotation on the plot !
png("~/Dropbox/R_GG/R_GRAPH/#304_Lollipop_with_highlighted_group2.png", height = 480, width=520)
p + 
  annotate("text", x = grep("A", data$x), y = data$y[which(data$x=="A")]*1.2, label = "34.4%", color="purple", size=4 , angle=0, fontface="plain", hjust=-14.5) + 
  annotate("text", x = grep("B", data$x), y = data$y[which(data$x=="B")]*1.2, label = "9.4%", color="purple", size=4 , angle=0, fontface="plain", hjust=-3.5) + 
  annotate("text", x = grep("C", data$x), y = data$y[which(data$x=="C")]*1.2, label = "6.25%", color="purple", size=4 , angle=0, fontface="plain", hjust=-1.5) +
  annotate("text", x = grep("D", data$x), y = data$y[which(data$x=="D")]*1.2, label = "6.25%", color="purple", size=4 , angle=0, fontface="plain", hjust=-1.5) +
  
  annotate("text", x = grep("E", data$x), y = data$y[which(data$x=="E")]*1.2, label = "6.25%", color="purple", size=4 , angle=0, fontface="plain", hjust=-1.5) + 
  annotate("text", x = grep("F", data$x), y = data$y[which(data$x=="F")]*1.2, label = "6.25%", color="purple", size=4 , angle=0, fontface="plain", hjust=-1.5) + 
  annotate("text", x = grep("G", data$x), y = data$y[which(data$x=="G")]*1.2, label = "3.125%", color="purple", size=4 , angle=0, fontface="plain", hjust=0) +
  annotate("text", x = grep("H", data$x), y = data$y[which(data$x=="H")]*1.2, label = "3.125%", color="purple", size=4 , angle=0, fontface="plain", hjust=0) +
  
  annotate("text", x = grep("I", data$x), y = data$y[which(data$x=="I")]*1.2, label = "3.125%", color="purple", size=4 , angle=0, fontface="plain", hjust=0) + 
  annotate("text", x = grep("J", data$x), y = data$y[which(data$x=="J")]*1.2, label = "3.125%", color="purple", size=4 , angle=0, fontface="plain", hjust=0) + 
  annotate("text", x = grep("K", data$x), y = data$y[which(data$x=="K")]*1.2, label = "3.125%", color="purple", size=4 , angle=0, fontface="plain", hjust=1.5) +
  annotate("text", x = grep("L", data$x), y = data$y[which(data$x=="L")]*1.2, label = "3.125%", color="purple", size=4 , angle=0, fontface="plain", hjust=1.5) +
  annotate("text", x = grep("M", data$x), y = data$y[which(data$x=="M")]*1.2, label = "3.125%", color="purple", size=4 , angle=0, fontface="plain", hjust=1.5)+
  annotate("text", x = grep("N", data$x), y = data$y[which(data$x=="M")]*1.2, label = "3.125%", color="purple", size=4 , angle=0, fontface="plain", hjust=1.5)+
  annotate("text", x = grep("O", data$x), y = data$y[which(data$x=="M")]*1.2, label = "3.125%", color="purple", size=4 , angle=0, fontface="plain", hjust=1.5)+
  annotate("text", x = grep("P", data$x), y = data$y[which(data$x=="M")]*1.2, label = "3.125%", color="purple", size=4 , angle=0, fontface="plain", hjust=1.5)+
  
  
  annotate("text", x = 13.25, y = 30, label = "Functions",color ="black",size=5)+

  
  annotate("text", x = 12.25, y = 30, label = "A : Unfolded protein binding",color ="black",size=3)+
  annotate("text", x = 11.5, y = 30, label = "B : Ubiquitin-like protein binding",color ="black",size=3)+
  annotate("text", x = 10.75, y = 30, label = "C : Transferase activity, transferring acyl groups",color ="black",size=3)+
  annotate("text", x = 10, y = 30, label = "D : RNA binding",color ="black",size=3)+
  annotate("text", x = 9.25, y = 30, label = "E : Phosphatase activity",color ="black",size=3)+
  annotate("text", x = 8.5, y = 30, label = "F : Nucleotidyltransferase activity",color ="black",size=3)+
  annotate("text", x = 7.75, y = 30, label = "G : Lipid binding",color ="black",size=3)+
  annotate("text", x = 7, y = 30, label = "H : Enzyme regulator activity",color ="black",size=3)+
  annotate("text", x = 6.25, y = 30, label = "I : DNA binding",color ="black",size=3)+
  annotate("text", x = 5.5, y = 30, label = "J : ATPase activity",color ="black",size=3)+
  annotate("text", x = 4.75, y =30, label = "K : Transcription factor activity, protein binding",color ="black",size=3)+
  annotate("text", x = 4, y = 30, label = "L : Signal transducer activity",color ="black",size=3)+
  annotate("text", x = 3.25, y = 30, label = "M : Enzyme binding",color ="black",size=3)+
  annotate("text", x = 2.5, y = 30, label = "N : DNA binding transcription factor activity",color ="black",size=3)+
  annotate("text", x = 1.75, y =30, label = "O : Cytoskeletal protein binding",color ="black",size=3)+
  annotate("text", x = 1, y = 30, label = "P : Molecular Function",color ="black",size=3)
