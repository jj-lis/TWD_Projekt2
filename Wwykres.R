library(dplyr)
library(ggplot2)

game_info <- read.csv("output3_game_info.csv")
moves <- read.csv("output3_moves.csv")

game_info <- game_info %>% 
  mutate(color=ifelse(white=="GDgamers","white","black"),my_result=ifelse(winner=="GDgamers","win",ifelse(winner=="draw","draw","lose")))

game_info %>% 
  ggplot(aes(x=my_result,fill=color))+
  geom_bar()+
  scale_y_continuous(breaks = seq(0, 400, by = 50))
