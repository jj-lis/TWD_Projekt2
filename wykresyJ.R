library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)


### Wczytanie danych z danej ramki

df_ruchy <- read.csv("TWD_Projekt2\\output_moves.csv")
df_dane_partii <- read.csv("TWD_Projekt2\\output_game_info.csv")
View(df_dane_partii)


###########################
### Tworzenie wykresu ile sumarycznie gier w jakim dniu tygodnia

### Zamiana dat i tworzenie dnia tygodnia
df_dane_partii <- df_dane_partii %>% mutate(date_played = ymd(date_played)) %>% 
  mutate(weekday = wday(date_played, label=TRUE, abbr=FALSE))

### Dane do wykresu
df_dni_partie <- df_dane_partii %>% group_by(weekday) %>% summarise(ile = n())
View(df_dni_partie)

### Wykres
df_dni_partie %>% ggplot(aes(x=weekday, y = ile)) + geom_col()
###########################

### Tworzenie wykresu ile razy jaką gigurą, czy pionkiem się ruszyłem
nick = c("FirejFox")

df_ruchy %>% filter(player %in% nick) %>% group_by(piece) %>% summarise(ile = n()) %>% 
  ggplot(aes(x=piece, y=ile)) + geom_col()

### Ten sam pomysł, ale dzielimy ruchy na poszczególne figury
df_ruchy %>% filter(player %in% nick) %>% group_by(piece) %>% summarise(ile = n()) %>%
  mutate(ile = case_when(
    piece == "P" ~ ile/8,
    piece %in% c("R","B","N") ~ile/2,
    .default = ile
   )) %>% 
  ggplot(aes(x=piece, y=ile)) + geom_col()



str(df_dane_partii)

