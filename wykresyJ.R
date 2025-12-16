library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)


### Wczytanie danych z danej ramki

df_ruchy <- read.csv("TWD_Projekt2\\output_moves.csv")
df_dane_partii <- read.csv("TWD_Projekt2\\output_game_info.csv")
View(df_dane_partii)
View(df_ruchy)

### Do wpisania nicki - potrzebne do wygranych czy ruchów
nick = c("FirejFox")

###########################
### Tworzenie wykresu ile sumarycznie gier w jakim dniu tygodnia

### Zamiana dat i tworzenie dnia tygodnia
df_dane_partii <- df_dane_partii %>% mutate(date_played = ymd(date_played)) %>% 
  mutate(weekday = wday(date_played, label=TRUE, abbr=FALSE))

### Dane do wykresu
df_dni_partie <- df_dane_partii %>% group_by(weekday) %>% summarise(ile = n())
View(df_dni_partie)

### Wykres
df_dni_partie %>% ggplot(aes(x=weekday, y = ile)) + geom_col() +
  labs(title="Rozkład ruchów na dni tygodnia")
###########################

### Tworzenie wykresu ile razy jaką gigurą, czy pionkiem się ruszyłem

df_ruchy %>% filter(player %in% nick) %>% group_by(piece) %>% summarise(ile = n()) %>% 
  ggplot(aes(x=piece, y=ile)) + geom_col() + labs(title = "Ilość ruchów na typ figury")

### Ten sam pomysł, ale dzielimy ruchy na poszczególne figury
df_ruchy %>% filter(player %in% nick) %>% group_by(piece) %>% summarise(ile = n()) %>%
  mutate(ile = case_when(
    piece == "P" ~ ile/8,
    piece %in% c("R","B","N") ~ile/2,
    .default = ile
   )) %>% 
  ggplot(aes(x=piece, y=ile)) + geom_col() + labs(title = "Ilość ruchów na pojedyńczą figurę")

### Rozkład partii względem długości
df_ruchy %>% filter(is_game_over==1) %>% select(move_no) %>% ggplot(aes(x=move_no)) +
  geom_histogram() + labs(title= "Rozkład partii względem ilości ruchów")

### Rozkład matów względem rodzaju figury, wynik trochę oczywisty
df_ruchy %>% filter(is_check_mate==1) %>% select(piece) %>% group_by(piece) %>% 
  summarise(ile = n()) %>% ggplot(aes(x=piece, y=ile)) + geom_col() +
  labs(title = "Rozkład matów względem typu figury")

### Heatmapa pól szachowych na planszy - ile na razy na jakie pole się ruszylismy, 
### czyli nie uwzględeniamy pól startowych

df_pola <- df_ruchy %>% select(to_square) %>% group_by(to_square) %>% summarise(ile = n())
df_pola <- df_pola %>% mutate(y = substr(to_square,2,2), x = substr(to_square,1,1)) %>% 
  select(-to_square)

df_pola %>% ggplot(aes(x=x, y=y, fill=ile)) + geom_tile()


### Średnia z partii
ilosc_partii <- dim(df_dane_partii)[1]

df_pola %>% mutate(ile = ile/ilosc_partii) %>% ggplot(aes(x=x, y=y, fill=ile)) + 
  geom_tile(color="black") +
  scale_fill_gradient(low = "white", high = "darkgreen") + theme_minimal() +
  labs(title="Średnie odwiedzanie pól względem partii")

### Zakończenie partii

df_dane_partii %>% select(termination, winner, weekday)


### Wygrana w zależności od dnia tygodnia
df_dane_partii %>% select(winner,weekday) %>% mutate(wygrana=case_when(
  winner %in% nick ~ "wygrana",
  winner == "draw" ~ "remis",
  .default = "przegrana")) %>% filter(!(wygrana=="remis")) %>% 
  group_by(weekday,wygrana) %>% summarise(ile = n()) %>% 
  ggplot(aes(x=weekday,y=ile,fill=wygrana)) + geom_col(position = "dodge") +
  labs(title = "Wygrane i przegrane w zależności od dnia tygodnia")


