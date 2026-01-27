library(shinydashboard)
library(shiny)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

### Wczytanie danych i obróbka

df_ruchy <- read.csv("..\\apk\\dane\\full_moves.csv")
df_dane_partii <- read.csv("..\\apk\\dane\\full_game_info.csv")
df_debiuty <- read.csv("..\\apk\\dane\\debiuty.csv")
df_debiuty <- df_debiuty %>% filter(name!="")

df_temp <- read.csv("..\\apk\\dane\\podsumowanie.csv")



nick = c("FirejFox","BarMan-ek","GDgamers","bArmAnEk")

df_dane_partii<-df_dane_partii %>% 
  mutate(gracz = if_else(white %in% nick,white,if_else(black %in% nick,black, NA))) %>% 
  mutate(gracz = if_else(gracz=="BarMan-ek","bArmAnEk",gracz)) %>% 
  mutate(date_played = ymd(date_played)) %>% 
  mutate(weekday = wday(date_played, label=TRUE, abbr=FALSE)) %>% 
  mutate(year = year(date_played))

df_ruchy <- df_ruchy %>% left_join(df_dane_partii %>% select(game_id, gracz, weekday, year),
                                   by = "game_id")

df_debiuty <- df_debiuty %>% left_join(df_dane_partii %>% select(game_id, gracz, weekday, year),
                                       by = "game_id")


ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                box(title= "Wygrane coś coś gdhfsghrsghdfghghdrhbfgbgnrnbdfhn",
                      plotOutput("plot1", height = 250)),
                
                box(
                  title = "Controls",
                  sliderInput("slider", "Number of observations:", 1, 100, 50)
                )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "widgets",
              h2("Widgets tab content"),
              fluidRow(
                box(title = "Weekday wins",
                  plotOutput("weekday_wins")
                ),
                box(selectInput("gracz",
                                "Wybierz użytkownika: ",
                                choices = c("Wszyscy" = "all",
                                            "Janek" ="FirejFox",
                                            "Bartek" = "bArmAnEk",
                                            "Wojtek" = "GDgamers")),
                    
                    sliderInput("lata", "Lata",min=2017,max=2025,step=1,
                                value = c(2017,2025)))
              )
      )
    )
  )
)

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  output$weekday_wins <- renderPlot({
    
    rok <- input$lata
    gracze <- input$gracz
    
    if (gracze=="all")
      gracze = nick
    poziomy = c("poniedziałek","wtorek","środa","czwartek","piątek","sobota","niedziela")
    
    wykres<-df_dane_partii %>% filter(year<=rok[2] & year>=rok[1]) %>% filter(gracz %in% c(gracze)) %>% 
      select(winner,weekday) %>% mutate(wygrana=case_when(
        winner %in% nick ~ "wygrana",
        winner == "draw" ~ "remis",
        .default = "przegrana")) %>% filter(!(wygrana=="remis")) %>% 
      group_by(weekday,wygrana) %>% summarise(ile = n()) %>% 
      mutate(weekday = factor(weekday, levels=poziomy))%>% 
      ggplot(aes(y=weekday,x=ile,fill=wygrana)) + geom_col(position = "dodge") +
      labs(title = "Wygrane i przegrane w zależności od dnia tygodnia", x = "liczba wygranych",
           y = "dzień tygodnia", fill="") +
      theme(
        panel.background = element_blank(),
        plot.background = element_rect(colour = "white"),
        plot.title.position = "plot",
        plot.title = element_text(hjust=0.5),
        axis.text.x= element_text(color = "black",vjust=1,size=10),
        axis.text.y = element_text(color="black",size=10),
        axis.ticks.y = element_line(color="black"),
        panel.grid.major.y = element_line(color="black",linetype=1),
        panel.grid.minor.y = element_line(color="black"),
        panel.grid.minor.x = element_line(colour = "black"),
        axis.ticks.x = element_blank(),
        legend.text = element_text(color="black",size=14),
        legend.title = element_text(color="black",size=14),
        title = element_text(size=16),
        panel.grid.major.x = element_blank()
      ) + scale_fill_discrete(palette = c("green","red"), limits=c("wygrana","przegrana"))
    wykres
    
  })
}

shinyApp(ui, server)
