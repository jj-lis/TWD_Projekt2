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
  dashboardHeader(title = "Analiza "),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th")),
      menuItem("Third", tabName = "third"),
      selectInput("player",
                  "Wybierz użytkownika: ",
                  choices = c("Wszyscy" = "all",
                              "Janek" ="FirejFox",
                              "Bartek" = "bArmAnEk",
                              "Wojtek" = "GDgamers")
                  ),
      sliderInput("lata", "Lata",min=2017,max=2025,step=1,
                  value = c(2017,2025), sep = ""
    )
  )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                
                box(
                  selectInput("gracz",
                              "Wybierz użytkownika: ",
                              choices = c("Wszyscy" = "all",
                                          "Janek" ="FirejFox",
                                          "Bartek" = "bArmAnEk",
                                          "Wojtek" = "GDgamers")),
                  
                  sliderInput("lata", "Lata",min=2017,max=2025,step=1,
                              value = c(2017,2025)
                )
              ), box(
                tableOutput("podsumowanie")
              ), box( plotOutput("weekday_wins")),
              box(plotOutput("kolowy"))
      )),
      
      # Second tab content
      tabItem(tabName = "widgets",
              h2("Widgets tab content"),
              fluidRow(
                box(selectInput("player",
                                "Wybierz użytkownika: ",
                                choices = c("Wszyscy" = "all",
                                            "Janek" ="FirejFox",
                                            "Bartek" = "bArmAnEk",
                                            "Wojtek" = "GDgamers"))
                ),
                box(sliderInput("lata", "Lata",min=2017,max=2025,step=1,
                                value = c(2017,2025))),
                    box(
                plotOutput("rozklad_partii")),
                box(plotOutput("rozklad_material"))
              )),
      tabItem(tabName = "third",
              fluidRow(
              box(selectInput("player",
                          "Wybierz użytkownika: ",
                          choices = c("Wszyscy" = "all",
                                      "Janek" ="FirejFox",
                                      "Bartek" = "bArmAnEk",
                                      "Wojtek" = "GDgamers"))),
              box(sliderInput("lata", "Lata",min=2017,max=2025,step=1,
                              value = c(2017,2025))),
              box(sliderInput("debiut_dlg", "Dlugość debiutu", min = 1, max=10, step=1,
                          value=c(1,5))),
              box(plotOutput("heatmap_ruchy")),
              box(plotOutput("debiuty_liczba"))
              )
              )
      )
      )
    )

  


server <- function(input, output) {
  output$weekday_wins <- renderPlot({
    
    rok <- input$lata
    gracze <- input$player
    
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
  
  output$rozklad_partii <- renderPlot({
    rok <- input$lata
    gracze <- input$player
    
    if (gracze=="all")
      gracze = nick
    
    wykres <- df_ruchy %>% filter(year<=rok[2] & year>=rok[1]) %>% filter(gracz %in% c(gracze)) %>% 
      select(game_id,move_no) %>% group_by(game_id) %>% summarise(ruchy = max(move_no)) %>% 
      ggplot(aes(x=ruchy)) + geom_histogram(fill="black",colour="lightgray",bins = 30) + 
      labs(title= "Rozkład partii względem ilości ruchów", x = "Liczba ruchów",y = "liczba partii") +
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
        title = element_text(size=16),
        panel.grid.major.x = element_blank()
      )
    
    wykres
  })
  
  output$rozklad_material <- renderPlot({
    
    rok <- input$lata
    
    wykres<-df_ruchy %>% filter(year<=rok[2] & year>=rok[1]) %>% 
      select(gracz, move_no,white_pawn_count,white_queen_count,white_bishop_count,white_knight_count,
             white_rook_count,black_pawn_count,black_queen_count,black_bishop_count,
             black_knight_count,black_rook_count) %>% 
      mutate(value = 9*(white_queen_count + black_queen_count)+
               5*(white_rook_count + black_rook_count)+
               3*(white_bishop_count+black_bishop_count+white_knight_count+black_knight_count)+
               (white_pawn_count+black_pawn_count)) %>% 
      select(move_no, value, gracz) %>% group_by(move_no, gracz) %>% summarise(avg = mean(value)) %>% 
      mutate(gracz = case_when(
        gracz == "FirejFox" ~ "Janek",
        gracz == "GDgamers"~ "Wojtek",
        .default = "Bartek"
      )) %>% 
      ggplot(aes(x=move_no, y = avg, color=gracz)) + geom_line(size=1) +
      labs(title = "Średni materiał na planszy ze względu na długość partii",
           x = "Liczba ruchów", y = "Średni materiał", color = "Gracz") +
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
      )
    wykres
    
  })
  
  output$rozklad_figury <- renderPlot({
    
    rok <- input$lata
    
    wykres<-df_ruchy %>% filter(year<=rok[2] & year>=rok[1]) %>% 
      select(move_no,white_count,black_count,gracz) %>% mutate(liczba = white_count+black_count) %>% 
      group_by(move_no, gracz) %>% summarise(avg = mean(liczba)) %>% 
      ggplot(aes(x=move_no, y=avg,color=gracz)) + geom_line() +
      labs(title= "Średnia liczba figur na planszy ze względu na liczbę ruchów")
    wykres
  })
  
  output$heatmap_ruchy <- renderPlot({
    rok <- input$lata
    gracze <- input$player
    
    if (gracze=="all")
      gracze = nick
    
    
    df_pola <- df_ruchy %>% select(to_square, gracz, year) %>% 
      filter(year<=rok[2] & year>=rok[1]) %>% filter(gracz %in% c(gracze)) %>% 
      group_by(to_square) %>% summarise(ile = n())
    
    df_pola <- df_pola %>% mutate(y = substr(to_square,2,2), x = substr(to_square,1,1)) %>% 
      select(-to_square)
    
    ilosc_partii <- dim(df_dane_partii %>% filter(gracz %in% c(gracze)) %>% 
                          filter(year <= rok[2] & year>=rok[1]))[1]
    
    wykres<-df_pola %>% mutate(ile = ile/ilosc_partii) %>% ggplot(aes(x=x, y=y, fill=ile)) + 
      geom_tile(color="black") +
      scale_fill_gradient(low = "white", high = "darkgreen", name="Średnia") +theme_minimal()+
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x= element_text(color = "black",vjust=1,size=10),
        axis.text.y = element_text(color="black",size=10),
        legend.text = element_text(color="black",size=14),
        legend.title = element_text(color="black",size=14),
        legend.ticks = element_line(colour = "black"),
        title = element_text(size=16),
        plot.title.position = "plot",
        plot.title = element_text(hjust=0)
      )+
      guides(fill = guide_colorbar(barwidth = 1.5, barheight = 15)) +
      labs(title="Średnie odwiedzanie pól względem partii") +
      coord_fixed()
    wykres
  })
  
  output$debiuty_liczba <- renderPlot({
    rok <- input$lata
    gracze <- input$player
    dlg <- input$debiut_dlg
    
    if (gracze=="all")
      gracze = nick
    
    
    
    wykres<- df_debiuty %>% filter(year<=rok[2] & year>=rok[1]) %>% filter(gracz %in% c(gracze)) %>%
      group_by(game_id) %>% 
      filter(move_no<=dlg[2] & move_no >= dlg[1]) %>% summarise(debiut = last(name), .groups = "drop") %>% 
      group_by(debiut) %>% summarise(ile = n()) %>% top_n(5) %>% arrange() %>% 
      ggplot(aes(y=debiut,x=ile)) + geom_col(fill="black") + 
      labs(title=paste("Top 5 rozegranych debiutów od", dlg[1],"do",dlg[2],"ruchów",sep=" "),
           x = "Liczba", y = "Debiut") +
      theme(
        panel.background = element_blank(),
        plot.background = element_rect(colour = "white"),
        plot.title.position = "plot",
        plot.title = element_text(hjust=0.5),
        axis.text.x= element_text(color = "black",vjust=1,size=14),
        axis.text.y = element_text(color="black",size=14),
        axis.ticks.y = element_line(color="black"),
        panel.grid.major.y = element_line(color="black",linetype=1),
        panel.grid.minor.y = element_line(color="black"),
        panel.grid.minor.x = element_line(colour = "black"),
        axis.ticks.x = element_blank(),
        title = element_text(size=16),
        panel.grid.major.x = element_blank()
      )
    wykres
    
    
    
  })
  
  output$kolowy <- renderPlot({
    
    rok <- input$lata
    gracze <- input$gracz
    
    if (gracze=="all")
      gracze = nick
    
    wykres<-df_dane_partii %>%  filter(year<=rok[2] & year>=rok[1]) %>% filter(gracz %in% c(gracze)) %>% 
      select(winner,gracz) %>% mutate(wygrana=case_when(
        winner %in% nick ~ "wygrana",
        winner == "draw" ~ "remis",
        .default = "przegrana")) %>% group_by(wygrana) %>% summarise(ile = n()) %>% 
      ggplot(aes(x = "", y = ile, fill = wygrana)) +
      geom_bar(stat="identity", width = 1, color = "white") +
      coord_polar("y", start = 0) +
      theme_void() +
      geom_text(aes(label = wygrana),size=6,position = position_stack(vjust = 0.5)) +
      scale_fill_brewer(palette="Set1") + guides(fill = "none")
    
    wykres
  })
  
  output$podsumowanie <- renderTable({df_temp
    
    
  })
}

shinyApp(ui, server)
