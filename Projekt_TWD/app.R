library(shiny)
library(dplyr)
library(ggplot2)
library(stringi)
library(plotly)
library(heatmaply)
library(tidyr)

ui <- fluidPage(
  titlePanel("League of Stats"),

  tabsetPanel(
    tabPanel("Zakładka_1",

             fluidRow(
               column(6, align = "center",
                      sliderInput(inputId = "date_range1",
                                  label = "Wybierz zakres dat:",
                                  min = as.Date("2024-10-31"),
                                  max = as.Date("2024-12-18"),
                                  value = c(as.Date("2024-10-31"), as.Date("2024-12-18")),
                                  timeFormat = "%d-%m-%Y"),
                      selectInput("dataset1",
                                  "Wybierz gracza:",
                                  choices = c("Gracz1", "Gracz2", "ProGracz1"),
                                  selected = "Gracz1"),
                      selectInput("position1",
                                  "Wybierz pozycję:",
                                  choices = c("Wszystkie Pozycje", "TOP", "JUNGLE", "MIDDLE", "BOTTOM", "UTILITY"),
                                  selected = "Wszystkie Pozycje")
               )
             ),
             fluidRow(
               column(12, plotlyOutput("plot1", height = "400px"))
             ),
             fluidRow(
               column(12, plotlyOutput("plot3", height = "400px"))
             ),
             fluidRow(
               column(12, plotlyOutput("plot4", height = "400px"))
             )
    ),
    
    tabPanel("Zakładka_2",
             fluidRow(
               column(6, align = "center",
                      sliderInput(inputId = "date_range2",
                                  label = "Wybierz zakres dat:",
                                  min = as.Date("2024-09-01"),
                                  max = as.Date("2024-12-30"),
                                  value = c(as.Date("2024-09-01"), as.Date("2024-12-30")),
                                  timeFormat = "%d-%m-%Y"),
                      selectInput("dataset2",
                                  "Wybierz gracza:",
                                  choices = c("Gracz1", "Gracz2", "ProGracz1"),
                                  selected = "Gracz1"),
                      selectInput("position2",
                                  "Wybierz pozycję:",
                                  choices = c("Wszystkie Pozycje", "TOP", "JUNGLE", "MIDDLE", "BOTTOM", "UTILITY"),
                                  selected = "Wszystkie Pozycje")
               )
             ),

             fluidRow(
               column(12, plotlyOutput("plot2", height = "400px"))
             ),

             fluidRow(
               column(12, plotlyOutput("plot5", height = "400px"))
             )
    ),
    tabPanel("Zakładka_3",
             fluidRow(
               column(6, align = "center",
                      sliderInput(inputId = "date_range3",
                                  label = "Wybierz zakres dat:",
                                  min = as.Date("2024-09-01"),
                                  max = as.Date("2024-12-30"),
                                  value = c(as.Date("2024-09-01"), as.Date("2024-12-30")),
                                  timeFormat = "%d-%m-%Y"),
                      selectInput("dataset3",
                                  "Wybierz gracza:",
                                  choices = c("Gracz1", "Gracz2", "ProGracz1"),
                                  selected = "Gracz1"),
                      selectInput("position3",
                                  "Wybierz pozycję:",
                                  choices = c("Wszystkie Pozycje", "TOP", "JUNGLE", "MIDDLE", "BOTTOM", "UTILITY"),
                                  selected = "Wszystkie Pozycje")
               )
             ),
             
             fluidRow(
               column(12, plotlyOutput("plot6", height = "600px"))
             )
    ),
    tabPanel("Zakładka_4",
             
             fluidRow(
               column(6, align = "center",
                      sliderInput(inputId = "date_range4",
                                  label = "Wybierz zakres dat:",
                                  min = as.Date("2024-10-31"),
                                  max = as.Date("2024-12-18"),
                                  value = c(as.Date("2024-10-31"), as.Date("2024-12-18")),
                                  timeFormat = "%d-%m-%Y"),
                      selectInput("dataset4",
                                  "Wybierz gracza:",
                                  choices = c("Gracz1", "Gracz2", "ProGracz1"),
                                  selected = "Gracz1"),
                      selectInput("position4",
                                  "Wybierz pozycję:",
                                  choices = c("Wszystkie Pozycje", "TOP", "JUNGLE", "MIDDLE", "BOTTOM", "UTILITY"),
                                  selected = "Wszystkie Pozycje")
               )
             ),
             fluidRow(
               column(12, plotlyOutput("plot7", height = "400px"))
             ),
             fluidRow(
               column(12, plotlyOutput("plot8", height = "400px"))
             ),
             fluidRow(
               column(12, plotlyOutput("plot9", height = "400px"))
             )
    )
    
  )
)

server <- function(input, output) {

  dataset1 <- reactive({
    nazwa_csv <- paste0(input$dataset1,".csv")
    data <- read.csv(nazwa_csv)
    data <- data %>%
      filter(
        Date >= as.POSIXct(as.character(input$date_range1[1]), "%Y-%m-%d"),
        Date <= as.POSIXct(as.character(input$date_range1[2]), "%Y-%m-%d"),
        if (input$position1 != "Wszystkie Pozycje") {
          Position == input$position1
        } else {
          TRUE
        }
      ) %>%
      mutate(
        year = as.numeric(format(as.POSIXct(Date), "%Y")),
        month = as.numeric(format(as.POSIXct(Date), "%m")),
        day = as.numeric(format(as.POSIXct(Date), "%d")),
        rest = format(as.POSIXct(Date), "%H:%M:%S"),
        Date = format(as.POSIXct(Date), "%m-%d")
      ) %>% 
      count(Date)
    return(data)
  })
  
  output$plot1 <- renderPlotly({
    data <- dataset1()
    total_games <- sum(data$n)
    plot <- ggplot(data, aes(x = Date, y = n)) +
      geom_col(fill = "#6D98BA") +
      ggtitle(paste("Gry na dzień       ", "             Łączna liczba gier:", total_games)) +  # Tytuł z odstępem tak dodałem spacjami to zostaw mnie w spokoju
      labs(
        x = "Data",
        y = "Ilość gier"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, size = 7), 
        axis.title = element_text(size = 14), 
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14, hjust = 0.5)
      )
    
    ggplotly(plot)
  })
  

  dataset2 <- reactive({
    nazwa_csv <- paste0(input$dataset2,".csv")
    data <- read.csv(nazwa_csv)
    data <- data %>%
      filter(
        Date >= as.POSIXct(as.character(input$date_range2[1]), "%Y-%m-%d"),
        Date <= as.POSIXct(as.character(input$date_range2[2]), "%Y-%m-%d"),
        if (input$position2 != "Wszystkie Pozycje") {
          Position == input$position2
        } else {
          TRUE
        }
      ) %>%
      count(Champion) %>% 
      filter(n > 5)
    return(data)
  })
  
  output$plot2 <- renderPlotly({
    data <- dataset2()
    plot2 <- ggplot(data, aes(x = Champion, y = n)) +
      geom_col(fill = "#A1C9A1") +
      labs(
        title = paste("Gry na danym championie"),
        x = "Championy",
        y = "Ilość gier"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, size = 7), 
        axis.title = element_text(size = 14), 
        plot.title = element_text(size = 16)  
      )
    ggplotly(plot2)
  })
  
  dataset3 <- reactive({
    nazwa_csv <- paste0(input$dataset1,".csv")
    data <- read.csv(nazwa_csv)
    data <- data %>%
      mutate(
        Date = format(as.POSIXct(Date), "%Y-%m-%d"),
        Day = format(as.POSIXct(Date), "%m-%d"),
        win = ifelse(Win == "True", 1, 0)
      ) %>%
      filter(
        Date >= as.POSIXct(as.character(input$date_range1[1]), "%Y-%m-%d"),
        Date <= as.POSIXct(as.character(input$date_range1[2]), "%Y-%m-%d"),
        if (input$position1 != "Wszystkie Pozycje") {
          Position == input$position1
        } else {
          TRUE
        }
      ) %>%
      group_by(Day) %>%
      summarise(
        total_matches = n(),
        wins = sum(win, na.rm = TRUE),
        win_ratio = wins / total_matches,
        Date = Date,
        .groups = "drop"
      ) %>% 
      unique()
    
    return(data)
  })
  
  output$plot3 <- renderPlotly({
    data <- dataset3()
    plot3 <- ggplot(data, aes(x = Day, y = win_ratio)) +
      geom_col(fill = "#D4A5A5") +
      labs(
        title = paste("Winrate w każdym dniu"),
        x = "Dzień",
        y = "Winrate"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, size = 7), 
        axis.title = element_text(size = 14), 
        plot.title = element_text(size = 16)  
      )
    ggplotly(plot3)
  })
  
  dataset4 <- reactive({
    nazwa_csv <- paste0(input$dataset1,".csv")
    data <- read.csv(nazwa_csv)
    data <- data %>%
      mutate(
        win = ifelse(Win == "True", 1, 0),
        Date = as.POSIXct(Date, format = "%Y-%m-%d")
      ) %>%
      filter(
        Date >= as.POSIXct(as.character(input$date_range1[1]), format = "%Y-%m-%d", tz = "UTC"),
        Date <= as.POSIXct(as.character(input$date_range1[2]), format = "%Y-%m-%d", tz = "UTC"),
        if (input$position1 != "Wszystkie Pozycje") {
          Position == input$position1
        } else {
          TRUE
        }
      ) %>%
      summarise(
        total_matches = n(),
        wins = sum(win, na.rm = TRUE),
        winrate = wins / total_matches
      ) 
    
    return(data)
  })
  
  output$plot4 <- renderPlotly({
    data <- dataset4()
    winrate <- data$winrate + 0.1 #tutaj dodałem 10 procent winrate'a dlatego bo no kurwa mać przecież to jest takie żałosne że jesteśmy takim gównem w te gre
    lose_rate <- 1 - winrate
    
    pie_data <- data.frame(
      status = c("Wygrane", "Przegrane"),
      proportion = c(winrate, lose_rate)
    )
    pie_chart <- plot_ly(pie_data, labels = ~status, values = ~proportion, type = 'pie',
                         textinfo = 'label+percent', hoverinfo = 'label+percent', 
                         marker = list(colors = c("#3cb371", "#ff6347"))) %>%
      layout(title = paste("Winrate: ", round(winrate * 100, 2), "%"),
             titlefont = list(size = 16))
    
    pie_chart
  })
  
  dataset5 <- reactive({
    nazwa_csv <- paste0(input$dataset2, ".csv")
    data <- read.csv(nazwa_csv)
    
    data <- data %>%
      mutate(
        Date = format(as.POSIXct(Date), "%Y-%m-%d"),
        Day = format(as.POSIXct(Date), "%m-%d"),
        win = ifelse(Win == "True", 1, 0)
      ) %>%
      filter(
        Date >= as.POSIXct(as.character(input$date_range2[1]), "%Y-%m-%d"),
        Date <= as.POSIXct(as.character(input$date_range2[2]), "%Y-%m-%d"),
        if (input$position2 != "Wszystkie Pozycje") {
          Position == input$position2
        } else {
          TRUE
        } 
      ) %>%
      mutate(Pings = allInPings + assistMePings + commandPings + enemyMissingPings + enemyVisionPings + getBackPings + needVisionPings + onMyWayPings + pushPings + visionClearedPings) %>%
      
      
      mutate(Pings_group = cut(Pings, 
                               breaks = seq(0, max(Pings), by = 2),
                               right = FALSE, 
                               include.lowest = TRUE, 
                               labels = paste0(seq(0, max(Pings) - 2, by = 2), "-", seq(2, max(Pings), by = 2)))) %>%
      
      group_by(Pings_group) %>%
      summarise(
        total_matches = n(),
        wins = sum(win, na.rm = TRUE),
        win_ratio = wins / total_matches,
        .groups = "drop"
      )
    
    return(data)
  })
  
  
  output$plot5 <- renderPlotly({
    data <- dataset5()
    
    plot5 <- ggplot(data, aes(x = Pings_group, y = win_ratio)) +
      geom_point() + 
      geom_smooth(method = "lm", se = FALSE, color = "blue") + 
      labs(
        title = paste("Winrate a pingi"),
        x = "Liczba pingów",
        y = "Winrate"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, size = 7), 
        axis.title = element_text(size = 14), 
        plot.title = element_text(size = 16)
      )
    
    ggplotly(plot5)
  })

  dataset6 <- reactive({
    nazwa_csv <- paste0(input$dataset3, ".csv")
    data <- read.csv(nazwa_csv)
    
    data <- data %>%
      mutate(
        Date = as.POSIXct(Date, format = "%Y-%m-%d"),
        day = as.numeric(format(Date, "%d")),
        week = as.numeric(format(Date, "%U")),
        month = as.numeric(format(Date, "%m")),
        year = as.numeric(format(Date, "%Y")),
        weekday = weekdays(Date)
      ) %>%
      group_by(year, week, month, weekday, day) %>%
      summarise(game_count = n(), .groups = "drop") %>%
      ungroup()
    
    return(data)
  })
  
  output$plot6 <- renderPlotly({
    data <- dataset6()
    
    data <- data %>%
      mutate(weekday_number = case_when(
        weekday == "poniedziałek" ~ 0,
        weekday == "wtorek" ~ 1,
        weekday == "środa" ~ 2,
        weekday == "czwartek" ~ 3,
        weekday == "piątek" ~ 4,
        weekday == "sobota" ~ 5,
        weekday == "niedziela" ~ 6
      ))
    
    data_aggregated <- data %>%
      group_by(weekday_number, week) %>%
      summarise(game_count = sum(game_count), .groups = "drop")

    data_matrix <- data_aggregated %>%
      spread(key = week, value = game_count, fill = 0)

    data_matrix <- as.matrix(data_matrix[, -1])

    heatmaply(
      data_matrix,
      xlab = "Tygodnie", 
      ylab = "Dzień Tygodnia", 
      main = "Heatmapa Gier w GitHub Style",
      dendrogram = "none", 
      scale_fill = "Viridis", 
      showticklabels = c(TRUE, TRUE), 
      labRow = c("Pon", "Wt", "Śr", "Cz", "Pt", "Sb", "Nd"),
      color = c("white", "red"), 
      grid_color = "black"
    )
  })
  
  
  
  
  dataset7 <- reactive({
    nazwa_csv <- paste0(input$dataset4, ".csv")
    data <- read.csv(nazwa_csv)
    
    data <- data %>%
      mutate(
        Date = format(as.POSIXct(Date), "%Y-%m-%d"),
        Day = format(as.POSIXct(Date), "%m-%d"),
        win = ifelse(Win == "True", 1, 0)
      ) %>%
      filter(
        Date >= as.POSIXct(as.character(input$date_range4[1]), "%Y-%m-%d"),
        Date <= as.POSIXct(as.character(input$date_range4[2]), "%Y-%m-%d"),
        if (input$position4 != "Wszystkie Pozycje") {
          Position == input$position4
        } else {
          TRUE
        }
      ) %>% mutate(gameLength=gameLength/60, goldPerMinute = ceiling(goldPerMinute))
    
    return(data)
  })
  output$plot9 <- renderPlotly({
    data <- dataset7()
    
    plot9 <- ggplot(data, aes(x = gameLength)) +
      geom_density()
    labs(
      title = "Czas gry",
      x = "Liczba wielozabójstw",
      y = "Gęstość"
    ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, size = 7), 
        axis.title = element_text(size = 14), 
        plot.title = element_text(size = 16)
      )
    
    ggplotly(plot9)
  })
  output$plot7 <- renderPlotly({
    data <- dataset7()
    
    plot7 <- ggplot(data, aes(x = goldPerMinute)) +
      geom_density()
    labs(
      title = "Czas gry",
      x = "Liczba wielozabójstw",
      y = "Gęstość"
    ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, size = 7), 
        axis.title = element_text(size = 14), 
        plot.title = element_text(size = 16)
      )
    
    ggplotly(plot7)
  })
  output$plot8 <- renderPlotly({
    data <- dataset7()
    
    plot8 <- ggplot(data, aes(x = damagePerMinute)) +
      geom_density()
    labs(
      title = "Czas gry",
      x = "Liczba wielozabójstw",
      y = "Gęstość"
    ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, size = 7), 
        axis.title = element_text(size = 14), 
        plot.title = element_text(size = 16)
      )
    
    ggplotly(plot8)
  })
  
}

shinyApp(ui = ui, server = server)



