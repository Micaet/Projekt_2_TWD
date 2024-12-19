library(shiny)
library(dplyr)
library(ggplot2)

ui <- fluidPage(
  titlePanel("League of Stats"),
  
  fluidRow(
    # Lewa kolumna: SliderInput i plot3
    column(6, align = "center",
           sliderInput(inputId = "date_range",
                       label = "Wybierz zakres dat:",
                       min = as.Date("2024-10-31"),
                       max = as.Date("2024-12-18"),
                       value = c(as.Date("2024-10-31"), as.Date("2024-12-18")),
                       timeFormat = "%d-%m-%Y"),
           plotOutput("plot3", height = "400px")
    ),
    
    # Prawa kolumna: SelectInput, plot1, plot2
    column(6, align = "center",
           selectInput("dataset",
                       "Wybierz gracza:",
                       choices = c("Gracz_1", "Gracz_2", "ProGracz_1", "ProGracz_2"),
                       selected = "Gracz_1"),
           fluidRow(
             column(12, plotOutput("plot1", height = "400px"))
           ),
           fluidRow(
             column(12, plotOutput("plot2", height = "400px"))
           )
    )
  )
)





server <- function(input, output) {
  
  dataset <- reactive({
    nazwa_csv <- paste0(input$dataset,".csv")
    data <- read.csv(nazwa_csv)
    
    data <- data %>%
      filter(
        Date >= as.POSIXct(as.character(input$date_range[1]), "%Y-%m-%d"),
        Date <= as.POSIXct(as.character(input$date_range[2]), "%Y-%m-%d")
      )%>%
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
  
  output$plot1 <- renderPlot({
    data <- dataset()
    ggplot(data, aes(x = Date, y = n)) +
      geom_col(fill = "#6D98BA") +
      labs(
        title = paste("Gry na dzień"),
        x = "Date",
        y = "Ile gier"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, size = 12), 
        axis.title = element_text(size = 14), 
        plot.title = element_text(size = 16)  
      )
  })
  
  dataset2 <- reactive({
    nazwa_csv <- paste0(input$dataset,".csv")
    data <- read.csv(nazwa_csv)
    
    data <- data %>%
      filter(
        Date >= as.POSIXct(as.character(input$date_range[1]), "%Y-%m-%d"),
        Date <= as.POSIXct(as.character(input$date_range[2]), "%Y-%m-%d")
      )%>%
      count(Champion) %>% 
      filter(n > 1)
    return(data)
  })
  
  output$plot2 <- renderPlot({
    data <- dataset2()
    ggplot(data, aes(x = Champion, y = n)) +
      geom_col(fill = "#A1C9A1") +
      labs(
        title = paste("Gry na danym championie"),
        x = "Championy",
        y = "Ilość gier"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, size = 12), 
        axis.title = element_text(size = 14), 
        plot.title = element_text(size = 16)  
      )
  })
  dataset3 <- reactive({
    nazwa_csv <- paste0(input$dataset, ".csv")
    data <- read.csv(nazwa_csv)
    
    data <- data %>%
      mutate(
        Date = format(as.POSIXct(Date), "%Y-%m-%d"),
        Day = format(as.POSIXct(Date), "%m-%d"),
        win = ifelse(Win == "True", 1, 0)
      ) %>%
      filter(
        Date >= as.POSIXct(as.character(input$date_range[1]), "%Y-%m-%d"),
        Date <= as.POSIXct(as.character(input$date_range[2]), "%Y-%m-%d")
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
  
  
  output$plot3 <- renderPlot({
    data <- dataset3()
    ggplot(data, aes(x = Day, y = win_ratio)) +
      geom_col(fill = "#D4A5A5") +
      labs(
        title = paste("Winrate w każdym dniu"),
        x = "Dzień",
        y = "Winrate"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, size = 12), 
        axis.title = element_text(size = 14), 
        plot.title = element_text(size = 16)  
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
