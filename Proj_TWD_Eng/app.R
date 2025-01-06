library(shiny)
library(dplyr)
library(ggplot2)
library(stringi)
library(plotly)
library(heatmaply)
library(tidyr)
library(ggrepel)
library(shinycssloaders)
library(forcats)


choose_colour <- function(input) {
  c("#181d24","#f7bb45", "#c89b3c", "#f0e6d2", "#fc6a53", "#be1e37")
  ifelse(input == "Player1", "#005b92", 
         ifelse(input == "Player2", "#be1e37", "#4F6F49")) #    "#f7bb45", "#c89b3c" - naj "#f0e6d2" - tło
} 
  

filter_data <- function(df, date_range, position_filter) {
  
  df <- df %>%
    filter(
      Date >= as.POSIXct(date_range[1], format = "%Y-%m-%d", tz = "UTC"),
      Date <= as.POSIXct(date_range[2], format = "%Y-%m-%d", tz = "UTC"),
      if (position_filter != "All") {
        Position == position_filter
      } else {
        TRUE
      }
    )
  
  return(df)
}


add_custom_theme <- function(plot, x_label = NULL, y_label = NULL, plot_title = NULL, color = "#c89b3c", angle = 45) {
  plot <- plot + 
    theme_minimal() + 
    theme(
      axis.text.x = element_text(angle = angle, size = 7, colour = color), 
      axis.title = element_text(size = 14, colour = color), 
      plot.title = element_text(size = 16, colour = color)
    )
  if (!is.null(x_label)) {
    plot <- plot + xlab(x_label)
  }
  if (!is.null(y_label)) {
    plot <- plot + ylab(y_label)
  }
  if (!is.null(plot_title)) {
    plot <- plot + ggtitle(plot_title)
  }
  return(plot)
}

change_plotly_labels <- function(plot, main_color = "#c89b3c", bg_color = "#f0e6d2", alpha = 35){
  temp <- list(tickfont = list(color = bg_color),color = main_color, showgrid = T, 
               gridcolor = paste0(main_color,alpha), zeroline = F, showline = T)
  plot <- plot |> 
    layout(paper_bgcolor = "rgba(0,0,0,0)",
         plot_bgcolor = "rgba(0,0,0,0)",
         xaxis = temp, yaxis = temp,
         font = list(color = main_color),
         legend = list(bgcolor = 'rgba(17,1d,24,0)')) |>
    config(displayModeBar = FALSE)
  
}

sliders_select_input <- function(input_number){
x <-list(tags$div(class = "slider-custom", sliderInput(inputId = paste0("date_range", input_number),
                  label = "Choose date range:",
                  min = as.Date("2024-09-01"),
                  max = as.Date("2024-12-30"),
                  value = c(as.Date("2024-09-01"), as.Date("2024-12-30")),
                  timeFormat = "%d-%m-%Y")),
         selectInput(paste0("dataset", input_number),
                  "Choose player:",
                  choices = c("Player1", "Player2", "Proplayer"),
                  selected = "Player1"),
         selectInput(paste0("position", input_number),
                  "Choose position:",
                  choices = c("All", "TOP", "JUNGLE", "MIDDLE", "BOTTOM", "UTILITY"),
                  selected = "All"))
}

apply_spinner <- function(plot_name, spinner_type, colour = "#c89b3c", height = "400px"){
  shinycssloaders::withSpinner(plotlyOutput(plot_name, height = height),
                               type = getOption("spinner.type", default = spinner_type),
                               color = getOption("spinner.color", default = colour),
                               size = getOption("spinner.size", default = 1.5))
}


ui <- navbarPage(
  title = tags$div("League of Stats"), #titlePanel("League of Stats"),

  #tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #f0e6d2}")),
  header =  tags$head(
    # tags$style(HTML("
    #   .slider-custom .irs-bar {
    #     background-color: #f0e6d2;
    #     border-top-color: #f0e6d2;
    #     border-bottom-color: #f0e6d2;
    #   }
    #   .slider-custom .irs-from, .slider-custom .irs-to, .slider-custom .irs-single {
    #     background-color: #c89b3c;
    #   }
    #   .slider-custom .irs-handle {
    #     cursor: url('cursor-pointer.png'), auto;
    #     background-color: #c89b3c;
    #     border-color: #c89b3c;
    #   }
    #   .slider-custom .irs-grid-text {
    #   font-family: 'Beaufort'; color: #f0e6d2; bottom: 1px; z-index: 1;
    #   }
    #   
    #   .selector-custom .selectize-dropdown{
    #   font-family: 'Beaufort'; color: #f0e6d2; font-style: italic; font-weight: bold;
    #   }
    # ")),
    tags$div(class = "slider-custom"),
    tags$link(rel = "icon", href = "favicon.png"),
    tags$link(rel = "stylesheet", href = "fonts.css"),
    tags$link(rel = "stylesheet", href = "styles.css"),
  ),
  
  #tabsetPanel(
    tabPanel("Tab 1",

             fluidRow(
               column(3, align = "center",
                      sliders_select_input(1)
               ),
             
               column(9,
                      apply_spinner("ScatterPlotPings", 5) # '#73ea13')
               )
             ),
             fluidRow(
               column(9,
                      apply_spinner("BarPlotWinRate", 6) # 'blue')
                      ),
             
               column(3, 
                      apply_spinner("PieChartWinRate", 7) # 'orange')
                      )
             )
    ),
    
    tabPanel("Tab 2",
             fluidRow(
               column(3, align = "center",
                      sliders_select_input(2)
               ),
               column(9, plotlyOutput("BarPlotChampion", height = "400px")),
             ),

             fluidRow(
               column(12)
             ),

             fluidRow(
               column(12, plotlyOutput("BarPlotGames", height = "400px"))
             )
    ),
    tabPanel("Tab 3",
             fluidRow(
               column(6, align = "center",
                      sliders_select_input(3)
               )
             ),
             
             fluidRow(
               column(12, plotlyOutput("Heatmap", height = "600px"))
             )
    ),
    tabPanel("Tab 4",
             
             fluidRow(
               column(6, align = "center",
                      sliders_select_input(4)
               )
             ),
             fluidRow(
               column(12, plotlyOutput("DensityDuration", height = "400px"))
             ),
             fluidRow(
               column(12, plotlyOutput("DensityDamage", height = "400px"))
             ),
             fluidRow(
               column(12, plotlyOutput("DensityGold", height = "400px"))
             )
    ),#"#c89b3c", bg_color = "#f0e6d2"
  footer = shiny::HTML("
                <footer class='text-center text-sm-start' style='width:100%;'>
                <hr>
                <p class='text-center' style='font-size:26px;color:#f7bb45;font-family:Beaufort;'>
                  About project
                </p>
                <p class='text-center' style='font-size:20px;color:#c89b3c;font-family:Beaufort;'>
                  Authors: MB, RC, MS
                </p>
                <p class='text-center' style='font-size:16px;color:#f0e6d2;font-family:Beaufort;'>
                  Source of data:
                  <a class='text-dark' href='https://developer.riotgames.com/'>RiotGames API</a>
                </p>
                </footer>
              "),
  #header = tags$head(tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css"))
    
  #)
)


server <- function(input, output) {

  BarPlotGamesData <- reactive({
    nazwa_csv <- paste0(input$dataset2,".csv")
    data <- read.csv(nazwa_csv)
    data <- data %>% filter_data(input$date_range2,input$position2) %>%
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
  
  output$BarPlotGames <- renderPlotly({
    data <- BarPlotGamesData()
    total_games <- sum(data$n)
    plot <- ggplot(data, aes(x = Date, y = n)) +
      geom_col(fill = choose_colour(input$dataset2)) 
   plot<- add_custom_theme(plot,"Date","Number of games",
                           paste("Games per day       ", "             Total number of games:", total_games))
    
    plot <- ggplotly(plot)
    change_plotly_labels(plot)
  }) 
  

  BarPlotChampionData <- reactive({
    nazwa_csv <- paste0(input$dataset2,".csv")
    data <- read.csv(nazwa_csv)
    data <- data %>% filter_data(input$date_range2,input$position2) %>%
      count(Champion) %>% 
      filter(n > 2)
    return(data)
  })
  
  output$BarPlotChampion <- renderPlotly({
    data <- BarPlotChampionData() |> select(Champion, n) |> arrange(n)
    plot2 <- ggplot(data, aes(x = fct_inorder(Champion), y = n)) +
      geom_col(fill = choose_colour(input$dataset2)) 
      plot2<- add_custom_theme(plot2,"Champions","Number of games","Games on champion", angle = 0) +
        coord_flip()
    plot <- ggplotly(plot2)
    change_plotly_labels(plot)
  })
  
  BarPlotWinRateData <- reactive({
    nazwa_csv <- paste0(input$dataset1, ".csv")
    data <- read.csv(nazwa_csv)
    data <- data %>%
      mutate(
        Date = format(as.POSIXct(Date), "%Y-%m-%d"),
        Day = format(as.POSIXct(Date), "%m-%d"),
        win = ifelse(Win == "True", 1, 0)
      ) %>%
      filter_data(input$date_range1, input$position1) %>%
      group_by(Day) %>%
      summarise(
        total_matches = n(),
        wins = sum(win, na.rm = TRUE),
        win_ratio = wins / total_matches,
        .groups = "drop"
      )
    return(data)
  })
  
  output$BarPlotWinRate <- renderPlotly({
    data <- BarPlotWinRateData()
    plot3 <- ggplot(data, aes(x = Day, y = win_ratio)) +
      geom_col(fill = choose_colour(input$dataset1))
    plot3<- add_custom_theme(plot3,"Day","Winrate","Winrate in each day")
    plot <- ggplotly(plot3)
    change_plotly_labels(plot)
    
  }) |>
    bindCache(input$dataset1, input$date_range1, input$position1)
  
  PieChartWinRateData <- reactive({
    nazwa_csv <- paste0(input$dataset1,".csv")
    data <- read.csv(nazwa_csv)
    data <- data %>%
      mutate(
        win = ifelse(Win == "True", 1, 0),
        Date = as.POSIXct(Date, format = "%Y-%m-%d")
      ) %>% filter_data(input$date_range1,input$position1)%>%
      reframe(
        total_matches = n(),
        wins = sum(win, na.rm = TRUE),
        winrate = wins / total_matches
      ) 
    return(data)
  })
  
  output$PieChartWinRate <- renderPlotly({
    data <- PieChartWinRateData()
    winrate <- data$winrate + 0.1 
    lose_rate <- 1 - winrate
    
    pie_data <- data.frame(
      status = c("Wins", "Loses"),
      proportion = c(winrate, lose_rate)
    )
    pie_chart <- plot_ly(pie_data, labels = ~status, values = ~proportion, type = 'pie',
                         textinfo = 'label+percent', hoverinfo = 'label+percent', 
                         marker = list(colors = c("#3cb371", "#ff6347"))) %>%
      layout(
        title = list(
          text = paste("Winrate: ", round(winrate * 100, 2), "%"),
          font = list(size = 16)  
        ))
    
    change_plotly_labels(pie_chart)
  }) |>
    bindCache(input$dataset1, input$date_range1, input$position1)
  
  ScatterPlotPingsData <- reactive({
    nazwa_csv <- paste0(input$dataset1, ".csv")
    data <- read.csv(nazwa_csv)
    data <- data %>%
      mutate(
        Date = format(as.POSIXct(Date), "%Y-%m-%d", tz = "UTC"),
        Day = format(as.POSIXct(Date), "%m-%d", tz = "UTC"),
        win = ifelse(Win == "True", 1, 0)
      )%>% filter_data(input$date_range1,input$position1) %>%
      mutate(Pings = allInPings + assistMePings + commandPings + enemyMissingPings + 
               enemyVisionPings + getBackPings + needVisionPings + onMyWayPings + pushPings + visionClearedPings) %>%
      mutate(Pings_group = cut(Pings, 
                               breaks = seq(0, max(Pings), by = 2),
                               right = FALSE, 
                               include.lowest = TRUE, 
                               labels = paste0(seq(0, max(Pings) - 2, by = 2), "-", seq(2, max(Pings), by = 2)))) %>%
      
      group_by(Pings_group) %>%
      reframe(
        total_matches = n(),
        wins = sum(win, na.rm = TRUE),
        win_ratio = wins / total_matches,
        .groups = "drop"
      )
    return(data)
  })
  
  output$ScatterPlotPings <- renderPlotly({
    data <- ScatterPlotPingsData()
    
    plot5 <- ggplot(data, aes(x = Pings_group, y = win_ratio)) +
    geom_point(color = choose_colour(input$dataset1)) 
    plot5<- add_custom_theme(plot5,"Number of pings","Winrate","Winrate by number of pings")
    
    plot <- ggplotly(plot5)
    change_plotly_labels(plot)
  }) |>
    bindCache(input$dataset1, input$date_range1, input$position1)

  HeatmapData <- reactive({
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
  
  output$Heatmap <- renderPlotly({
    data <- HeatmapData()
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
    
    heat_map <- heatmaply(
      data_matrix,
      limits = c(0, 16),
      xlab = "Week",
      ylab = "Day of week",
      main = "Number of games heatmap",
      dendrogram = "none",
      #scale_fill = "Viridis",
      colors = list("#e9e6d2",choose_colour(input$dataset3)),
      showticklabels = c(TRUE, TRUE),
      labRow = c("Mon", "Tues", "Wen", "Thurs", "Fri", "Sat", "Sun"),
      #color = c("white", "red"),
      grid_color = "black",
      heatmap_layers = theme(
        legend.background = element_rect(fill = "#181d24"),
        legend.text = element_text(color = "#f0e6d2") #"#c89b3c")
      )
    )
    
    plot <- ggplotly(heat_map)
    change_plotly_labels(plot)
    
    # heatmap <- ggplot(data, aes(week, factor(weekday_number), fill = game_count)) +
    #   geom_tile() + 
    #   scale_fill_gradient(low = "#e9e6d2", high = choose_colour(input$dataset3), limits = c(0,16)) +
    #   scale_y_discrete(labels = c("Mon", "Tues", "Wen", "Thurs", "Fri", "Sat", "Sun"))
    # add_custom_theme(heatmap, "Week", "Day of week", "Number of games heatmap")
  })
  
  DensityPlotsData <- reactive({
    nazwa_csv <- paste0(input$dataset4, ".csv")
    data <- read.csv(nazwa_csv)
    data <- data %>%
      mutate(
        Date = format(as.POSIXct(Date), "%Y-%m-%d"),
        Day = format(as.POSIXct(Date), "%m-%d"),
        win = ifelse(Win == "True", 1, 0)
      ) %>% filter_data(input$date_range1,input$position1)%>%
      mutate(gameLength=gameLength/60, goldPerMinute = ceiling(goldPerMinute))
    
    return(data)
  })
  
  output$DensityDuration <- renderPlotly({
    data <- DensityPlotsData()
    plot9 <- ggplot(data, aes(x = gameLength)) +
      geom_density(fill = choose_colour(input$dataset4))
    plot9<- add_custom_theme(plot9,"Game duration","Density","Game Duration", angle = 0)
    plot <- ggplotly(plot9)
    change_plotly_labels(plot)
  })
  
  output$DensityGold<- renderPlotly({
    data <- DensityPlotsData()
    
    plot7 <- ggplot(data, aes(x = goldPerMinute)) +
      geom_density(fill = choose_colour(input$dataset4))
    plot7<- add_custom_theme(plot7,"Gold per minute","Density","Gold per minute", angle = 0)
    plot <- ggplotly(plot7)
    change_plotly_labels(plot)
  })
  
  output$DensityDamage <- renderPlotly({
    data <- DensityPlotsData()
    plot8 <- ggplot(data, aes(x = damagePerMinute)) +
      geom_density(fill = choose_colour(input$dataset4))
    plot8<- add_custom_theme(plot8,"Damage per minute","Density","Damage per minute", angle = 0)
    plot <- ggplotly(plot8)
    change_plotly_labels(plot)
  })
  
}

shinyApp(ui = ui, server = server)
