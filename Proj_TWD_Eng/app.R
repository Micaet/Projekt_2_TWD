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
  
choose_colour2 <- function(selected_players) {
  colors <- c(
    Player1 = "#005b92",
    Player2 = "#be1e37",
    ProPlayer = "#4F6F49"
  )
  return(colors[selected_players])
}

filter_data <- function(df, date_range, position_filter) {
  if (is.null(position_filter) || length(position_filter) == 0) {
    position_filter <- "All"
  }
  df <- df %>%
    filter(
      Date >= as.POSIXct(date_range[1], format = "%Y-%m-%d", tz = "UTC"),
      Date <= as.POSIXct(date_range[2], format = "%Y-%m-%d", tz = "UTC"),
      if (position_filter != "All") Position == position_filter else TRUE
    )
  
  return(df)
}


add_custom_theme <- function(plot, x_label = NULL, y_label = NULL, plot_title = NULL, 
                             color = "#c89b3c", angle = 45, if_legend = F) {
  #add_legend <- ifelse(legend, element_text(size = 12, colour = color), element_blank())
  plot <- plot + 
    theme_minimal() + 
    theme(
      axis.text.x = element_text(angle = angle, size = 7, colour = color, family = "Spiegel"), 
      axis.title = element_text(size = 14, colour = color, family = "Spiegel"), 
      plot.title = element_text(hjust = 0.5, size = 16, colour = color, family = "Spiegel")) +
    {if (if_legend == T) {theme(legend.title = element_text(size = 10, colour = color, family = "Spiegel"),
      legend.text = element_text(size = 9, colour = color, family = "Spiegel")
      #legend.key.size = unit(1, 'cm')
      )}}
  
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
         font = list(color = main_color, family = 'Spiegel'),
         legend = list(bgcolor = 'rgba(17,1d,24,0)')) |>
    config(displayModeBar = FALSE)
  return(plot)
  
  
}

sliders_select_input <- function(input_number) {
  fluidRow(
    tags$div(
      class = "slider-custom",
      sliderInput(
        inputId = paste0("date_range", input_number),
        label = "Choose date range:",
        min = as.Date("2024-09-01"),
        max = as.Date("2024-12-30"),
        value = c(as.Date("2024-09-01"), as.Date("2024-12-30")),
        timeFormat = "%d-%m-%Y"
      )
    ),
    selectInput(
      paste0("dataset", input_number),
      "Choose player:",
      choices = c("Player1", "Player2", "Proplayer"),
      selected = "Player1"
    ),
    uiOutput(paste0("position_ui", input_number))
  )
}


apply_spinner <- function(plot_name, spinner_type = 1, colour = "#f0e6d2", height = "400px"){
  shinycssloaders::withSpinner(plotlyOutput(plot_name, height = height),
                               image = list(src = "load01.gif"),
                               caption = "PLEASE WAIT",
                               #type = getOption("spinner.type", default = spinner_type),
                               color = getOption("spinner.color", default = colour),
                               size = getOption("spinner.size", default = 0.75)
                               )
}


place_holder <- column(12, tags$div(class = 'placeholder'))

add_text_decorator <- function(text_before = NULL, text_after = NULL, decorator){
  if(decorator == 'large') {
    col <- column(12, align = 'center', 
                  tags$div(class = 'text-img', text_before , tags$img(class = "dec-l", src = "dec_l.png"), text_after))
  } else if (decorator == 'small'){
    col <- column(12, align = 'center', 
                  tags$div(class = 'text-img', text_before , tags$img(class = "dec-s", src = "dec_s.png"), text_after))
  }
  x <- list(
  place_holder, col, place_holder)
}




ui <- navbarPage(
  title = tags$div(class = "app-title", span(img(src = "favicon.png")), 'eague of Stats'), #titlePanel("League of Stats"),
    #header =  tags$head(  
    #),
  
  #tabsetPanel(
    tabPanel("Win rate",
             tags$link(rel = "icon", href = "favicon.png"),
             tags$div(class = "slider-custom"),
             tags$div(class = "custom-checkbox"),
             tags$link(type="text/css", rel = "stylesheet", href = "fonts.css"),
             tags$link(type="text/css", rel = "stylesheet", href = "styles.css"),
             
             add_text_decorator(paste(rep("Some description", 12), collapse = " "), decorator = 'large'),

             fluidRow(
               column(3, align = "center",
                      sliders_select_input(1)
               ),
             
               column(9,
                      apply_spinner("ScatterPlotPings") # '#73ea13')
               )
             ),
             
             add_text_decorator(paste(rep("Some description", 12), collapse = " "), decorator = 'small'),
              
             fluidRow(
               column(9,
                      apply_spinner("BarPlotWinRate") # 'blue')
                      ),
             
               column(3, 
                      apply_spinner("PieChartWinRate") # 'orange')
                      )
             )
    ),
    
    tabPanel("Number of games",
             add_text_decorator(paste(rep("Some description", 12), collapse = " "), decorator = 'large'),
             
             fluidRow(
               column(3, align = "center",
                      sliders_select_input(2)
               ),
               column(9, apply_spinner("BarPlotChampion")),
             ),

             add_text_decorator(paste(rep("Some description", 12), collapse = " "), decorator = 'small'),

             fluidRow(
               column(12, apply_spinner("BarPlotGames"))
             )
    ),
    tabPanel("Detailed stats",
             add_text_decorator(paste(rep("Some description", 12), collapse = " "), decorator = 'large'),
             
             fluidRow(
               column(6, align = "center",
                      tags$div(
                        class = "custom-checkbox",
                        checkboxGroupInput(
                          inputId = "players",
                          label = "Select Players:",
                          choices = c("Player1", "Player2", "ProPlayer"),
                          selected = c("Player1")
                        )
                      )
               ),
               column(6, align = "center",
                      selectInput(
                        inputId = "plotChoice",
                        label = "Choose density plot:",
                        choices = c("Game Duration", "Damage per minute", "Gold per minute"),
                        selected = "DensityDuration"
                      )
               )
             ),
             
             fluidRow(
              column(12,
                    uiOutput("dynamicPlot") # Dynamic UI for the selected plot
             )
           ),
           
             add_text_decorator(paste(rep("Some description", 12), collapse = " "), decorator = 'small'),
             
             fluidRow(
              column(10, apply_spinner("Heatmap", height = "600px")
                    ),
              column(2, align = "center",
                    selectInput(
                      "dataset3",
                      "Choose player:",
                      choices = c("Player1", "Player2", "Proplayer"),
                      selected = "Player1"
                    )
                  )
             )
    ),
  tabPanel("To be done",
           add_text_decorator(paste(rep("Some description", 12), collapse = " "), decorator = 'large'),
           
           # fluidRow(
           #   column(6, align = "center",
           #          tags$div(
           #            class = "custom-checkbox",
           #            checkboxGroupInput(
           #              inputId = "players",
           #              label = "Select Players:",
           #              choices = c("Player1", "Player2", "ProPlayer"),
           #              selected = c("Player1")
           #            )
           #          )
           #     ),
           #   column(6, align = "center",
           #          selectInput(
           #            inputId = "plotChoice",
           #            label = "Choose density plot:",
           #            choices = c("Game Duration", "Damage per minute", "Gold per minute"),
           #            selected = "DensityDuration"
           #          )
           #   )
           #         ),
           # fluidRow(
           #   column(12,
           #          uiOutput("dynamicPlot") # Dynamic UI for the selected plot
           #   )
           # )
  )
  ,#"#c89b3c", bg_color = "#f0e6d2"
  footer = shiny::HTML("
                <footer class='text-center text-sm-start' style='width:100%;'>
                <hr>
                <p class='text-center' style='font-size:26px; color:#f7bb45; font-family:Beaufort;'>
                  About project
                </p>
                <p class='text-center' style='font-size:22px; color:#c89b3c; font-family:Beaufort;'>
                  Authors: MB, RC, MS
                </p>
                <p class='text-left' style='font-size:20px; color:#f0e6d2; font-family:Beaufort; margin:10px;'>
                  Some description Some description Some description Some description Some description
                  Some description Some description Some description Some description Some description 
                  Some description Some description Some description Some description Some description
                  Some description Some description Some description Some description Some description
                </p>
                <p class='text-center' style='font-size:16px; color:#c89b3c; font-family:Beaufort;'>
                  Source of data:
                  <a class='text-dark' href='https://developer.riotgames.com/'>RiotGames API</a>
                </p>
                </footer>
              "),
  #header = tags$head(tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css"))
    
  #)
)


server <- function(input, output,session) {
### Kod do wyświietlania ograniczonego dla ProPlayer
  observe({
    for (i in 1:2) {
      dataset_input <- paste0("dataset", i)
      position_input <- paste0("position", i)
      if (!is.null(input[[dataset_input]])) {
        choices <- if (input[[dataset_input]] == "Proplayer") {
          c("All", "BOTTOM", "UTILITY")
        } else {
          c("All", "TOP", "JUNGLE", "MIDDLE", "BOTTOM", "UTILITY")
        }
        updateSelectInput(
          session,
          inputId = position_input,
          choices = choices,
          selected = "All"
        )
      }
    }
  })
  observe({
    lapply(1:2, function(i) { 
      output[[paste0("position_ui", i)]] <- renderUI({
        selectInput(
          paste0("position", i),
          "Choose position:",
          choices = c("All", "TOP", "JUNGLE", "MIDDLE", "BOTTOM", "UTILITY"),
          selected = "All"
        )
      })
    })
  })

#### Wyświetlanie 1 z trzech
  output$dynamicPlot <- renderUI({
    selected_plot <- input$plotChoice
    
    if (selected_plot == "Game Duration") {
      apply_spinner("DensityDuration")
    } else if (selected_plot == "Damage per minute") {
      apply_spinner("DensityDamage")
    } else if (selected_plot == "Gold per minute") {
      apply_spinner("DensityGold")
    }
  }) |>
    bindCache(input$plotChoice)
  ###
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
    plot <- ggplot(data |> rename(`Number of games` = n), aes(x = Date, y = `Number of games`)) +
      geom_col(fill = choose_colour(input$dataset2)) +
      coord_cartesian(ylim = c(0, 16))
    plot<- add_custom_theme(plot,"Date","Number of games",
                           paste("Games per day       ", "             Total number of games:", total_games))
    
    plot <- ggplotly(plot)
    plot <- plot |>
      layout(title = list(
        text = paste0('Games per day',
                      '<br>',
                      '<sup>',
                      'Total number of games: ', total_games ,'</sup>')))
    change_plotly_labels(plot)
  }) |>
    bindCache(input$dataset2, input$date_range2, input$position2)
  
  BarPlotChampionData <- reactive({
    nazwa_csv <- paste0(input$dataset2, ".csv")
    data <- read.csv(nazwa_csv)
    data <- data %>% 
      filter_data(input$date_range2, input$position2) %>%
      count(Champion, Position) %>% 
      filter(n >2)%>%
      filter(!(Champion == "Vladimir") )
    return(data)
  })
  
  output$BarPlotChampion <- renderPlotly({
    data <- BarPlotChampionData() %>% 
      select(Champion, Position, n) %>% 
      arrange(n)
    
    position_colors <- c(
      "BOTTOM" = "lightblue", 
      "TOP" = "#f0e6d2", 
      "JUNGLE" = "#2ca02c", 
      "UTILITY" = "#ff7f0e", 
      "MIDDLE" = "#9467bd", 
      "MIXED" = "#8c564b"
    )
    
    data <- data %>% 
      mutate(Position = ifelse(is.na(Position) | Position == "", "MIXED", Position)) %>%
      mutate(Color = ifelse(Position %in% names(position_colors), 
                            position_colors[Position], 
                            position_colors["MIXED"]))
    plot2 <- ggplot(data, aes(x = fct_inorder(Champion), y = n, fill = Position, 
                              text = paste("Champion:", Champion, 
                                           "<br>Number of games:", n, 
                                           "<br>Position:", Position))) +
      geom_col() +
      scale_fill_manual(values = position_colors, name = "Position") +
      coord_flip(ylim = c(0, 43))
    
    plot2 <- add_custom_theme(
      plot2, 
      "Champions", 
      "Number of games", 
      "Games on champion", 
      angle = 0
    ) +theme(legend.text = element_text(color = "#c89b3c"),legend.title = element_text(color = "#c89b3c")) 
    
    plot <- ggplotly(plot2, tooltip = "text")
    
    change_plotly_labels(plot)
  }) |>
    bindCache(input$dataset2, input$date_range2, input$position2)
  
  
  
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
    plot3 <- ggplot(data, aes(x = Day, y = win_ratio,
    text = paste("Date:", as.character(Day), "<br>Win rate:", round(win_ratio*100,2),"%")))+
    geom_col(fill = choose_colour(input$dataset1)) +
      coord_cartesian(ylim = c(0, 1))
    plot3<- add_custom_theme(plot3,"Day","Win rate","Win rate in each day")
    plot <- ggplotly(plot3,tooltip = 
                       "text")
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
  
  output$PieChartWinRate <- renderPlotly({ #skala na y % 
    data <- PieChartWinRateData()
    winrate <- data$winrate + 0.1 
    lose_rate <- 1 - winrate
    
    pie_data <- data.frame(
      status = c("Wins", "Loses"),
      proportion = c(winrate, lose_rate)
    )
    pie_chart <- plot_ly(pie_data, labels = ~status, values = ~proportion, type = 'pie',
                         textinfo = 'label+percent', hoverinfo = 'label+percent', 
                         marker = list(colors = c("#3cb371", "#ff6347"))) %>% #jasny szary, złoty
      layout(
        title = list(
          text = paste("Win rate: ", round(winrate * 100, 2), "%"),
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
    
    plot5 <- ggplot(data, aes(x = Pings_group, y = win_ratio,
                              text=paste("Number of pings:", Pings_group, "<br>Win rate:", round(win_ratio*100,2),"%"))) +
    geom_point(color = choose_colour(input$dataset1)) +
      coord_cartesian(ylim = c(0, 1))
    plot5<- add_custom_theme(plot5,"Number of pings","Win rate","Win rate by number of pings")
    
    plot <- ggplotly(plot5,tooltip = "text")
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
  }) |>
    bindCache(input$dataset3, input$date_range3, input$position3)
  
  
  DensityPlotsData <- reactive({
    selected_players <- input$players
    if (length(selected_players) == 0) {
      return(data.frame())
    }
    
    player_data <- lapply(selected_players, function(player) {
      file_name <- paste0(player, ".csv") 
      data  <- read.csv(file_name)
     data <- data%>%  mutate(riotIdTagline = as.character(riotIdTagline))%>%
        mutate(
          Player = player,
          Date = format(as.POSIXct(Date), "%Y-%m-%d"),
          Day = format(as.POSIXct(Date), "%m-%d"),
          win = ifelse(Win == "True", 1, 0),
          gameLength = gameLength / 60,
          goldPerMinute = ceiling(goldPerMinute)
        )
      return(data)
    })
    
    combined_data <- bind_rows(player_data)
    
    return(combined_data)
  })
  output$DensityDuration <- renderPlotly({
    data <- DensityPlotsData()
    if (nrow(data) == 0) {
      return(NULL)
    }
  
    
    plot9 <- ggplot(data, aes(x = gameLength, color = Player, fill = Player)) +
      geom_density(alpha = 0.4) +  
      scale_fill_manual(values = choose_colour2(input$players)) +
    #scale_color_manual(values = choose_colour2(input$players))+
      coord_cartesian(xlim = c(0, 50), ylim = c(0, 0.07))
    plot9 <- add_custom_theme(plot9, x = "Game Duration", y = "Density", "Game Duration Density",
                              angle = 0, if_legend = T)
    plot <- ggplotly(plot9, tooltip = c("x", "color"))
    change_plotly_labels(plot)
  }) 
  
  output$DensityGold <- renderPlotly({
    data <- DensityPlotsData()
    
    if (nrow(data) == 0) {
      return(NULL)
    }
    
    plot7 <- ggplot(data, aes(x = goldPerMinute, color = Player, fill = Player)) +
      geom_density(alpha = 0.4) + 
      scale_fill_manual(values = choose_colour2(input$players)) +
      coord_cartesian(xlim = c(0, 1000), ylim = c(0, 0.008))
      #scale_color_manual(values = choose_colour2(input$players)) +
    plot7 <- add_custom_theme(plot7, x = "Gold Per Minute", y = "Density", "Gold Per Minute Density", 
                              angle = 0, if_legend = T)
   
    plot <- ggplotly(plot7,tooltip = c("x", "color"))
    change_plotly_labels(plot)
  })
  
  output$DensityDamage <- renderPlotly({
    data <- DensityPlotsData()
    
    if (nrow(data) == 0) {
      return(NULL)
    }
    
    plot8 <- ggplot(data, aes(x = damagePerMinute, color = Player, fill = Player)) +
      geom_density(alpha = 0.4) + 
      scale_fill_manual(values = choose_colour2(input$players)) +
      coord_cartesian(xlim = c(0, 3000), ylim = c(0, 0.0025))
      #scale_color_manual(values = choose_colour2(input$players)) +
    plot8 <- add_custom_theme(plot8, x = "Damage Per Minute", y = "Density", "Damage Per Minute Density", 
                              angle = 0, if_legend = T)
    
    plot <- ggplotly(plot8,tooltip = c("x", "color"))
    change_plotly_labels(plot)
    
  })
}

shinyApp(ui = ui, server = server)

