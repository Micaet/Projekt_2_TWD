data <- read.csv("Bottom_matches.csv")

plot <- data %>% plot_ly(
  x = ~x,
  y = ~y,
  type = "scatter",
  mode = "markers",
  color = ~type,
  size = 3,
  height = 416,
  width = 500
) %>% layout(
  images = list(
    source = base64enc::dataURI(file = "./www/map11.png"),
    x = 0,
    y = 0,
    sizex = 1,
    sizey = 1,
    xref = "paper",
    yref = "paper",
    xanchor = "left",
    yanchor = "bottom",
    layer = "below"
  ),
  paper_bgcolor = 'rgba(0,0,0,0)',
  plot_bgcolor = 'rgba(0,0,0,0)',
  xaxis = list(
    title = '',
    showgrid = FALSE,
    showticklabels = FALSE,
    range(0, 14000),
    tickfont = list(color = 'rgba(0,0,0,0)'),
    linecolor = 'rgba(0,0,0,0)'
  ),
  yaxis = list(
    title = '',
    showgrid = FALSE,
    showticklabels = FALSE,
    range(0, 14000),
    tickfont = list(color = 'rgba(0,0,0,0)'),
    linecolor = 'rgba(0,0,0,0)'
  ),
  legend = list(
    font = list(
      color ="#c8aa6e"
    )
  )
) %>%
  config(displayModeBar = FALSE)
plot
