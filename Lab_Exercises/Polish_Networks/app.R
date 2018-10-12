#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
require(lubridate)
require(dplyr)
require(ggplot2)
require(igraph)
require(ggraph)
require(plotly)

IA_mem <- read.csv("https://raw.githubusercontent.com/kbodwin/Polish-Social-Networks/master/IA_member_dates.csv?token=AVHCwQkL4vqxZ5_elD8hLyuNmN_Ls-djks5bxZ4wwA%3D%3D")

inst_names <- unique(as.character(IA_mem$IA_new))

plot_for_date <- function(date, IA_highlight){
  
  my_graph <- IA_mem %>% 
    filter(ymd(Start_Date) <= dmy(date), ymd(End_Date) >= dmy(date)) %>%
    select(IA, Member_ID) %>%
    group_by(IA) %>%
    table() %>%
    as.matrix %>%
    tcrossprod() %>%
    graph.adjacency(weighted = TRUE)
  
  
  # my_graph %>% ggraph(layout = "kk") + 
  #   geom_edge_link() + 
  #   geom_node_point(size = 1) +
  #   ggtitle(date) +
  #   theme_graph()
  
  L <- create_layout(my_graph, layout = "kk")
  L$name = as.character(L$name)
  Xn <- L$x
  Yn <- L$y
  es <- as.data.frame(get.edgelist(my_graph))
  n_node <- length(L$name)
  n_edge <- nrow(es)
  
  node_cols <- rep("#619CFF", n_node)
  edge_cols <- rep("black", n_edge)
  
  hl <- L$name == IA_highlight
  if(sum(hl) > 0){
    hl_idx = which(hl)
    node_cols[hl_idx] = "red"
  } 
  
  hl <- es$V1 == IA_highlight | es$V2 == IA_highlight
  if(sum(hl) > 0){
    es <- es[c(which(!hl), which(hl)),]
    edge_cols[(n_edge - sum(hl)):(n_edge)] = "red"
  } 
  

  
  network <- plot_ly(x = ~Xn, y = ~Yn, type = "scatter", mode = "markers", 
                     marker = list(color = node_cols),
                     text = L$name, hoverinfo = "text",
                     width = 700, height = 600)
  
  axis <- list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)
  
  edge_shapes <- list()
  
  
  if(sum(hl) > 0){

  }
  
  for(i in 1:length(es$V1)) {
    v0_idx <- which(L$name == es[i,]$V1)
    v1_idx <- which(L$name == es[i,]$V2)
    
    edge_shape = list(
      type = "line",
      line = list(color = edge_cols[i], width = 1),
      x0 = Xn[v0_idx],
      y0 = Yn[v0_idx],
      x1 = Xn[v1_idx],
      y1 = Yn[v1_idx]
    )
    
    edge_shapes[[i]] <- edge_shape
  }

  
  layout(
    network,
    title = date,
    shapes = edge_shapes,
    xaxis = axis,
    yaxis = axis
  )
  
  
}

# Define UI for application that draws a histogram
ui <- pageWithSidebar(
  
  headerPanel("Connections between Polish institutions"),
  
  sidebarPanel(
    radioButtons('Day', 'Day',
                 choices = c(
                   "Start of Month" = 01,
                   "Mid Month" = 15)
    ),
      sliderInput('Month', 'Month', 1,
                       min = 1, max = 12),
           sliderInput('Year', 'Year', 1970,
                       min = 1950, max = 1989),
      selectInput('IA_highlight', 'Highlight an institution', 
                  choices = c("None", inst_names)
      )
  ),

    mainPanel(
      plotlyOutput('plot1')
    )

)


server <- function(input, output) {
  
  output$plot1 <- renderPlotly({
    plot_for_date(paste(input$Day, input$Month, input$Year, sep = "-"), input$IA_highlight)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

