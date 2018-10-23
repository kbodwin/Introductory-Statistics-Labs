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
