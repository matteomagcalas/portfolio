library(tidyverse)
library(lubridate)
library(shiny)

nba3pt <- read_csv("nba3pt.csv")
nba3pt$Season <- as.factor(nba3pt$Season)
nba3pt <- nba3pt %>%
  mutate(`3P%` = ifelse(`3PA` > 0, `3PM` / `3PA`, NA))

playoff_shape_rank <- c("None" = 16, "1st Round" = 17, "2nd Round" = 15, 
                        "Conf. Finals" = 3, "Finals" = 7, "Champions" = 8)

ui <- fluidPage(
  titlePanel("NBA 3-Pointers Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      # Select teams
      selectInput("team", "Select Team(s):", 
                  choices = unique(nba3pt$Team), 
                  selected = unique(nba3pt$Team)[1], multiple = TRUE),
      
      # Checkbox to select which graphs to show
      checkboxGroupInput("graphType", "Select Graph(s) to Display:",
                         choices = list("3PM" = "3PM", "3PA" = "3PA", "3P%" = "3P%"),
                         selected = "3PM")  # Pre-select 3PM by default
    ),
    
    mainPanel(
      # Add three plot outputs for the graphs
      plotOutput("plot3PM"),
      plotOutput("plot3PA"),
      plotOutput("plot3Ppct")
    )
  )
)

# Server
server <- function(input, output) {
  
  # Reactive expression for filtered data
  filteredData <- reactive({
    nba3pt %>%
      filter(Team %in% input$team) %>%
      mutate(shape_rank = playoff_shape_rank[`Playoff Result`])
  })
  
  # Render the 3PM graph if selected
  output$plot3PM <- renderPlot({
    if ("3PM" %in% input$graphType) {
      ggplot(filteredData(), aes(x = Season, y = `3PM`, group = Team, color = Team)) +
        geom_point(aes(size = 3, shape = `Playoff Result`)) +
        geom_line(size = 1) +
        scale_shape_manual(values = playoff_shape_rank) +
        labs(title = "3-Pointers Made by Team and Season", 
             x = "Season", 
             y = "3-Pointers Made (Avg)", 
             shape = "Playoff Result") +
        theme_minimal()
    }
  })
  
  # Render the 3PA graph if selected
  output$plot3PA <- renderPlot({
    if ("3PA" %in% input$graphType) {
      ggplot(filteredData(), aes(x = Season, y = `3PA`, group = Team, color = Team)) +
        geom_point(aes(size = 3, shape = `Playoff Result`)) +
        geom_line(size = 1) +
        scale_shape_manual(values = playoff_shape_rank) +
        labs(title = "3-Pointers Attempted by Team and Season", 
             x = "Season", 
             y = "3-Pointers Attempted (Avg)", 
             shape = "Playoff Result") +
        theme_minimal()
    }
  })
  
  # Render the 3P% graph if selected
  output$plot3Ppct <- renderPlot({
    if ("3P%" %in% input$graphType) {
      ggplot(filteredData(), aes(x = Season, y = `3P%`, group = Team, color = Team)) +
        geom_point(aes(size = 3, shape = `Playoff Result`)) +
        geom_line(size = 1) +
        scale_shape_manual(values = playoff_shape_rank) +
        labs(title = "3-Point Percentage by Team and Season", 
             x = "Season", 
             y = "3-Point Percentage", 
             shape = "Playoff Result") +
        theme_minimal()
    }
  })
}


shinyApp(ui, server)

