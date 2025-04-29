library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(RColorBrewer)
library(plotly)

keys_df <- read.csv("https://uwmadison.box.com/shared/static/f8iu3nhu0hyeyfsenf2ga51jmsr1z2e6.csv")
flights_dfs <- read.csv("https://uwmadison.box.com/shared/static/un7buncwa4grbz1osv3jbxhbolq4gptu.csv")

flights_dfs$OP_CARRIER <- as.character(flights_dfs$OP_CARRIER)
keys_df$IATA <- as.character(keys_df$IATA)

flights_dfs <- flights_dfs %>%
  left_join(keys_df, by = c("OP_CARRIER" = "IATA")) %>%
  mutate(Airline_Name = ifelse(is.na(Name), OP_CARRIER, Name))

ui <- fluidPage(
  theme = bslib::bs_theme(
    bootswatch = "cosmo",
    primary = "#0d6efd",
    secondary = "#6c757d"
  ),
  titlePanel(
    div(
      icon("plane-departure", style = "font-size: 24px; color: #0d6efd;"),
      h3("Airline Delay", align = "center", style = "color: #0d6efd; font-weight: bold; display: inline;")
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      tags$h4("Delay Type Legend", style = "color: #0d6efd; font-weight: bold;"),
      tags$div(
        class = "legend-box",
        tags$ul(
          tags$li(tags$span(icon("wrench"), style = "color: #FF6F61;"), " Carrier Delays: Airline-controlled delays like maintenance."),
          tags$li(tags$span(icon("cloud"), style = "color: #76C7C0;"), " Weather Delays: Adverse weather conditions."),
          tags$li(tags$span(icon("route"), style = "color: #FFA07A;"), " NAS Delays: Air traffic management issues."),
          tags$li(tags$span(icon("shield-alt"), style = "color: #9370DB;"), " Security Delays: Caused by breaches, evacuations."),
          tags$li(tags$span(icon("plane-arrival"), style = "color: #FFD700;"), " Late Aircraft: Previous flights arriving late.")
        )
      ),
      width = 3
    ),
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Delay Analysis",
          tags$h4(icon("chart-bar"), " Total Delay Overview", style = "font-weight: bold; color: #0d6efd;"),
          fluidRow(
            column(
              width = 6,
              selectInput(
                "delayTypeAnalysis",
                "Select Delay Type:",
                choices = c("All", "Carrier", "Weather", "NAS", "Security", "Late Aircraft"),
                selected = "All"
              )
            ),
            column(
              width = 6,
              selectInput(
                "airlineFilterAnalysis",
                "Select Airline:",
                choices = c("All", unique(flights_dfs$Airline_Name)),
                selected = "All"
              )
            )
          ),
          plotlyOutput("airlineDelayPlot", height = "600px", width = "100%"),
          tags$h4(icon("chart-pie"), " Breakdown by Cause", style = "font-weight: bold; color: #0d6efd;"),
          plotlyOutput("delayCausePlot", height = "600px", width = "100%")
        )
      )
    )
  )
)


server <- function(input, output) {
  
  filteredDataAnalysis <- reactive({
    data <- flights_dfs %>%
      group_by(Airline_Name) %>%
      summarise(
        Carrier = sum(CARRIER_DELAY, na.rm = TRUE),
        Weather = sum(WEATHER_DELAY, na.rm = TRUE),
        NAS = sum(NAS_DELAY, na.rm = TRUE),
        Security = sum(SECURITY_DELAY, na.rm = TRUE),
        `Late Aircraft` = sum(LATE_AIRCRAFT_DELAY, na.rm = TRUE)
      ) %>%
      pivot_longer(
        cols = Carrier:`Late Aircraft`,
        names_to = "Delay_Type",
        values_to = "Total_Delay"
      ) %>%
      group_by(Airline_Name) %>%
      mutate(Total_Delay_Per_Airline = sum(Total_Delay, na.rm = TRUE))
    
    if (input$airlineFilterAnalysis != "All") {
      data <- data %>% filter(Airline_Name == input$airlineFilterAnalysis)
    }
    
    if (input$delayTypeAnalysis != "All") {
      data <- data %>% filter(Delay_Type == input$delayTypeAnalysis)
    }
    
    data
  })
  
  output$airlineDelayPlot <- renderPlotly({
    data <- filteredDataAnalysis()
    plot <- ggplot(data, aes(
      x = reorder(Airline_Name, -Total_Delay_Per_Airline),
      y = Total_Delay,
      fill = Delay_Type,
      text = paste(
        "Airline:", Airline_Name,
        "<br>Total Delay:", scales::comma(Total_Delay, accuracy = 1),
        "<br>Delay Type:", Delay_Type
      )
    )) +
      geom_bar(stat = "identity", position = "stack", width = 0.7, color = "white") +
      labs(
        title = "Total Delay by Type Across Airlines",
        x = "Airlines",
        y = "Total Delay (minutes)",
        fill = "Type of Delay"
      ) +
      scale_y_continuous(labels = scales::comma) +
      scale_fill_brewer(palette = "Set2") +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10, color = "#6c757d"),
        plot.title = element_text(face = "bold", size = 18, color = "#0d6efd"),
        legend.title = element_text(face = "bold", color = "#0d6efd"),
        panel.grid.major = element_line(color = "gray90")
      )
    ggplotly(plot, tooltip = "text")
  })
  
  output$delayCausePlot <- renderPlotly({
    data <- flights_dfs %>%
      summarise(
        Carrier = sum(CARRIER_DELAY, na.rm = TRUE),
        Weather = sum(WEATHER_DELAY, na.rm = TRUE),
        NAS = sum(NAS_DELAY, na.rm = TRUE),
        Security = sum(SECURITY_DELAY, na.rm = TRUE),
        `Late Aircraft` = sum(LATE_AIRCRAFT_DELAY, na.rm = TRUE)
      ) %>%
      pivot_longer(
        cols = everything(),
        names_to = "Delay_Type",
        values_to = "Total_Delay"
      )
    
    plot <- ggplot(data, aes(x = Delay_Type, y = Total_Delay, fill = Delay_Type)) +
      geom_bar(stat = "identity", position = "dodge", width = 0.6, color = "white") +
      geom_text(aes(label = scales::comma(Total_Delay)), vjust = -0.3, size = 4, color = "black") +
      labs(
        title = "Total Delay by Cause Across All Airlines",
        x = "Delay Cause",
        y = "Total Delay (minutes)"
      ) +
      scale_y_continuous(
        labels = scales::comma,
        breaks = c(0, 1e5, 1e6, 1e7, 2e7, 3e7, 4e7, 5e7, 6e7)  
      ) +
      scale_fill_brewer(palette = "Set1") +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x = element_text(size = 12, face = "bold", color = "#6c757d"),
        axis.text.y = element_text(size = 12, color = "#6c757d"),
        plot.title = element_text(face = "bold", size = 18, color = "#0d6efd"),
        panel.grid.major = element_line(color = "gray90")
      )
    
    ggplotly(plot, tooltip = c("x", "y", "fill"))
  })
}

shinyApp(ui = ui, server = server)


