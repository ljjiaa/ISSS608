#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

pacman::p_load(shiny, tidyverse,fpp3, plotly, tsibbletalk, dygraphs, fpp3)


# Read the dataset (replace the path with the actual path to your data file)
weather <- read_rds("data/weather_imputed_11stations.rds")

# Convert to tsibble and group by 'Station'
weather_tsbl <- as_tsibble(weather, key = Station, index = Date)


# Define UI for application
ui <- fluidPage(
  tags$head(
    tags$style(HTML(".well {background-color: #FFFFFF;}")) # This sets the sidebar background to white
  ),
  titlePanel("Interactive Weather Visualization"),
  
  sidebarLayout(
    sidebarPanel(
      uiOutput("station_checkboxes"), # Dynamically create checkboxes
      radioButtons("variable", "Select Variable",
                  choices = c("Mean Temperature (°C)" = "Mean Temperature (°C)", 
                              "Minimum Temperature (°C)" = "Minimum Temperature (°C)", 
                              "Maximum Temperature (°C)" = "Maximum Temperature (°C)",
                              "Daily Rainfall Total (mm)" = "Daily Rainfall Total (mm)")),
      dateInput("startDate", "Start Date", value = "2021-01-01", min = "2021-01-01", max = "2023-12-31"),
      dateInput("endDate", "End Date", value = "2023-12-31", min = "2021-01-02", max = "2023-12-31"),
      radioButtons("Compare", "Compare Across",
                   choices = c("Day" = "Day", 
                               "Week" = "Week", 
                               "Month" = "Month")),
      sliderInput("lags", "Number of Lags", min = 1, max = 365, value = 20)
    ),
    
    mainPanel(
      plotlyOutput("weatherPlot"),
      fluidRow(
        column(6, plotOutput("acfPlot")),
        column(6, plotOutput("pacfPlot"))
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  output$station_checkboxes <- renderUI({
    stations <- unique(weather_tsbl$Station)
    checkboxGroupInput("selected_stations", "Select Stations:", choices = stations)
  })
  
  # Reactive expression to filter data based on selected stations
  filtered_data <- reactive({
    req(input$selected_stations) # Ensure some stations are selected
    weather_tsbl %>%
      filter(Station %in% input$selected_stations)
  })
    
    # Create the plot
  output$weatherPlot <- renderPlotly({
    df <- filtered_data() # Get the reactive dataset
    p <- plot_ly(df, x = ~Date, y = ~`Minimum Temperature (°C)`, type = 'scatter', mode = 'lines+markers',
                 color = ~Station, colors = RColorBrewer::brewer.pal(8, "Dark2")) %>%
      layout(title = "Minimum Temperature (°C) by Station",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Minimum Temperature (°C)")) %>%
    
     
      layout(xaxis = list(rangeslider = list(type = "date")))
    p
  })
  
  # ACF plot that reacts to the slider input
  output$acfPlot <- renderPlot({
    data <- filtered_data()
    acf_data <- as.numeric(data[[input$variable]])
    acf(acf_data, lag.max = input$lags, main = "ACF")
  })
  
  # PACF plot that reacts to the slider input
  output$pacfPlot <- renderPlot({
    data <- filtered_data()
    pacf_data <- as.numeric(data[[input$variable]])
    pacf(pacf_data, lag.max = input$lags, main = "PACF")
  })
}

# Run the Shiny app
shinyApp(ui, server)