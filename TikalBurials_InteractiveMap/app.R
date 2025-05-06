# Prelims: Packages
library(shiny)
library(tidyverse)
library(sf)
library(terra)
library(spatstat.geom)
library(ggplot2)
library(viridis)
library(grid)
library(spatstat.utils)

# Define UI
ui <- fluidPage(
  
  titlePanel("Burial Density in Tikal: Interactive Map"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("filter_col", "Select Attribute:", choices = c("sex", "age")),
      selectInput("filter_val", "Select Category:", choices = NULL)
    ),
    mainPanel(
      plotOutput("densityMap"),
      tableOutput("filteredData")
    )
  )
)

# Define server logic 
server <- function(input, output, session) {
  
  
  # 1. Load Data
  ## 1.1. Burial Data:
  b <- read_csv("Data/clean_burial_data.csv", col_names = TRUE) %>%
    drop_na(x, y) #just in case
  
  ## 1.2. Tikal Map:
  t <- rast("Data/MapTikal.tif")
  t_extent <- ext(t)
  t_matrix <- as.raster(t[[1]])
  t_grob <- rasterGrob(t_matrix, width = unit(1, "npc"), height = unit(1, "npc"), interpolate = TRUE)
  
  
  # 2. Filtering Data
  ## 2.1 Use filter_col and filter_val as dropdown menus
  observeEvent(input$filter_col, {
    updateSelectInput(
      session,
      "filter_val",
      choices = sort(unique(b[[input$filter_col]]))
    )
  })
  
  ## 2.2. Reactive Object that filters data
  b_filtered <- reactive({
    ### 2.2.1 Make sure user selected both
    req(input$filter_col, input$filter_val)
    
    ### 2.2.2 Filter data
    b %>%
      filter(get(input$filter_col) == input$filter_val)
  })
  
  
  # 3. Table output to check filtered data
  output$filteredData <- renderTable({
    b_filtered()
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}   #END OF SERVER FUNCTION

# Run the application 
shinyApp(ui = ui, server = server)