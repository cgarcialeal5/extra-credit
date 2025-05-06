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
library(raster)

# Load functions saved in R files
r_files <- list.files("TikalBurials_InteractiveMap/R", pattern = "\\.R$", full.names = TRUE)
sapply(r_files, source)

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
  b <- read_csv("TikalBurials_InteractiveMap/Data/clean_burial_data.csv", col_names = TRUE) %>%
    drop_na(x, y) #just in case
  
  ## 1.2. Tikal Map:
  t <- rast("TikalBurials_InteractiveMap/Data/MapTikal.tif")
  t_extent <- ext(t)
  t_matrix <- as.raster(t[[1]])
  t_grob <- rasterGrob()
  
  
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
  
  # 3. Convert to sf object
  b_filtered_sf <- st_as_sf(
    b_filtered,
    coords = c("x", "y"),
    crs = 32616
  )
  
  # 4. Simulate new points (using function from creative vis project)
  b_simulated <- simulate_cloud(b_filtered_sf)
  
  # 5. Generate KDE (using the kde function from creative vis project)
  b_kde <- kde(
    b_simulated,
    ext_template = t,
    sigma = 300
  )
  
  # 6. Convert to data frame for plotting
  b_kde_df <- as.data.frame(b_kde)
  
  # 7. Filter out low densities
  b_kde_df$value[b_kde_df$value < 0.001] <- NA
  
  # 8. Create plot with Tikal basemap
  output$densityMap <- renderPlot({
    ## 8.1 Use Tikal map as a basemap
    terra::plot(t_matrix, col = grey(0.9), legend = FALSE)
    
    ## 8.2 Plot the kde
    plot.im(b_kde, add = TRUE, col = viridis(100))
    
  })
  
  # . Table output to check filtered data
  output$filteredData <- renderTable({
    b_filtered()
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
 
  
  
  
  
  
  
  
  
  
  
}   #END OF SERVER FUNCTION

# Run the application 
shinyApp(ui = ui, server = server)