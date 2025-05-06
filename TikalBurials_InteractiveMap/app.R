# Prelims: Packages
library(shiny)
library(tidyverse)
library(sf)
library(terra)
library(spatstat)
library(viridis)
library(grid)

# Define UI
ui <- fluidPage(
  
  titlePanel("Burial Density in Tikal: Interactive Map"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("filter_col", "Select Attribute:", choices = c(
        "quadrangle", "age", "sex", "time_period", "grave_type", "location", 
        "head_orientation", "associated_material_1"
        )),
      selectInput("filter_val", "Select Category:", choices = NULL)
    ),
    
    mainPanel(
      plotOutput("densityMap", height = "700px")
    )
  )
)

# Define server logic 
server <- function(input, output, session) {
  
  
  # 1. Load Data
  ## 1.1. Burial Data:
  b <- read_csv("Data/clean_burial_data.csv", col_names = TRUE) %>%
    drop_na(x, y)    #just in case
    
  
  ## 1.2. Tikal Map:
  t <- rast("Data/MapTikal.tif")
  t_extent <- ext(t)
  t_matrix <- as.raster(t[[1]])
  t_grob <- rasterGrob(
    t_matrix,
    width = unit(1, "npc"),
    height = unit(1, "npc"),
    interpolate = TRUE
  )
  
  
  # 2. Filtering Data
  ## 2.1. Use filter_col and filter_val as dropdown menus
  observeEvent(input$filter_col, {
    updateSelectInput(
      session,
      "filter_val",
      choices = sort(unique(b[[input$filter_col]]))
    )
  })
  
  ## 2.2. Reactive Object that filters data
  b_filtered <- reactive({
    ### 2.2.1. Make sure user selected both
    req(input$filter_col, input$filter_val)
    
    ### 2.2.2. Filter data
    b %>%
      filter(get(input$filter_col) == input$filter_val) %>%
      mutate(
        x = as.numeric(x),
        y = as.numeric(y)
      )
  })

  
  # 3. Convert filtered data to sf object
  b_sf <- reactive({
    sf_data <- st_as_sf(b_filtered(), coords = c("x", "y"), crs = 32616)
    return(sf_data)
  })
  
  
  # 4. Simulate new points (using function from creative vis project)
  b_simulated <- reactive({
    radius <- 30
    n_points <- floor(10000 / nrow(b_sf()))
    
    coords <- st_coordinates(b_sf())
    
    sim_pt_list <- lapply(1:nrow(b_sf()), function(i) {
      
      x <- coords[i, 1]
      y <- coords[i, 2]
      
      burial_data <- st_drop_geometry(b_sf()[i, drop = FALSE])
      
      angles <- runif(n_points, 0, 2 * pi)
      distances <- runif(n_points, 0, 2 * radius)
      
      x_new <- x + distances * cos(angles)
      y_new <- y + distances * sin(angles)
      
      sim_data <- cbind(burial_data[rep(1, n_points), ], x = x_new, y = y_new)
      
      return(sim_data)
    })
    
    sim_pts <- do.call(rbind, sim_pt_list)
    
    sim_sf <- st_as_sf(sim_pts, coords = c("x", "y"), crs = st_crs(b_sf()))
    
    return(sim_sf)
  })
  
  
  # 5. Generate KDE 
  b_kde <- reactive({
    sim_data <- b_simulated()
    
    windowA <- owin(
      xrange = c(t_extent[1], t_extent[2]), 
      yrange = c(t_extent[3], t_extent[4])
      )
    
    ppp_data <- ppp(
      x = st_coordinates(sim_data)[, 1], 
      y = st_coordinates(sim_data)[, 2], 
      window = windowA
      )
    
   kde_result <- density.ppp(
      ppp_data, 
      sigma = 300,
      dims = c(700, 700)
      )
   
    return(kde_result)
  })
  
  

    
  
  # 7. Create plot with Tikal basemap
  output$densityMap <- renderPlot({
    ## 7.1. Use Tikal map as a basemap
    terra::plot(
      t,
      col = grey.colors(100, start = 0, end = 1),
      legend = FALSE
      )
    
    ## 7.2. Plot the kde
    kde_result <- b_kde()
    
    min_density <- max(kde_result$v) * 0.001
    
    plot.im(
     kde_result, 
      add = TRUE, 
      col = viridis(100, alpha = seq(0, 1, length.out = 100)),
     zlim = c(0, max(kde_result$v))
      )
    
  })
  
  
}   #END OF SERVER FUNCTION

# Run the application 
shinyApp(ui = ui, server = server)