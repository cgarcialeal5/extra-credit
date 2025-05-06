simulate_cloud <- function(data){
  radius <- 30
  n_points <- floor(10000 / nrow(data))
  
  coords <- st_coordinates(data)
  
  sim_pt_list <- lapply(1:nrow(data), function(i) {
    
    # Original coordinates of each point
    x <- coords[i, 1]
    y <- coords[i, 2]
    
    # Original attributes associated to each point, except geometry
    burial_data <- st_drop_geometry(data[i, drop = FALSE])
    
    # Randomize angle and distance to simulate the new points
    angles <- runif(n_points, 0, 2 * pi)
    distances <- runif(n_points, 0, 2 * radius)
    
    # Calculate the new simulated coordinates
    x_new <- x + distances * cos(angles)
    y_new <- y + distances * sin(angles)
    
    # Repeat burial data for each new point without shuffling
    sim_data <- cbind(burial_data[rep(1, n_points), ], x = x_new, y = y_new)
    
    return(sim_data)
  })
  
  # Combine all simulated points into one data frame
  sim_pts <- do.call(rbind, sim_pt_list)
  
  # Convert data frame into an sf object with the correct CRS
  sim_sf <- st_as_sf(sim_pts, coords = c("x", "y"), crs = st_crs(data))
  return(sim_sf)
}