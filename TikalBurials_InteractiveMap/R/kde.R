kde <- function(data, ext_template, sigma){
  
  Tikalext <- ext(ext_template)
  
  windowA <- owin(
    xrange = c(Tikalext[1], Tikalext[2]),
    yrange = c(Tikalext[3], Tikalext[4]))
  
  coordsA <- st_coordinates(data)
  
  ppp_data <- ppp(x = coordsA[,1], y = coordsA[,2], window = windowA)
  
  kde_data <- density(ppp_data, sigma = sigma)
  
  return(kde_data)
}