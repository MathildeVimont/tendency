#' makeMap
#'
#' A function that creates a map with all sites visited
#' 
#' @param data a `data.frame` containing observations of species
#' @param type a `string` : representation at the "site" or "point"/"transect" scale
#' @param shape a `string` : representation of the sites with "square" or "point"
#' 
#' @return
#' A ggplot object containing the map
#'
#' @export
#' @example
makeMap <- function(data, type = "site", shape = "square"){
  
  #######################
  # DATA TRANSFORMATION #
  #######################
  
  # Get longitude/latitude per site, and number of studied years 
  if(type == "site"){
    dataPlot <- group_by(data, site) %>%
      summarise(longitude = mean(longitude),
                latitude = mean(latitude),
                nbObs = n_distinct(year))
    
  # Get longitude/latitude per point, and number of studied years 
  }else if(type == "point"){
    dataPlot <- group_by(data, site, point) %>%
      summarise(longitude = first(longitude),
                latitude = first(latitude),
                nbObs = n())
   
  # Get longitude/latitude per transect, and number of studied years  
  }else if (type == "transect"){
    dataPlot <- group_by(data, site, transect) %>%
      summarise(longitude = first(longitude),
                latitude = first(latitude),
                nbObs = n())
  }
  ####################
  # MAP REQUIREMENTS #
  ####################
  # Extract background map of France
  dataFrance <- map_data("france")
  
  # Determine the type of points
  if(shape == "square"){
    shape <- 0
  }else{
    shape <- 1
  }
  ################
  # MAP CREATION #
  ################
  
  # Create the coordinate space of the map
  plot <- ggplot(dataFrance, aes(long, lat)) + 
    
    # Create the contours of France
    geom_polygon(aes(group = group), col = "darkgray", fill = "white") +
  
    # Add squares/points associated with the sites
    geom_point(data = dataPlot, aes(x = longitude, y = latitude, col = nbObs), 
               shape = shape, size = 2, stroke = 1.5) +
    
    # Change colour scale to blue - yellow style
    scale_color_continuous("Number of years", type = "viridis") +
    
    # Add a simple theme as the background
    theme_bw() +
      
    # Change axis names
    ylab("Latitude") + xlab("Longitude") 
  
  return(plot)
}
