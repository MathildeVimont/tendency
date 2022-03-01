#' tempDistrPlot
#'
#' A function that creates a histogram of number of abundance observations against year
#' @param data a `dataframe` containing information on abundance and time 
#' @param interestVar a `vector` specifying the abundance column
#' 
#' @return a histogram of number of abundance observations against year
#'
tempDistrPlot <- function(data, interestVar){
  
  #####################################
  # EXTRACT PRESENCE / ABSENCE COUNTS #
  #####################################
  
  # Check format is data.frame
  data <- data.frame(data)
  
  # Add a column that testifies if the species was present or not
  data$presence <- 'Absence'
  data[data[,interestVar] != 0,]$presence <- 'Presence'
  
  # Count number of presence / absence observations for each year
  options(dplyr.summarise.inform = FALSE)
  dataYear <- group_by(data, year, presence) %>%
    summarise(tot = n())
  
  #############
  # MAKE PLOT #
  #############
  
  # Create plot
  plot <- ggplot(dataYear, aes(x = year, y = tot, fill = presence)) +
    geom_bar(stat = "identity") +
    ylab("Number of studied points") +
    xlab("Year") +
    scale_fill_manual(values = c("#d7dbdd", "#16a085")) +
    theme_bw() +
    theme(legend.title = element_blank())
  
  return(plot)
  
} 