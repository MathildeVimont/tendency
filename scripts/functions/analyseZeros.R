#' analyseZeros
#' 
#' A function that calculates approximately the probability of under/over-dispersion
#' 
#' @param data a dataframe containing abundance observations 
#' 
#' @return 
#' 
#' @example
#'  

analyseZeros <- function(data){
  
  # Extract number of "absence" observations
  n0 <- nrow(data[data$count == 0,])
  
  # Extract total number of observations
  n <- nrow(data)
  
  # Extract total number of observations
  perc0 <- round(100 * n0 / n, 2)
  perc0 <- paste(perc0, "%")
  
  # Create plot for the frequency of each number of abundance
  plotZeros <- ggplot(data, aes(x = count)) + 
    geom_bar(fill = "orange") +
    xlab("Abondance") + 
    ylab("Nombre d'occurrences") +
    xlim(-0.5,20) + 
    theme_bw()
  
  # Create list of results
  res <- list(n0 = n0, perc0 = perc0, plotZeros = plotZeros)
  
  return(res)
  
}
