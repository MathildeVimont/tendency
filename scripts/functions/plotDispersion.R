#' plotDispersion
#' 
#' A function that plots abundance distribution with confrontation to the assumed distribution 
#' 
#' @param data data a `dataframe` containing the variables used in the GLM
#' @param interestVar a `string` corresponding to the response variable 
#' @param distribution a `string` corresponding to the distribution of the residuals
#' @param nTry an `integer` corresponding to the number of trials in case of a binomial distribution 
#' 
#' 
#' @return an `histogram` plotting :
#' - theoretical (i.e, if no dispersion and no zero-inflation) against
#' - observed distribution of the response variable
#' 
#' @example
#'  

plotDispersion <- function(data, interestVar, distribution, nTry = NULL){

  countVal <- data[,interestVar]
  ###########################
  # DISTRIBUTION SIMULATION #
  ###########################
  
  if(distribution %in% c("poisson", "nbinom2")){
    distrVal <- rpois(n = nrow(data), lambda = mean(countVal, na.rm = T))
    
  }else if(theorDistsrib == "binomial"){
    distrVal <- rbinom(n = nrow(data), size = nTry, prob = mean(countVal, na.rm = T) / nrow(data))
  }
  
  ###################
  # PLOT FORMATTING #
  ###################
  
  # Extract maximum counting value
  xmax <- max(distrVal, countVal)

  # Create a variable  containing the number of breaks
  nbBreaks = xmax * 2
  
  # Create the data frame
  dataPlot <- data.frame(values = c(countVal, distrVal),
                         type = rep(c("Raw", "Simulated"), each = nrow(data)))
  
  # Create the histogram
  hist <- ggplot(dataPlot, aes(values, fill = type)) + 
    geom_histogram(position = "dodge", bins = nbBreaks)  + 
    theme_bw() +
    scale_fill_manual("Data Type", values = c("honeydew3", "yellowgreen"))  +
    xlab("Abundance") +
    ylab("Frequency")

  return(hist)
}
