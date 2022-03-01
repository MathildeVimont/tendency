#' analyseCoef
#' 
#' A function that calculates the trend and percentage of evolution from a regression model
#' 
#' @param model an object containing results from the regression model 
#' @param rescale a `boolean` that indicates if parameters should be de-normalized or not
#' @param data a `data.frame` containing data of the regression variables
#' @param distribution a `string` that is the distribution used in the model ("gaussian", "poisson", "binomial")
#' @param effectVar a `string` that is the explanatory variable we aim to study (ex : "year")  
#' @param varRange an `integer`, that is the range of values of the effectVar. Default is 1.
#' 
#' @return 
#' A list of 4 elements:
#' - trend, that is the multiplicative coefficient of abundance evolution in between the varRange 
#' - perc, that is the percentage of evolution of abundance in between the varRange 
#' - percInf, that is the lower bound of the interval confindence around perc 
#' - percSup, that is the upper bound of the interval confindence around perc 
#' 
#' @example
#'  

analyseCoef <- function(model, 
                        rescale = F,
                        data = NULL,
                        distribution, 
                        effectVar = "year", 
                        varRange = 1){
  
  # Extract estimates from the model
  coef <- summary(model)$coefficients$cond
  
  # Turn to dataframe
  dataCoef <- data.frame(coef)
  
  # Extract coefficient from the chosen explanatory variable
  interestCoefs <- c(dataCoef[effectVar,]$Estimate,
                     dataCoef[effectVar,]$Estimate - 1.96 * dataCoef[effectVar,]$Std..Error,
                     dataCoef[effectVar,]$Estimate + 1.96 * dataCoef[effectVar,]$Std..Error)
  
  # Rescale parameter if needed
  if (rescale){
    interestCoefs <- sapply(interestCoefs, function(x) x / sd(data[,effectVar]))
  }
  
  # Back-transform the coefficients according to the distribution
  trends <- sapply(interestCoefs, function(x) transformDistrib(x * varRange, distribution))
  
  # Extract the trend from the estimates 
  # "For an increase of x in year, the mean count was multiplied by trend"
  trend <- trends[1]
  
  # Turn the trend to a percentage of variation
  # "For an increase of x in year, the mean count was in/decreased by perc"
  perc <- 100 * (trend - 1)
  
  # Extract the lower and upper bounds of the interval confidence around perc
  percInf <- 100 * (trends[2] - 1)
  percSup <- 100 * (trends[3] - 1)
  
  # Create a list with those indicators
  coefList <- list(trend = trend, perc = perc, percInf = percInf, percSup = percSup)
  
  # Round those indicators so that only 2 decimals are kept
  coefList <- lapply(coefList, function(x) round(x, 2))
  
  return(coefList)
  
  
}

