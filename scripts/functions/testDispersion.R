#' testDispersion
#' 
#' A function that calculates approximately the probability of under/over-dispersion
#' 
#' @param model an object containing results from the regression model 
#' 
#' @return 
#' 
#' @example
#'  

testDispersion <- function(model){
  
  ########################
  # Calculate statistics #
  ########################
  # Extract residual degree of freedom
  residualDf <- df.residual(model)
  
  # Extract "pearson" residuals
  pearsonRes <- residuals(model, type = "pearson")
  
  # Calculate sum of squared residuals
  Pearson.chisq <- round(sum(pearsonRes^2), 0)
  
  # Calculate ratio between sum of squared residuals and residual degrees of freedom
  ratio <- round(Pearson.chisq / residualDf, 1)
  
  # Calculate probability that the ratio differs from 1
  pValue <- pchisq(Pearson.chisq, df = residualDf, lower.tail = FALSE)
  
  ##################
  # Format outputs #
  ##################
  # Put all results together
  stats = matrix(c(Pearson.chisq, residualDf, ratio, pValue))
  
  # Turn to dataframe
  stats = data.frame(t(stats))
  
  # Rename colnames
  colnames(stats) <- c("SumSqRes", "ResidualDf", "Ratio", "P Values")

  return(stats)
  
}
