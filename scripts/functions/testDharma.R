#' testDharma
#' 
#' A function that tests for model hypotheses throught the use of DHARMa package (simulations-based)
#' 
#' @param model an object containing results from the regression model 
#' 
#' @return a list of 2 elements:
#' - unif, the result of the uniform residuals test
#' - outliers, the results of the outliers test
#' 
#' @example
#'  

testDharma <- function(model){
  # Simulate residuals
  simulationOutput <- DHARMa::simulateResiduals(model,plot =  F)
  
  # Test for uniformity of residuals
  unifTest <- DHARMa::testUniformity(simulationOutput, plot = F)
  
  # Test for outliers in residuals
  outTest <- DHARMa::testOutliers(simulationOutput, plot = F)
  
  return(list(unif = unifTest,
              outliers = outTest))
}
  