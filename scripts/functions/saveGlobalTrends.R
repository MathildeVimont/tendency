#' saveGlobalTrends
#'
#' A function that agregates all model results from different species into a single dataframe
#' WHEN the explaining variable whose effect we're interested in is CONTINUOUS 
#' No dataframe is returned, but it is saved as a .csv document
#' 
#' @param sp a `string` corresponding to the species considered
#' @param data a `data.frame` containing the abundance information
#' @param interestVar a `string` specifying the abundance column
#' @param effectVar a `string` specifying the explaining variable whose effect we're interested in
#' @param distribution a `string` specifying the chosen distribution in the regression model
#' @param annualMod an `object` containing results of the global regression
#' @param annualSum an `object` containing the formatted estimates of the global regression
#' @param coefs a `list` containing the global trends over the time period of the survey
#' @param path a `string` leading to the directory where the file should be saved
#' 
#' @return nothing
#'
#' @export
#'
#' @example 
saveGlobalTrends <- function(sp, data, interestVar, effectVar,
                             distribution, model, summary, coefs, vif, 
                             path = NULL){
  
  #############################
  # IMPORT EXISTING DATAFRAME #
  #############################
  preData <- data.frame() 
  if(file.exists(paste0(path,"globalTrends.csv"))){
    preData <- read.csv(file = paste0(path, "globalTrends.csv"), row.names = 1)
  }
  
  ########################
  # INITIALIZE DATAFRAME #
  ########################
  dataTrend <- matrix(nrow = 1, ncol = 20)
  dataTrend <- as.data.frame(dataTrend)
  colnames(dataTrend) <- c("species", "nbObs", "nbYear", "totAbundance", "vif",
                           "intercept", "interceptSE", "interceptInf", "interceptSup", "interceptPval",
                           "estimate", "estimateSE", "estimateInf", "estimateSup", "estimatePval",
                           "trend", "trendInf", "trendSup", "significance", "category")
  
  ##################
  # FILL DATAFRAME #
  ##################
  
  # Extract species names
  dataTrend$species <- sp
  
  # Extract number of observations site x year
  dataTrend$nbObs <- nrow(data)
  
  # Extract number of year
  dataTrend$nbYear <- max(data[, effectVar]) - min(data[, effectVar])
  
  # Extract total abundance
  dataTrend$totAbundance <- sum(data[, interestVar])
  
  # Extract vif when available
  if(is.null(vif$error) & class(vif$value) != "character"){
    dataTrend$vif <- vif$value[, effectVar]
  }
  
  # Extract model outputs when convergence has been reached
  if(!identifyConvIssue(model)){
    
    # Extract intercept and upper/lower value of its interval confidence
    dataTrend$intercept <- as.numeric(summary["(Intercept)",]$Estimates)
    dataTrend$interceptSE <- as.numeric(summary["(Intercept)",]$`Standard Errors`)
    dataTrend$interceptInf <- as.numeric(summary["(Intercept)",]$IC_inf)
    dataTrend$interceptSup <- as.numeric(summary["(Intercept)",]$IC_sup)
    dataTrend$interceptPval <- as.numeric(summary["(Intercept)",]$`P Values`)
    
    # Extract estimates and upper/lower value of its interval confidence
    dataTrend$estimate <- as.numeric(summary[effectVar, ]$Estimates)
    dataTrend$estimateSE <- as.numeric(summary[effectVar, ]$`Standard Errors`)
    dataTrend$estimateInf <- as.numeric(summary[effectVar, ]$IC_inf)
    dataTrend$estimateSup <- as.numeric(summary[effectVar, ]$IC_sup)
    dataTrend$estimatePval <- as.numeric(summary[effectVar, ]$`P Values`)
    
    # Extract trend and upper/lower value of its interval confidence
    dataTrend$trend <- coefs$perc
    dataTrend$trendInf <- coefs$percInf
    dataTrend$trendSup <- coefs$percSup
    
    # Extract significance of the trend
    dataTrend$significance <- summary["year",]$Significatif
    
    # Affect an EBCC category to the trend
    dataTrend$category <- affectCatEBCC(summary = summary, 
                                        effectVar = effectVar, 
                                        distribution = distribution)
    
  }
  
  ###################
  # BIND DATAFRAMES #
  ###################
  
  globalTrends <- rbind(preData, dataTrend)
  
  ##################
  # SAVE DATAFRAME #
  ##################
  write.csv(x = globalTrends, file = paste0(path, "globalTrends.csv"))
}





