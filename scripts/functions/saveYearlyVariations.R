#' saveYearlyVariations
#'
#' A function that agregates all model results from different species into a single dataframe
#' WHEN the explaining variable whose effect we're interested in is CATEGORICAL 
#' <!> No dataframe is returned, but it is saved as a .csv document
#' 
#' @param sp a `string` corresponding to the species considered
#' @param data a `data.frame` containing the abundance information
#' @param interestVar a `string` specifying the abundance column
#' @param effectVar a `string` specifying the explaining variable whose effect we're interested in
#' @param model an `object` containing results of the global regression
#' @param summary an `object` containing the formatted estimates of the global regression
#' @param contr a `string` specifying the contrast chosen for the time variable
#' @param path a `string` leading to the directory where the file should be saved
#' 
#' @return nothing
#'
#' @example 
saveYearlyVariations <- function(sp, data, interestVar, effectVar = "year",
                                 model, summary, contr, path = NULL){
  
  #############################
  # IMPORT EXISTING DATAFRAME #
  #############################
  preData <- data.frame() 
  if(file.exists(paste0(path,"yearlyVariations.csv"))){
    preData <- read.csv(file = paste0(path, "yearlyVariations.csv"), row.names = 1)
  }
  
  ########################
  # INITIALIZE DATAFRAME #
  ########################
  
  # Extract the type of contrasts and thus the estimated levels of the effectVar
  if(is.na(contr)|contr %in% c("mean", min(data[, effectVar]))){
    eff <- (min(data[, effectVar])+1):max(data[, effectVar])
  }else if (contr == max(data[, effectVar])){
    eff <- min(data[, effectVar]):(max(data[, effectVar])-1)
  }else{
    eff <- c(min(data[, effectVar]):(as.numeric(contr) - 1),(as.numeric(contr) + 1):max(data[, effectVar]))
  }
  
  # Initialize the proper sized dataframe
  dataTrend <- matrix(nrow = length(eff) + 1, ncol = 11)
  dataTrend <- as.data.frame(dataTrend)
  
  # Initialize the column names
  colnames(dataTrend) <- c("species", "nbObs", "nbYear", "totAbundance", 
                           "contrast", effectVar, "estimate", "estimateInf", 
                           "estimateSup", "pval", "significance")
  
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
  dataTrend$totAbundance <- sum(data[,interestVar])
  
  # Add the contrast information
  dataTrend$contrast <- ifelse(is.na(contr), min(data[, effectVar]), contr)
  
  # Add the contrast information
  dataTrend[,effectVar] <- c("intercept", eff)
  
  # Extract model outputs when convergence has been reached
  if(!is.null(model)&!identifyConvIssue(model)){
    
    # Extract position for each level of effectVar
    ind <- unlist(sapply(dataTrend[,effectVar], function(x) grep(x, tolower(rownames(summary)))))
    
    # Add the estimate value
    dataTrend$estimate <- as.numeric(summary[ind, "Estimates"])
    
    # Add the lower value of the confidence interval
    dataTrend$estimateInf <- as.numeric(summary[ind, "IC_inf"])
    
    # Add the upper value of the confidence interval
    dataTrend$estimateSup <- as.numeric(summary[ind, "IC_sup"])
    
    # Add the pvalues associated with each estimate
    dataTrend$pval <- as.numeric(summary[ind, "P Values"])
    
    # Add the significance of each estimate
    dataTrend$significance <- summary[ind, "Significatif"]
    
  }
  
  ###################
  # BIND DATAFRAMES #
  ###################
  
  yearlyVariations <- rbind(preData, dataTrend)
  
  ##################
  # SAVE DATAFRAME #
  ##################
  write.csv(x = yearlyVariations, file = paste0(path, "yearlyVariations.csv"))
}
