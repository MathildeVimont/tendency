#' addMissingEstimate
#' 
#' A function that fill the summary data.frame with the value of the reference level 
#' 
#' @param model an `object` containing results from the global trend model 
#' @param summary a `data.frame` containing the observations used for the regression
#' @param data a `data.frame` containing the observations used for the regression
#' @param distribution a `string` specifying the distribution chosen for the model
#' @param effectVar a `string` that is the explanatory variable we wish to study
#' @param contr a `string` corresponding to the contrast set up for the effectVar 
#' 
#' @return 
#' A dataframe containing outputs of a regression model, that is correctly formatted 
#' 
 
addMissingEstimate <- function(model, summary, data, distribution, effectVar, contr){
  
  if(is.na(contr)){
    contr <- min(data[,effectVar])
  }
  
  ##############
  # MEAN LEVEL #
  ##############
  
  if (contr == "mean"){
    # Extract variance - covariance matrix
    varCovMat <- vcov(model$value)$cond
    
    # Filter for only effectVar
    varCovMat <- varCovMat[grep(effectVar, rownames(varCovMat)),grep(effectVar, colnames(varCovMat))]
    
    # Calculate the missing estimate as sum of the others
    estInit = - 1 * sum(log(as.numeric(summary[grep(effectVar, rownames(summary)),"Estimates"])))
    
    # Calculate the variance of this estimate
    ## Initialize as the sum of the variance of each year
    estVar = sum(diag(varCovMat))
    
    ## Add 2 * covariance between each pair of year
    for (i in 1:(dim(varCovMat)[1] - 1)){
      inf = i+1
      estVar = estVar + 2 * sum(varCovMat[i, inf:dim(varCovMat)[1]])
    }
    
    # Turn the variance to a standard error
    estSE = sqrt(estVar)
    
    # Retransform according to the chosen distribution
    estSEInf = transformDistrib(estInit - 1.96 * estSE, distribution = distribution)
    estSESup = transformDistrib(estInit + 1.96 * estSE, distribution = distribution)
    estMean = transformDistrib(estInit, distribution = distribution)
    
    # Save all of this in a data.frame
    summaryAdd <- data.frame(Estimates = estMean, `Standard Errors` = estSE, IC_inf = estSEInf,
                             IC_sup = estSESup, `P Values` = NA, Significatif = NA, 
                             row.names = paste0(effectVar, " : ", min(data[,effectVar])))
    
    ###################
    # REFERENCE LEVEL #
    ###################
  } else if (contr %in% unique(data[,effectVar])){
    # Estimates is worth 1 for the reference level
    summaryAdd <- data.frame(Estimates = 1, `Standard Errors` = NA, IC_inf = 1,
                             IC_sup = 1, `P Values` = NA, Significatif = NA, 
                             row.names = paste0(effectVar, " : ", contr))
    
    
  }
  
  # Correct colnames 
  colnames(summaryAdd)[c(2,5)] = c("Standard Errors", "P Values")
  
  # Bind old coefficients with the new one
  newSum <- rbind(summary, summaryAdd)
  
  return(newSum)
}
