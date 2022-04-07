#' rescaleParam
#'
#' A function that transform estimates associated with scaled variables
#' 
#' @param dataCoef a `data.frame` containing estimates, standard errors and interval confidence from a regression
#' @param data a `data.frame` containing dependent and independent variables
#' @param fixedEffects a `vector` of variables that should be treated as fixed effects
#' @param factorVariables a `vector` of variables that should be treated as factors
#'
#' @return
#' A dataframe where estimates, standard errors and confidence intervals of parameters have been rescaled
#'
#' @export
#'
#' @example
rescaleParam <- function(dataCoef, data, 
                         fixedEffects = NULL,
                         factorVariables = NULL){
  
  # Extract numeric variables from effects 
  numVar <- fixedEffects
  if (!is.null(factorVariables)){
    numVar <- numVar[is.na(match(numVar, factorVariables))]
  }
  
  ################
  # IF INTERCEPT #
  ################
  if("(Intercept)" %in% rownames(dataCoef)){
    
    # Initialize the value of estimates / standard errors related to the intercept 
    # NB: only required if an intercept is present
    interceptEst <- dataCoef["(Intercept)",]$Estimate
    interceptStdErr <- dataCoef["(Intercept)",]$Std..Error
    
    for (v in numVar){
      # Extract mean and standard deviation from original dataset
      meanVar <- mean(data[,v], na.rm = T)
      sdVar <- sd(data[,v], na.rm = T)
      
      # Calculate new coefficients and standard deviations for the considered variable
      # B = Bs(X) / sd(X)
      dataCoef[v,]$Estimate <- dataCoef[v,]$Estimate / sdVar
      dataCoef[v,]$Std..Error <- dataCoef[v,]$Std..Error / sdVar
      
      # Calculate new coefficients and standard deviations for intercept
      # A = As - Bs(X) * mean(X) / sd(X)
      interceptEst <- interceptEst - (dataCoef[v,]$Estimate * meanVar / sdVar)
      interceptStdErr <- interceptStdErr - (dataCoef[v,]$Std..Error * meanVar / sdVar)
      
    }
    
    # Calculate new coefficients and standard deviations for the intercept
    dataCoef["(Intercept)",]$Estimate <- interceptEst
    dataCoef["(Intercept)",]$Std..Error <- interceptStdErr
    
  }
  ###################
  # IF NO INTERCEPT #
  ###################
  else{
    # Calculate new coefficients and standard deviations for the considered variable
    # Bs = B(X) / sd(X)
    dataCoef[numVar,]$Estimate <- sapply(numVar, function(v){ dataCoef[v,]$Estimate / sd(data[,v], na.rm = T) })
    dataCoef[numVar,]$Std..Error <- sapply(numVar, function(v) { dataCoef[v,]$Std..Error / sd(data[,v], na.rm = T) })
    
  }

  return(dataCoef)
  
}

