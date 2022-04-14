#' summaryOutput
#' 
#' A function that formats the output of a regression model
#' 
#' @param model an object containing results from the regression model 
#' @param data a `dataframe` containing the observations used for the regression.
#' @param distribution a `string` that is the distribution used in the model ("gaussian", "poisson", "binomial", "betabinomial", "nbinom2")
#' @param factorVariables a `vector` containing variables that should be treated as factor
#' @param poly a `list` of 2-elements vectors with variable that should be treated as polynomial
#' @param transform a `boolean`, TRUE if coefficients should be tranformed due to distribution
#' @param rescale a `boolean`, TRUE if numeric variables estimates should be rescaled
#' @param fixedEffects a `vector` containing variables that have been used as fixed effects. `NULL` if rescale = FALSE. 
#' @param contr a `vector`containing the contrasts used for each factor in the regression 
#' 
#' @return 
#' A dataframe containing outputs of a regression model, that is correctly formatted 
#' 
#' @example 
summaryOutput <- function(model,
                          data,
                          distribution = "gaussian", 
                          factorVariables = NULL,
                          poly = NULL,
                          transform = FALSE,
                          rescale = FALSE,
                          fixedEffects = NULL,
                          contr = NA){
  
  # Deal with convergence issue
  if(identifyConvIssue(model)){
    warning("The model has encountered a convergence issue.\nThe formatting cannot be done.")
    
    return(NULL)
  }else{
    # Extract estimates from the model
    coef <- summary(model$value)$coefficients$cond
    
    # Turn to dataframe
    dataCoef <- data.frame(coef)
    
    # Erase T/Z Values and save pvalues
    dataCoef$z.value <- NULL
    
    # If numeric variables have been scaled, then rescale estimates
    if (rescale){
      dataCoef <- rescaleParam(dataCoef, data, fixedEffects, factorVariables)
    }
    
    # Create confidence interval
    dataCoef$IC_inf <- dataCoef$Estimate - 1.96 * dataCoef$Std..Error
    dataCoef$IC_sup <- dataCoef$Estimate + 1.96 * dataCoef$Std..Error
    
    # Adapt transformation to distribution if required
    if (transform){
      dataCoef$Estimate <- transformDistrib(dataCoef$Estimate, distribution)
      dataCoef$Std..Error <- transformDistrib(dataCoef$Std..Error, distribution)
      dataCoef$IC_inf <- transformDistrib(dataCoef$IC_inf, distribution)
      dataCoef$IC_sup <- transformDistrib(dataCoef$IC_sup, distribution)
      
    }
    
    # Interpret pvalues as significant or not
    sign <- ifelse(dataCoef[,"Pr...z.."] < 0.05, "Oui", "Non")
    
    # Round to 3 digits while keeping scientific notation
    dataCoef <- apply(X = dataCoef, 
                      MARGIN = 2, 
                      FUN = function(x) {
                        formatC(x, format = "e", digits = 4)
                      })
    
    
    # Keep format as data.frame
    dataCoef <- data.frame(dataCoef)
    
    # Add a significance column to the summary
    dataCoef$sign <- sign
    
    # Rename columns
    colnames(dataCoef) <- c("Estimates", "Standard Errors", "P Values", 
                            "IC_inf", "IC_sup", "Significatif")
    
    # Reorder columns
    dataCoef = dataCoef[,c("Estimates", "Standard Errors", "IC_inf", 
                           "IC_sup", "P Values", "Significatif")]
    ############
    # ROWNAMES #
    ############
    # Reformat and rename rows associated with polynomial variables
    if(!is.null(poly)){
      for (p in poly){
  
        indPoly <- grep(p[[1]], rownames(dataCoef))
        
        rownames(dataCoef)[indPoly] <- sapply(1:length(indPoly), function(x){paste0(p[[1]], " : poly", x)})
      }
    }
    
    # Reformat and rename rows associated with factor variables
    if (!is.null(factorVariables)){
      for (i in 1:length(factorVariables)){
        # Extract the ith reference level
        contrF <- ifelse(is.na(contr[i]), "regular", contr[i])
        
        # Extract the ith factor
        f <- factorVariables[i]
        
        # Turn the corresponding column in the dataframe to a factor type
        data[,f] <- factor(data[,f])
        
        # If the reference state has been set to the mean
        if (contrF == "mean"){
          # Extract row indices where the factor is present
          ind <- grep(f, rownames(dataCoef))
          
          # Extract levels associated with this factor
          levelsF <- levels(data[,f])[-1]
          
          # Change row names
          rownames(dataCoef)[ind] <- paste0(f, " : ", levelsF)
          
          # Else if the reference state has been set to one level
        }else{
          rownames(dataCoef) <- str_replace(string = rownames(dataCoef), 
                                            pattern = f,
                                            replacement = paste0(f, " : "))
        }
        
        
      }
      
    }
  }
  
  
  
  return(dataCoef)
  
}
