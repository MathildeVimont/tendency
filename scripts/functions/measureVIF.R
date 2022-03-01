#' measureVIF
#' 
#' A function that calculates Variance Inflation Indicators for each variable
#' 
#' @param model an glmmTMB object containing the output of the regression
#' 
#' @return a `dataframe` containing VIFs associated to each variable
#' 
#' @example 
measureVIF <- function (model) {
  
  # Extract variance-Covariance matrix from model output
  v <- vcov(model)$cond
  
  # If there is only one variable treated as fixed effect, do not measure VIF.
  if (dim(v)[1] <= 2){
    VIF <- "There is only one variable treated as fixed effect in the model. No VIF was measured."
    
  } else{
    # Exclude intercepts
    v <- v[-1,-1]
    
    # Extract names of the matrix
    cols <- colnames(v) 
    
    # Square root of the diagonal of the variance-covariance matrix
    d <- sqrt(diag(v))
    
    # Stop the measure of VIF if NAs are created
    if(any(is.na(d))) {
      stop(paste0("The diagonal matrice of the variance-covariance matrix produced",
                  "at least one negative value\n",
                  "The VIFs cannot be assessed"))
    }
    
    # Variance-covariance matrix on outer product of d
    prodD <- v/(d %o% d)
    
    # Inverse d
    invD <- solve(prodD)
    
    # Return the diagonal of d
    VIF <- diag(invD)
    
    # Transform to data.frame
    VIF <- data.frame(t(VIF))
    
    # Add rownames
    rownames(VIF) <- "VIF"
    
    # Round to 2 digits
    VIF <- round(VIF,2)
    
  }
  
  return(VIF)
}
