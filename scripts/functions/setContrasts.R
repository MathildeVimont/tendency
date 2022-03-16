#' setContrasts
#'
#' A function that changes contrasts associated with 
#'
#' @param data a `dataframe` containing the variables used in the GLM
#' @param factorVariables a `vector` containing variables that should be treated as fixed
#' @param contr a `vector` containing reference that should be used in the GLM
#'
#' @return a `data.frame` with the right contrasts set
#' 
#' #' @example
setContrasts <- function(data, factorVariables = NULL, contr = NULL){
  
  # Check that there are indeed factor variables to be treated
  if(!is.null(factorVariables)){
  
    # Go through all variables
    for (i in 1:length(factorVariables)){
      # Extract the type of contrast change that should be applied
      contrVar <- contr[i]
      
      # Extract the name of the factor variable
      f <- factorVariables[i]
      
      # Change type of the column
      data[,f] <- as.factor(data[,f])
      
      # Extract the levels of that variable
      fLevels <- levels(data[,f])
      
      # If the contrast type is not known, force it to the first level
      if (ifelse(is.null(contrVar), TRUE, !(contrVar %in% c("mean", fLevels)))){
        
        contrVar = fLevels[1]
        message(paste0("Contrast associated with the variable '",f,
                       "' is not specified or not recognized. It has been ",
                       "changed to : estimate(", fLevels[1],") = 0."))
      }
      
      # If contrasts should be set to sum(estimate) = 0
      if(contrVar == "mean"){
        # Extract the number of levels of the categorical variable
        nbLevels <- length(fLevels)
        
        # Extract the number of columns of the contrast matrix
        nbCols <- nbLevels - 1
        
        # Set first row of the contrasts to -1
        firstRowCont <- c(rep(-1, nbCols))
        
        # Set all following rows to a diagonal of 1
        diagCont <- diag(1, nrow = nbCols, ncol = nbCols)
        
        # Set contrasts
        contrasts(data[,f]) <- matrix(c(firstRowCont, diagCont), 
                                      nrow = nbLevels, 
                                      ncol = nbCols, 
                                      byrow = T)
        
        
        # If contrasts should be set to a specific column of reference
        # Relevel the variable, beginning by the reference factor
      }else if (contrVar %in% fLevels){
        data[,f] <- factor(x = data[,f], 
                           levels = c(contrVar, fLevels[-which(contrVar == fLevels)]) )
        
        
        
      }
    }
  }
  return(data)
}
