#' detectDistrib
#'
#' A function that makes a GLM with specified
#'
#' @param data a `dataframe` containing the variables used in the GLM
#' @param interestVar a `string` corresponding to the response variable
#' @param distribution a `string` specifying the wanted distribution for the regression. Can be empty. 
#' @param zi a `boolean`. TRUE if a zero-inflated model should be taken into account. Can be empty. 
#'
#' @return a list of 3 objects :
#' 
#' @example
detectDistrib <- function(data, interestVar, distribution = NULL, zi = NULL){
 
  # If the distribution is not specified
  if(is.null(distribution) | is.null(zi)){
    
    # Extract the values of the response variable
    abundance <- data[,interestVar]
    
    # Erase NAs
    abundance <- abundance[!is.na(abundance)]
    
    # If the response variable is a counting variable
    if(all(abundance == round(abundance)) & max(abundance) > 1){
      distribution <- "nbinom2"
      zi <- TRUE
      
    }
    # If the response variable is a presence/absence variable
    else if(all(abundance %in% c(0:1))){
      distribution <- "binomial"
      zi <- FALSE
    }
    
  # If the distribution is already specified, keep it
  } else{
    distribution <- distribution
    zi <- zi
  }
  
  return(list(distribution = distribution,
              zi = zi))
}
