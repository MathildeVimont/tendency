#' affectCatEBCC
#'
#' A function that affect a category of decline/increase to each species 
#' Based on trend significance and values
#' 
#' @param summary an `object` containing the formatted results from the global trend model
#' @param effectVar a `string` specifying the explaining variable to stuty
#' @param distribution a `string` specifying the distribution used in the model
#' 
#' @return nothing
#'
#' @export
#'
#' @example 
affectCatEBCC <- function(summary, effectVar = "year", distribution){
  
  catEBCC <- NA
  
  if(is.null(summary)){
    warning("Object 'summary' is NULL, probably due to a convergence issue.\n",
            "The category of increase/decline could not be computed. NAs were created.")
    
  }else{
    if(distribution %in% c('poisson', 'nbinom2')){
      # Extract the significance of the trend
      sign = summary[,effectVar]$Significatif == "Oui" 
      
      # Extract the certainty of the trend
      cert = as.numeric(summary[,effectVar]$IC_sup) < 1.05 
      cert = cert | as.numeric(summary[,effectVar]$IC_inf) > 0.95
      
      # Extract the intensity of the decline
      intenseDecline = as.numeric(summary[,effectVar]$IC_sup) < 0.95
      
      # Extract the intensity of the increase
      intenseIncrease = as.numeric(summary[,effectVar]$IC_inf) > 1.05
      
      # If the trend is not significant but close to stability
      if(!sign & cert){
        catEBCC <- "Stable"
      }
      # If the trend is not significant but far from stability
      else if(!sign & !cert){
        catEBCC <- "Incertain"
      }
      # If the trend is significant and fast declining
      else if(sign & intenseDecline){
        catEBCC <- "Fort déclin"
      }
      # If the trend is significant and low declining
      else if(sign & !intenseDecline){
        catEBCC <- "Déclin modéré"
      }
      # If the trend is significant and fast increasing
      else if(sign & intenseIncrease){
        catEBCC <- "Forte augmentation"
      }
      # If the trend is significant and low increasing
      else if(sign & !intenseIncrease){
        catEBCC <- "Augmentation modérée"
      }
    }else{
      warning("The categorization has not been implemented for distributions other than 'poisson' or 'nbinom2'.",
              "NAs were created.")
      
    }
    
  }
  
  return(catEBCC)
  
}