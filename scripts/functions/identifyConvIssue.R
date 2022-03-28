#' identifyConvIssue
#' 
#' A function that aims to identify if there was any convergence issues
#' 
#' @param model a glmmTMB object containing results from regression
#' 
#' @return a `boolean` indicating if there was indeed a convergence issue
#' NB : convergence issue = either an error, a convergence warning or NA in the summary
#' 
identifyConvIssue <- function(model){
  
  # Extract potential errors
  e <- model$error
  
  # Extract warnings with "convergence issue"
  w <- model$warnings
  
  convW <- unlist(sapply(w, function(x) grep("convergence", x)))
  
  # Identify NA in the summary
  naSum <- FALSE
  if(!is.null(model$value)){
    coefs <- summary(model$value)$coefficients$cond
    naSum <- sapply(coefs, function(x) is.na(x))
  }
  
  # If error or convergence warning or NA in summary, return TRUE
  convIssue <- FALSE
  if (!is.null(e) | any(convW) | any(naSum)){
    convIssue <- TRUE
  }
  
  return(convIssue)
}
