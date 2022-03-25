#' checkData
#'
#' A function that checks if variables are present in the dataframe
#' @param data a `data.frame` containing observations of species.#'
#' @param type a `string` either NULL, "transect" or "point
#' @param interestVar a `string` containing the variable of interest
#' @param fixedEffects a `vector` containing variables that should be treated as fixed effects
#' @param randomEffects a `vector` containing variables that should be treated as random effects 
#' @param effectVar a `string` that is the independent variable whose effect we are interested in 
#' 
#' @return
#' A 2 elements list with :
#' - a `boolean` telling if certain variables are missing in the data.frame
#' - a `vector` of these missing variables if they do exist 
#'
#' @export
#' @example
checkData <- function(data, type = NULL,
                      interestVar = NULL, 
                      fixedEffects = NULL, 
                      randomEffects = NULL,
                      effectVar = "year"){
  
  # All variables that should be contained in dataframe 
  vars <- c("year", "species", "site", "ID")
  
  # Add effects to the vector
  vars <- c(vars, interestVar, fixedEffects, randomEffects, effectVar, unlist(nestedEffects))
  
  # Add a "point" or "transect" variable
  if (!is.null(type)){
    vars <- c(vars, type)
  }
  
  # Make sure the vector has unique values
  vars <- unique(vars)
  
  # If some variables are not found in the dataframe
  if (any(!(vars %in% colnames(data)))){
    missing <- TRUE
    missingVars <- vars[which(!(vars %in% colnames(data)))]
    
    stop("Missing Variables in dataframe: ", missingVars, "\n",
         "  Please provide a dataframe containing those variables")
    return(FALSE)
    
  }
  # If the effectVar is not a continuous variable, stop the process
  else if (!is.numeric(data[,effectVar])){ 
    stop("The variable effectVar : ", effectVar, ", should be numeric !\n",
         "Please check that you chose the correct variable and that it has proper format.")
    }
  
  
  # If all variables are found in the dataframe
  else{
    message("Your dataframe has proper column names and \n",
            "You can proceed to further analysis !")
    return(TRUE)
  }
  
}
