#' scaleData
#'
#' A function that scale numeric variables needed in a regression
#'
#' @param data a `data.frame` containing dependent and independent variables
#' @param fixedEffects a `vector` of variables that should be treated as fixed effects
#' @param factorVariables a `vector` of variables that should be treated as factors
#'
#' @return
#' A dataframe where the requested variables have been scaled
#'
#' @export
#'
#' @example 
scaleData <- function(data, fixedEffects = NULL,
                      factorVariables = NULL){

  # Extract all variables of interest
  numVar <- fixedEffects
  
  if (!is.null(factorVariables)){
    numVar <- numVar[is.na(match(numVar, factorVariables))]
  }

  # Scale variables
  data[, numVar] = scale(data[, numVar])

  return(data)

}
