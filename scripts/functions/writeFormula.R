#' writeFormula
#' 
#' A function that creates a suitable formula object depending on the chosen parameters
#' 
#' @param interestVar a `string` containing the name of the dependent variable
#' @param fixedEffects a `vector` of variables that should be treated as fixed effects
#' @param randomEffects a `vector` of variables that should be treated as random effects 
#' @param nestedEffects a `list` of 2-elements vectors with variable names that should be treated as nested effects 
#' @param interactions a `list` of 2-elements vectors with variable names whose interaction should be taken into account
#' @param intercept a `boolean` indicating whether an intercept should be included in the model ?
#' 
#' @importFrom stats as.formula
#' 
#' @return a formula object
#' @example 
writeFormula <- function(interestVar = "count",
                         fixedEffects = NULL, 
                         randomEffects = NULL,
                         nestedEffects = list(),
                         interactions = list(),
                         intercept = TRUE){
  
  ## Initialize formula
  if(intercept){
    regrFormula <- paste0(interestVar," ~ 1")}
  else{
    regrFormula <- paste0(interestVar," ~ -1")
  }
  
  ## Add fixedEffects
  if (!is.null(fixedEffects)){
    
    for (effect in fixedEffects){
      regrFormula <- paste0(regrFormula, " + ", effect)
    }
  }
  
  ## Add random effects
  if (!is.null(randomEffects)){
    
    for (effect in randomEffects){
      regrFormula <- paste0(regrFormula, " + (1|", effect, ")")
    }
  }
  
  # ## Add interactions
  # if (length(interactions)>0){
  #   
  #   for (inter in interactions){
  #     interactionTerm <- paste0(inter[1], ":", inter[2])
  #     regrFormula <- paste0(regrFormula, " + ", interactionTerm)
  #   }
  # }
  
  ## Add nested effects
  if (length(nestedEffects)>0){

    for (nE in nestedEffects){
      nestedEffectTerm <- paste0("(1|",nE[2],"/",nE[1],")")
      regrFormula <- paste0(regrFormula, " + ", nestedEffectTerm)
    }
  }
  
  
  ## Turn character to formula type
  regrFormula <- as.formula(regrFormula)
  
  return(regrFormula)
}


