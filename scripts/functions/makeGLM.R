#' makeGLM
#'
#' A function that makes a GLM with specified
#'
#' @param data a `dataframe` containing the variables used in the GLM
#' @param interestVar a `string` corresponding to the response variable
#' @param fixedEffects a `vector` of variables that should be treated as fixed effects
#' @param randomEffects a `vector` of variables that should be treated as random effects
#' @param factorVariables a `vector` of variables that should be treated as factors
#' @param contrasts a `vector` containing the contrasts that should be applied to the categorical variables
#' @param distribution a `string` containing the chosen distribution between "gaussian", "poisson", "binomial", "betabinomial", "nbinom2" (by default : "gaussian")
#' @param zi a `boolean` that is `TRUE` if zero-inflation should be taken into account
#' @param nTry an `integer` corresponding to the number of potential presences. Only needed in case of binomial distribution.
#' @param scaling a `boolean` indicating whether numeric variables should be scaled
#' @param intercept a `boolean` indicating whether an intercept should be included in the model ?
#'
#' @importFrom glmmTMB glmmTMB
#'
#' @return a list of 3 objects :
#' - value a `glmmTMB` object containing results of the regression
#' - warnings a `list` of potential warnings encountered during the regression
#' - error a `string` containing a potential error encountered during the regression
#' @example
makeGLM <- function(data, interestVar = "count", fixedEffects = NULL,
                    randomEffects = NULL, nestedEffects = NULL,
                    factorVariables = NULL, contr = NULL,
                    distribution = "gaussian", zi = FALSE, 
                    nTry = NULL, scaling = FALSE, intercept = TRUE){
  
  ####################
  # Error management #
  ####################
  
  # Check class of data
  if (class(data) != "data.frame"){
    stop("'data' should be a data.frame")
  }
  
  # Check that all variables exist
  vars <- c(interestVar, fixedEffects, randomEffects)
  
  if (any(!(vars %in% colnames(data)))){
    missingVars <- vars[which(!(vars %in% colnames(data)))]
    stop("the variable '", paste0(missingVars, "' is not found in dataframe"))
  }
  
  # Check distribution exists
  if (!(distribution %in% c("binomial", "betabinomial", "gaussian", "poisson", "nbinom2"))){
    stop("the chosen distribution doesn't exist. \nPlease chose between : binomial, betabinomial, gaussian, poisson or nbinom2")
  }
  
  ###############
  # Format data #
  ###############
  
  # Check that factor variables correspond to factor columns
  # And choose the right contrasts for each categorical variable
  data <- setContrasts(data, factorVariables, contr)
  
  # Scale variables of interest if requested
  if(scaling){
    data <- scaleData(data, fixedEffects, factorVariables)
  }
  
  ###############################
  # Write correctly the formula #
  ###############################
  
  # For the regression model
  formula <- writeFormula(interestVar = interestVar,
                          fixedEffects = fixedEffects,
                          randomEffects = randomEffects,
                          nestedEffects = nestedEffects,
                          intercept = intercept)
  
  # To take zero-inflation into account
  ziFormula <- formula("~0") 
  if(zi){
    ziFormula <- formula("~1")
  }
  
  #####################
  # Tendency modeling #
  #####################
  # <!> PAS TRES PROPRE <!> #
  
  if(distribution %in% c("poisson", "nbinom2")){
    model <- catchConditions(glmmTMB(formula, data = data, 
                                     family = distribution, 
                                     ziformula = ziFormula,
                                     control = glmmTMBControl(profile = quote(nrow(data) > 10000 & length(parameters$beta)>=5))))
    
    # If 1st convergence issue, try changing the optimizer
    convIssue <- identifyConvIssue(model)
   
    if(convIssue){
      model <- catchConditions(glmmTMB(formula, data = data, 
                                       family = distribution, 
                                       ziformula = ziFormula,
                                       control = glmmTMBControl(profile = quote(nrow(data) > 10000 & length(parameters$beta)>=5),
                                                                optimizer = optim, optArgs =  list(method="BFGS"))))
      
    }
    
    # If 2nd convergence issue, try increasing iterations
    convIssue <- identifyConvIssue(model)
    
    if(convIssue){
      model <- catchConditions(glmmTMB(formula, data = data, 
                                       family = distribution, 
                                       ziformula = ziFormula,
                                       control = glmmTMBControl(profile = quote(nrow(data) > 10000 & length(parameters$beta)>=5),
                                                                optCtrl = list(iter.max = 1000, eval.max = 1000))))
      
    }
    
  }else{
    model <- catchConditions(glmmTMB(formula, data = data, 
                                     family = distribution, 
                                     weights = rep(nTry, nrow(data)),
                                     control = glmmTMBControl(profile = quote(nrow(data) > 10000) & length(parameters$beta)>=5)))
    
    # If 1st convergence issue, try changing the optimizer
    convIssue <- identifyConvIssue(model)
    
    if(convIssue){
      model <- catchConditions(glmmTMB(formula, data = data, 
                                       family = distribution, 
                                       weights = rep(nTry, nrow(data)),
                                       control = glmmTMBControl(profile = quote(nrow(data) > 10000 & length(parameters$beta)>=5),
                                                                optimizer = optim, optArgs =  list(method="BFGS"))))
      
    }
    
    # If 2nd convergence issue, try increasing iterations
    convIssue <- identifyConvIssue(model)
    
    if(convIssue){
      model <- catchConditions(glmmTMB(formula, data = data, 
                                       family = distribution, 
                                       weights = rep(nTry, nrow(data)),
                                       control = glmmTMBControl(profile = quote(nrow(data) > 10000 & length(parameters$beta)>=5),
                                                                optCtrl = list(iter.max = 1000, eval.max = 1000))))
      
    }
  }
  
  
  ######################
  # Save model results #
  ######################
  
  return(model)
}
