#' plotGLM
#' 
#' A function that plots the estimates and error bars of the formatted model estimates
#' 
#' @param summary a `data.frame` that is the formatted summary output of the model
#' @param effectVar a `string` that is the explanatory variable we're interested in
#' @param distribution a `string` containing the chosen distribution
#' @param type a `string` either "relative" or "absolute" to decide which type of abundance to represent
#' 
#' @return 
#' A plot that shows estimates and confidence intervals associated with the effect variable 
#' 
#' @example
plotGLM <- function(summary, effectVar, distribution, type = "relative"){
  
  ########################
  # Dataframe formatting #
  ########################
  
  # Filter summary for the requested effect
  summary <- summary[grep(effectVar,rownames(summary)),]
  
  # Add an effect column
  summary[,effectVar] <- rownames(summary)
  
  # Reformat the effect column
  summary[,effectVar] <- str_replace(string = summary[,effectVar],
                                     pattern = paste0(effectVar, " : "),
                                     replacement = "")
  
  summary[,effectVar] <- as.factor(summary[,effectVar])
  
  # Change level of factor "significant"
  summary$Significant <- factor(summary$Significant, c("Yes", "No"))
  
  ########################
  # Y axis limits & name #
  ########################
  
  if (type == "absolute"){
    yMaxTheory <- max(as.numeric(summary$IC_sup))
    yMax <- min(yMaxTheory *2, yMaxTheory+2) 
    
    yName <- "Abondance absolue moyenne"
  }else{
    yMax <-  max(c(as.numeric(summary$IC_sup),1.5))
    
    yName <- "Abondance relative"
  }
  
  #############################
  # Distribution implications #
  #############################
  yInt <- 0
  
  # If gaussian distribution 
  # The reference value is 0
  if (distribution == "gaussian"){
    yInt <- 0
  }
  # If poisson or negative binomiale distribution 
  # The reference value is exp(0) i.e, 1
  else if(distribution %in% c("poisson", "nbinom2")){
    yInt <- 1  
  }
  
  # If binomial distribution
  # The reference value is 1/(1+exp(0)) i.e, 1/2
  else if (distribution %in% c("binomial", "betabinomial")){
    yInt <- 0.5
  }

  ########
  # Plot #
  ########
  
  # Initialize the plot
  if (type == "relative"){
    plotGLM <- ggplot(summary, aes(x = get(effectVar), 
                                   y = as.numeric(Estimates),
                                   shape = Significant))
  }else{
    plotGLM <- ggplot(summary, aes(x = get(effectVar), 
                                   y = as.numeric(Estimates)))
  }

  plotGLM <- plotGLM +
    
    # Add points to the graphic
    geom_point(colour = "orange") +
    
    # Add error bars to each point
    geom_errorbar(aes(ymin = as.numeric(IC_inf), 
                      ymax = as.numeric(IC_sup)), 
                  colour = "orange") +
    
    # Change visual theme
    theme_bw() +
    
    # Rotate x axis text
    theme(axis.text.x = element_text(angle = 45)) +

    # Change y axis limits
    ylim(c(0, yMax)) +
    
    # Rename x & y label
    xlab("Annee") + ylab(yName)
  
  if(type == "relative"){
    plotGLM <- plotGLM +
      
      # Add an horizontal line corresponding to the reference
      geom_hline(yintercept = yInt, linetype = "dashed", color = "darkgray")
  }
  
    return(plotGLM)
}
