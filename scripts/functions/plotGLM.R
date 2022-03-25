#' plotGLM
#' 
#' A function that plots the estimates and error bars of the formatted model estimates
#' 
#' @param summary an `data.frame` containing the formatted output of the global trend model 
#' @param modelCat an `object` corresponding to the number of year since 1st year
#' @param summaryCat a `data.frame` containing the formatted output of the yearly variations model
#' @param effectVar a `string` that is the explanatory variable we wish to study
#' @param distribution a `string` containing the chosen distribution
#' @param type a `string` either "relative" or "absolute" to decide which type of abundance to represent
#' @param sp a `string` corresponding to the name of the species
#' @param coefs a `list` containing the global trends (with uncertainty) of the species
#' @param contr a `string` corresponding to the contrast set up for the effectVar
#' @param path a `string` corresponding to the path where the graphics should be saved
#' 
#' @return 
#' A plot that shows estimates and confidence intervals associated with the effect variable 
#' 
#' @example
plotGLM <- function(summary, modelCat, summaryCat, 
                    effectVar, distribution, type = "relative",
                    sp = NULL, coefs = NULL, contr, path = NULL){
  ###############################
  # Deal with convergence issue #
  ###############################
  if(is.null(summary)){
    warning("The global trend model has encountered a convergence issue.\nThe graphical output cannot be done.")
    return(NULL)
    
  }else if(is.null(summaryCat)){
    warning("The abundance yearly variations model has encountered a convergence issue.\nThe graphical output cannot be done.")
    return(NULL)
    
  }else{
    
    ##############################################
    # Dataframe formatting for annual variations #
    ##############################################
    
    # Add the estimate associated with the reference level
    fullSummaryCat <- addMissingEstimate(model = modelCat, summary = summaryCat, data = data, 
                                         distribution = distribution, effectVar = effectVar, contr = contr)
    
    # Reformat the summary of the global trend model
    dataVariation <- formatVariationPlot(summary = fullSummaryCat, effectVar = effectVar)
    
    ##################################
    # Dataframe formatting for trend #
    ##################################
    # Extract the values of the effectVar column
    effect <- dataVariation[,effectVar]
    
    # Find its lower / upper values
    limits <- min(effect):max(effect)
    
    # Reformat the summary of the global trend model
    dataTrend <- formatTrendPlot(summary, effectVar, limits, contr)
    
    ##################################
    # Dataframe formatting for trend #
    ##################################
    
    # Extract significance of the trend
    trendPvalue <- as.numeric(summary[effectVar,]$`P Values`)
    
    if (trendPvalue < 0.001){ 
      signTrend <- "***"
    }else if (trendPvalue < 0.01){ 
      signTrend <- "**"
    }else if(trendPvalue < 0.05){ 
      signTrend <- "*"
    }else{
      signTrend <- "NS"
    }
    
    # x-position of the significance 
    xSignTrend <- max(data[,effectVar]) + 0.5
    
    # y-position of the significance
    ySignTrend <- dataTrend$trend[nrow(dataTrend)]
    
    # adapt position of annotation according to the sign of the trend
    trend <- as.numeric(summary[effectVar,]$`Estimate`)
    ySignAnot <- ifelse(trend >= 1, 1, -1)
    
    ########################
    # Y axis limits & name #
    ########################
    
    if (type == "absolute"){
      yMaxTheory <- max(as.numeric(dataVariation$IC_sup))
      yMax <- min(yMaxTheory *2, yMaxTheory+2) 
      
      yName <- "Abondance absolue moyenne"
    }else{
      yMax <-  max(c(as.numeric(dataVariation$IC_sup), dataTrend$trend, 1.5))
      
      yName <- "Abondance relative"
    }
    
    #####################
    # Title & subtitles #
    #####################
    
    # TITLE
    title <- ifelse(!is.null(sp), paste0("Tendance pour l'espèce : ", sp), "")
    
    # SUBTITLE
    ## Turn to percentage and show sign
    newCoefs <- lapply(coefs, function(x){ paste0(ifelse(x > 0, "+", ""), round(x, 1), "%") })
    
    ## Create the trend information
    subtitle <- paste0(newCoefs$perc, " (", newCoefs$percInf, " ; ", newCoefs$percSup, ")")
    
    ## Add the temporality
    subtitle <- paste0(subtitle, " en ", length(limits)-1, " ans")
    
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
      plotGLM <- ggplot() +
        # With an horizontal line corresponding to the reference
        geom_hline(yintercept = yInt, linetype = "dashed", color = "darkgray", show.legend = T) +
        
        # With a trend
        geom_line(data = dataTrend, aes(x = var, y = trend, group = 1, colour = col),
                  size = 1.1) +
        
        annotate("text", label = "Abondance relative moyenne", 
                 x = effect[trunc(1/5 * length(effect))], y = 1 + ySignAnot * 0.03 * yMax, size = 3.1,
                 fontface = 'bold.italic', col = "darkgray") + 
        
        # And with significance of the trend or not
        annotate(geom = "text", label = signTrend, color = "red", 
                 size = 5, x = xSignTrend, y = ySignTrend)
      
    }else{
      plotGLM <- ggplot(dataVariation, aes(x = get(effectVar), 
                                           y = as.numeric(Estimates)))
    }
    
    
    plotGLM <- plotGLM +
      
      # Add points to the graphic
      geom_point(data = dataVariation, aes(x = get(effectVar), 
                                           y = as.numeric(Estimates),
                                           colour= col)) +
      
      # Add error bars to each point
      geom_errorbar(data = dataVariation, aes(x = get(effectVar),
                                              ymin = as.numeric(IC_inf), 
                                              ymax = as.numeric(IC_sup),
                                              colour = col)) +
      
      # Change visual theme
      theme_bw() +
      
      # Rotate x axis text
      theme(axis.text.x = element_text(angle = 90)) +
      
      # Change y axis limits
      ylim(c(0, yMax)) +
      
      # Rename x & y label
      xlab("Année") + ylab(yName) +
      
      # Add a title and subtitle 
      ggtitle(title, subtitle) +
      
      # 
      scale_x_continuous("Année", breaks = limits) +
      
      # Format type of trend colour legend
      scale_colour_manual(name = "Tendance", values = c("orange", "red"), 
                          labels = c("Globale", "Inter-annuelle\n(intervalle de confiance \nà 95 %)"), 
                          guide = guide_legend(override.aes = list(linetype = c(1, 1),
                                                                   shape = c(NA, 16),
                                                                   color = c("red", "orange"),
                                                                   size = c(1.1, 1))))
    
    if (!is.null(path)){
      ggsave(filename = paste0(sp, ".png"), plot = plotGLM, path = path, device = "png")
    }
    
    return(plotGLM)
    
  }
  
}

#################
# SUB-FONCTIONS # 
#################


#' formatTrendDataframe
#' 
#' A function that constructs the dataframe for representing the global trend
#' 
#' @param summary the summary of the regression model with the interesting variable as continuous 
#' @param effectVar a `string` corresponding to the interesting variable in regard to the trend  
#' @param range an `integer`corresponding to the interval of time between first and last year 
#' 
#' @return 
#' A dataframe containing outputs of a regression model, that is correctly formatted 
#' 
#' @example 
formatTrendPlot <- function(summary, effectVar, limits, contr){
  
  # Extract the rate of growth
  ROG <- summary[effectVar,]$Estimates
  
  # Turn it to numeric value
  ROG <- as.numeric(ROG)
  
  # Create vectors of relative abundance under trend
  trendVal <- c()
  for (i in 1:length(limits)){
    trendVal <- c(trendVal, ROG^i)
  }
  
  # Center the relative abundances on the 1-line
  if (contr == "mean"){
    trendVal <- trendVal / ROG^((length(limits)+1)/2) 
  }
  
  # Create data.frame containing trend
  dataTrend <- data.frame(var = limits, trend = trendVal)
  
  # Add a column with color in the data.frame
  dataTrend$col <- rep("red", nrow(dataTrend))
  
  return(dataTrend)
}


#' formatVariationPlot
#' 
#' A function that constructs the dataframe for representing the global trend
#' 
#' @param summary an `object` containing the summary of the yearly variations model
#' @param effectVar a `string` corresponding to the interesting variable in regard to the trend  
#' 
#' @return 
#' A dataframe containing outputs of a regression model, that is correctly formatted 
#' 
#' @example 
formatVariationPlot <- function(summary, effectVar){
  
  # Filter summary for the requested effect
  summary <- summary[grep(effectVar,rownames(summary)),]
  
  # Add an effect column
  summary[,effectVar] <- rownames(summary)
  
  # Reformat the effect column
  summary[,effectVar] <- str_replace(string = summary[,effectVar],
                                     pattern = paste0(effectVar, " : "),
                                     replacement = "")
  
  summary[,effectVar] <- as.numeric(summary[,effectVar])
  
  # Change level of factor "significant"
  summary$Significatif <- factor(summary$Significatif, c("Oui", "Non"))
  
  # Add a color column
  summary$col <- rep("orange", nrow(summary))
  
  return(summary)
}
