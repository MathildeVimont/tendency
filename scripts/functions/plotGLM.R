#' plotGLM
#' 
#' A function that plots the estimates and error bars of the formatted model estimates
#' 
#' @param summaryCat a `data.frame` that is the formatted summary output of the model
#' @param effectVar a `string` that is the explanatory variable we're interested in
#' @param distribution a `string` containing the chosen distribution
#' @param type a `string` either "relative" or "absolute" to decide which type of abundance to represent
#' @param summary an `object` containing the formatted output of 
#' @param sp a `string` corresponding to the name of the species
#' @param coefs a `list` containing the global trends (with uncertainty) of the species
#' @param range an `integer` corresponding to the number of year since 1st year
#' 
#' @return 
#' A plot that shows estimates and confidence intervals associated with the effect variable 
#' 
#' @example
plotGLM <- function(summaryCat, effectVar, distribution, type = "relative",
                    summary = NULL, sp = NULL, coefs = NULL){
  
  ##############################################
  # Dataframe formatting for annual variations #
  ##############################################
  
  dataVariation <- formatVariationPlot(summaryCat, effectVar)
  
  ##################################
  # Dataframe formatting for trend #
  ##################################
  years <- dataVariation[,effectVar]
  limits <- min(years):max(years)
  dataTrend <- formatTrendPlot(summary, effectVar, limits)
  
  ##################################
  # Dataframe formatting for trend #
  ##################################
  
  # Extract significance of the trend
  trendPvalue <- as.numeric(summary[effectVar,]$`P Values`)
  
  if(trendPvalue < 0.05){ 
    signTrend <- "***"
  }else if (trendPvalue < 0.01){ 
    signTrend <- "**"
  }else if (trendPvalue < 0.001){ 
    signTrend <- "*"
  }else{ 
    signTrend <- "NS"
  }
  
  # x-position of the significance 
  xSignTrend <- dataTrend$var[nrow(dataTrend)] 
  
  # y-position of the significance
  ySignTrend <- dataTrend$trend[nrow(dataTrend)] - 0.1 *  dataTrend$trend[nrow(dataTrend)]
  
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
  newCoefs <- lapply(coefs, function(x){ paste0(ifelse(x > 0, "+", ""), x, "%") })
  
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
      
      annotate("text", label = "Abondance relative \nmoyenne", 
               x = min(years) + 1.5, y = 1.1, size = 4,
               fontface = 'italic', col = "darkgray") + 
      
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
                                         shape = Significatif,
                                         colour= col)) +
    
    # Add error bars to each point
    geom_errorbar(data = dataVariation, aes(x = get(effectVar),
                                            ymin = as.numeric(IC_inf), 
                                            ymax = as.numeric(IC_sup),
                                            colour = col)) +
    
    # Change visual theme
    theme_bw() +
    
    # Rotate x axis text
    theme(axis.text.x = element_text(angle = 45)) +
    
    # Change y axis limits
    ylim(c(0, yMax)) +
    
    # Rename x & y label
    xlab("Année") + ylab(yName) +
    
    # Add a title and subtitle 
    ggtitle(title, subtitle) +
    
    # 
    scale_x_continuous("Année", breaks = limits) +
    
    # Format significance shape legend 
    scale_shape_manual(name = "Ecart à l'abondance \nrelative moyenne", 
                       values = c(16, 17),
                       labels = c("Significatif", "Non significatif")) +
    
    # Format type of trend colour legend
    scale_colour_manual(name = "Tendance", values = c("orange", "red"), 
                        labels = c("Inter-annuelle\n(moyenne + IC)", "Annuelle"), 
                        guide = guide_legend(override.aes = list(linetype = c(1, 1),
                                                                 shape = c(16, NA),
                                                                 color = c("orange", "red"),
                                                                 size = c(1, 1.1))))
  return(plotGLM)
  
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
formatTrendPlot <- function(summary, effectVar, limits){
  
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
  trendVal <- trendVal / ROG^((length(limits)+1)/2)
  
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
#' @param summaryCat an `object` containing the summary of the regression model with the interesting variable as factor 
#' @param effectVar a `string` corresponding to the interesting variable in regard to the trend  
#' 
#' @return 
#' A dataframe containing outputs of a regression model, that is correctly formatted 
#' 
#' @example 
formatVariationPlot <- function(summaryCat, effectVar){
  
  # Filter summary for the requested effect
  summaryCat <- summaryCat[grep(effectVar,rownames(summaryCat)),]
  
  # Add an effect column
  summaryCat[,effectVar] <- rownames(summaryCat)
  
  # Reformat the effect column
  summaryCat[,effectVar] <- str_replace(string = summaryCat[,effectVar],
                                        pattern = paste0(effectVar, " : "),
                                        replacement = "")
  
  summaryCat[,effectVar] <- as.numeric(summaryCat[,effectVar])
  
  # Change level of factor "significant"
  summaryCat$Significatif <- factor(summaryCat$Significatif, c("Oui", "Non"))
  
  # Add a color column
  summaryCat$col <- rep("orange", nrow(summaryCat))
  
  return(summaryCat)
}
