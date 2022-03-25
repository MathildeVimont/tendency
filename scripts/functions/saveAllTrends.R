#' saveGlobalTrends
#'
#' A function that agregates all model results from different species into a single dataframe
#' No dataframe is returned, but it is saved as a .csv document
#' 
#' @param sp a `string` corresponding to the species considered
#' @param data a `data.frame` containing the abundance information
#' @param interestVar a `string` specifying the abundance column
#' @param annualMod an `object` containing results of the global regression
#' @param annualSum an `object` containing the formatted estimates of the global regression
#' @param coefs a `list` containing the global trends over the time period of the survey
#' @param path a `string` leading to the directory where the file should be saved
#' 
#' @return nothing
#'
#' @export
#'
#' @example 
saveGlobalTrends <- function(sp, data, interestVar, annualMod, 
                             annualSum, coefs, vif, path = NULL){
  
  #############################
  # IMPORT EXISTING DATAFRAME #
  #############################
  preData <- data.frame() 
  if(file.exists(paste0(path,"globalTrends.csv"))){
    preData <- read.csv(file = paste0(path, "globalTrends.csv"), row.names = 1)
  }
  
  ########################
  # INITIALIZE DATAFRAME #
  ########################
  dataTrend <- matrix(nrow = 1, ncol = 17)
  dataTrend <- as.data.frame(dataTrend)
  colnames(dataTrend) <- c("species", "nbObs", "nbYear", "totAbundance", "vif",
                           "intercept", "interceptInf", "interceptSup", "interceptPval",
                           "estimate", "estimateInf", "estimateSup", "estimatePval",
                           "trend", "trendInf", "trendSup", "significance")
  
  ##################
  # FILL DATAFRAME #
  ##################
  
  # Extract species names
  dataTrend$species <- sp
  
  # Extract number of observations site x year
  dataTrend$nbObs <- nrow(data)
  
  # Extract number of year
  dataTrend$nbYear <- max(data$year) - min(data$year)
  
  # Extract total abundance
  dataTrend$totAbundance <- sum(data[,interestVar])
  
  # Extract vif when available
  if(is.null(vif$error)){
    dataTrend$vif <- vif$value[,interestVar]
  }
  
  # Extract model outputs when convergence has been reached
  if(!identifyConvIssue(annualMod)){
    
    # Extract intercept and upper/lower value of its interval confidence
    dataTrend$intercept <- as.numeric(annualSum["(Intercept)",]$Estimates)
    dataTrend$interceptInf <- as.numeric(annualSum["(Intercept)",]$IC_inf)
    dataTrend$interceptSup <- as.numeric(annualSum["(Intercept)",]$IC_sup)
    dataTrend$interceptPval <- as.numeric(annualSum["(Intercept)",]$`P Values`)
    
    # Extract estimates and upper/lower value of its interval confidence
    dataTrend$estimate <- as.numeric(annualSum["year",]$Estimates)
    dataTrend$estimateInf <- as.numeric(annualSum["year",]$IC_inf)
    dataTrend$estimateSup <- as.numeric(annualSum["year",]$IC_sup)
    dataTrend$estimatePval <- as.numeric(annualSum["year",]$`P Values`)
    
    # Extract trend and upper/lower value of its interval confidence
    dataTrend$trend <- coefs$perc
    dataTrend$trendInf <- coefs$percInf
    dataTrend$trendSup <- coefs$percSup
    
    # Extract significance of the trend
    dataTrend$significance <- annualSum["year",]$Significatif
  }
  
  ###################
  # BIND DATAFRAMES #
  ###################
  
  globalTrends <- rbind(preData, dataTrend)
  
  ##################
  # SAVE DATAFRAME #
  ##################
  write.csv(x = globalTrends, file = paste0(path, "globalTrends.csv"))
}



#' saveYearlyVariations
#'
#' A function that agregates all model results from different species into a single dataframe
#' No dataframe is returned, but it is saved as a .csv document
#' 
#' @param sp a `string` corresponding to the species considered
#' @param data a `data.frame` containing the abundance information
#' @param interestVar a `string` specifying the abundance column
#' @param interAnnualMod an `object` containing results of the global regression
#' @param interAnnualSum an `object` containing the formatted estimates of the global regression
#' @param contr a `string` specifying the contrast chosen for the time variable
#' @param path a `string` leading to the directory where the file should be saved
#' 
#' @return nothing
#'
#' @example 
saveYearlyVariations <- function(sp, data, interestVar, interAnnualMod, 
                                 interAnnualSum, contr, effectVar = "year",
                                 path = NULL){
  
  #############################
  # IMPORT EXISTING DATAFRAME #
  #############################
  preData <- data.frame() 
  if(file.exists(paste0(path,"yearlyVariations.csv"))){
    preData <- read.csv(file = paste0(path, "yearlyVariations.csv"), row.names = 1)
  }
  
  ########################
  # INITIALIZE DATAFRAME #
  ########################
  
  # Extract the type of contrasts and thus the estimated levels of the effectVar
  if(is.na(contr)|contr %in% c("mean", min(data[, effectVar]))){
    eff <- (min(data[, effectVar])+1):max(data[, effectVar])
  }else if (contr == max(data[, effectVar])){
    eff <- min(data[, effectVar]):(max(data[, effectVar])-1)
  }else{
    eff <- c(min(data[, effectVar]):(as.numeric(contr) - 1),(as.numeric(contr) + 1):max(data[, effectVar]))
  }
  
  # Initialize the proper sized dataframe
  dataTrend <- matrix(nrow = length(eff) + 1, ncol = 11)
  dataTrend <- as.data.frame(dataTrend)
  
  # Initialize the column names
  colnames(dataTrend) <- c("species", "nbObs", "nbYear", "totAbundance", 
                           "contrast", effectVar, "estimate", "estimateInf", 
                           "estimateSup", "pval", "significance")
  
  ##################
  # FILL DATAFRAME #
  ##################
  
  # Extract species names
  dataTrend$species <- sp
  
  # Extract number of observations site x year
  dataTrend$nbObs <- nrow(data)
  
  # Extract number of year
  dataTrend$nbYear <- max(data[, effectVar]) - min(data[, effectVar])
  
  # Extract total abundance
  dataTrend$totAbundance <- sum(data[,interestVar])
  
  # Add the contrast information
  dataTrend$contrast <- ifelse(is.na(contr), min(data[, effectVar]), contr)
  
  # Add the contrast information
  dataTrend[,effectVar] <- c("intercept", eff)
  
  # Extract model outputs when convergence has been reached
  if(!is.null(interAnnualMod)&!identifyConvIssue(interAnnualMod)){
    
    # Extract position for each level of effectVar
    ind <- unlist(sapply(dataTrend[,effectVar], function(x) grep(x, tolower(rownames(interAnnualSum)))))
    
    # Add the estimate value
    dataTrend$estimate <- as.numeric(interAnnualSum[ind, "Estimates"])
    
    # Add the lower value of the confidence interval
    dataTrend$estimateInf <- as.numeric(interAnnualSum[ind, "IC_inf"])
    
    # Add the upper value of the confidence interval
    dataTrend$estimateSup <- as.numeric(interAnnualSum[ind, "IC_sup"])
    
    # Add the pvalues associated with each estimate
    dataTrend$pval <- as.numeric(interAnnualSum[ind, "P Values"])
    
    # Add the significance of each estimate
    dataTrend$significance <- interAnnualSum[ind, "Significatif"]
    
  }
  
  ###################
  # BIND DATAFRAMES #
  ###################
  
  yearlyVariations <- rbind(preData, dataTrend)
  
  ##################
  # SAVE DATAFRAME #
  ##################
  write.csv(x = yearlyVariations, file = paste0(path, "yearlyVariations.csv"))
}


#' reportConvergence
#'
#' A function that creates a text file specifying the convergence issues encountered
#' 
#' @param data a `data.frame` containing all information across all species
#' @param path a `string` specifying the folder where the report should be saved
#' 
#' @return nothing
#'
reportConvergence <- function(data, path){
  #####################
  # Import dataframes #
  #####################
  
  # Read dataframe containing global trends
  dataTrend <- read.csv(file = paste0(path, "globalTrends.csv"), row.names = 1)
  
  # Read dataframe containing yearly variations
  dataVar <-  read.csv(file = paste0(path, "yearlyVariations.csv"), row.names = 1)
  
  ##############################
  # Key indicators on the data #
  ##############################
  
  # Filter for unique information on site and year of observation
  dataSiteYear <- unique(data[,c("site", "year")])
  
  # Extract the number of visited sites x years 
  nbObs <- nrow(dataSiteYear)
  
  # Extract the mean number of years each site is visited 
  dataSite <- dplyr::group_by(dataSiteYear, site) %>% 
    summarise(totYear = n_distinct(year)) 
  
  meanSite = mean(dataSite$totYear)
  
  # Extract the mean number of sites visited each year
  dataYear <- group_by(dataSiteYear, year) %>% 
    summarise(totSite = n_distinct(site))
  
  meanYear = mean(dataYear$totSite)
  ###########################
  # Measures of convergence #
  ###########################
  
  # Extract the number of species going under study
  nbSpecies = nrow(dataTrend)
  
  # Extract the number of species that have a global trend
  nbConvTrend <- nrow(dataTrend[!is.na(dataTrend$intercept),])
  percConvTrend <- round(100 * nbConvTrend / nbSpecies, 2)
  
  # Extract the number of species that have yearly variations  
  nbConvVar <- length(unique(dataVar[!is.na(dataVar$estimate),]$species))
  percConvVar <- round(100 * nbConvVar / nbSpecies, 2)
  
  ####################################
  # Lists of non convergence species #
  ####################################
  
  # Extract species whose global trend has not been measured
  spNoTrend <- dataTrend[is.na(dataTrend$intercept),]$species
  
  # Extract species whose annual relative abondance have not been measured
  spNoVar <- unique(dataVar[is.na(dataVar$estimate),]$species)
  spNoVar <- spNoVar[-match(spNoTrend,spNoVar)]
  
  # Format vectors containing non converging species
  spNoTrend <- paste(spNoTrend, collapse = "\n")
  spNoVar <- paste(spNoVar, collapse = "\n")
  
  #############
  # Add title #
  #############
  fileReport = paste0(path, "reportOnConvergence.txt")
  
  cat("######################################", 
      file = fileReport, sep = "\n")
  cat("# Rapport sur le calcul de tendances #", 
      file = fileReport, sep = "\n", append = T)
  cat("######################################", 
      file = fileReport, sep = "\n", append = T)
  
  #######################
  # Give key indicators #
  #######################
  
  cat("# 1. Chiffres clés\n", file = fileReport, sep = "\n", append = T)
  
  cat(paste0("- Le calcul de tendances a été réalisé sur ", nbSpecies," espèces.\n"), 
      file = fileReport, sep = "\n", append = T)
  
  
  cat(paste0("- Pour réaliser ce calcul, on dispose de ", nbObs," observations (année x site)."), 
      file = fileReport, sep = " ", append = T)
  
  cat(paste0("Cela correspond en moyenne à ", round(meanYear, 0)," sites visités chaque année, et à des ",
             "sites visités en moyenne ", round(meanSite, 1)," années.\n"), 
      file = fileReport, sep = "\n", append = T)
  
  cat(paste0("- La tendance globale a pu être estimée pour : ", nbConvTrend,
             " espèces sur ", nbSpecies," (", percConvTrend,"%).\n"), 
      file = fileReport, sep = "\n", append = T)
  
  cat(paste0("- Les variations annuelles ont pu être estimées pour ", nbConvVar, 
             " espèces sur ", nbSpecies," (", percConvVar,"%).\n"), 
      file = fileReport, sep = "\n", append = T)
  
  ######################################################
  # Give the list of species that encountered an issue #
  ######################################################
  cat("# 2. Liste des espèces problématiques\n", file = fileReport, sep = "\n", append = T)
  
  cat("- Un problème de convergence a été rencontré lors de l'estimation de la tendance globale des espèces suivantes. Les variations d'abondance inter-annuelles n'ont pas été calculées pour ces epsèces :",
      file = fileReport, sep = "\n", append = T)
  
  cat(spNoTrend, file = fileReport, sep = "\n", append = T)
  
  cat("- Par ailleurs, un problème de convergence a été rencontré lors de l'estimation des variations annuelles d'abondance pour les espèces suivantes : ",
      file = fileReport, sep = "\n", append = T)
  
  cat(spNoVar, file = fileReport, sep = "\n", append = T)
  
  
}

