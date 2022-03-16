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
saveGlobalTrends <- function(sp, data, interestVar, annualMod, annualSum, coefs, path = NULL){
  
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
  dataTrend <- matrix(nrow = 1, ncol = 14)
  dataTrend <- as.data.frame(dataTrend)
  colnames(dataTrend) <- c("species", "nbObs", "nbYear", "totAbundance", 
                           "intercept", "interceptInf", "interceptSup",
                           "estimate", "estimateInf", "estimateSup",
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
  
  # Extract model outputs when convergence has been reached
  if(!identifyConvIssue(annualMod)){
    
    # Extract intercept and upper/lower value of its interval confidence
    dataTrend$intercept <- as.numeric(annualSum["(Intercept)",]$Estimates)
    dataTrend$interceptInf <- as.numeric(annualSum["(Intercept)",]$IC_inf)
    dataTrend$interceptSup <- as.numeric(annualSum["(Intercept)",]$IC_sup)
    
    # Extract estimates and upper/lower value of its interval confidence
    dataTrend$estimate <- as.numeric(annualSum["year",]$Estimates)
    dataTrend$estimateInf <- as.numeric(annualSum["year",]$IC_inf)
    dataTrend$estimateSup <- as.numeric(annualSum["year",]$IC_sup)
    
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
                                 interAnnualSum, contr, path = NULL){
  
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
  
  if(is.null(contr)|contr %in% c("mean", min(dataSp$year))){
    years <- (min(data$year)+1):max(data$year)
  }else if (contr == max(data$year)){
    years <- min(data$year):(max(data$year)+1)
  }else{
    years <- c(min(data$year):(as.numeric(contr)-1),(as.numeric(contr)+1):max(data$year))
  }
  
  colNames <- c("estimate", "estimateInf", "estimateSup", "estimateSign")
  
  dataTrend <- matrix(nrow = 1, ncol = 9 + 4 * length(years))
  dataTrend <- as.data.frame(dataTrend)
  colnames(dataTrend) <- c("species", "nbObs", "nbYear", "totAbundance", "contrast",
                           "intercept", "interceptInf", "interceptSup", "interceptSign",
                           paste0(rep(colNames, each = 1, times = length(years)), 
                                  rep(years, each = 4)))
  
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
  
  # Add the contrast information
  dataTrend$contrast <- ifelse(is.null(contr), min(data$year), contr)
  
  # Extract model outputs when convergence has been reached
  if(!is.null(interAnnualMod)|!identifyConvIssue(interAnnualMod)){
    
    # FOR INTERCEPT
    dataTrend[, "intercept"] <- as.numeric(interAnnualSum["(Intercept)","Estimates"])
    dataTrend[, "interceptInf"] <- as.numeric(interAnnualSum["(Intercept)","IC_inf"])
    dataTrend[, "interceptSup"] <- as.numeric(interAnnualSum["(Intercept)","IC_sup"])
    dataTrend[, "interceptSign"] <- interAnnualSum["(Intercept)", "Significatif"]
    
    # FOR ALL YEARS
    
    cols = sapply(years, function(x) grep(x, colnames(dataTrend)))
    rows = sapply(years, function(x) grep(x, rownames(interAnnualSum)))
    
    dataTrend[,cols[1,]] <- as.numeric(interAnnualSum[rows, "Estimates"])
    dataTrend[,cols[2,]] <- as.numeric(interAnnualSum[rows, "IC_inf"])
    dataTrend[,cols[3,]] <- as.numeric(interAnnualSum[rows, "IC_sup"])
    dataTrend[,cols[4,]] <- interAnnualSum[rows, "Significatif"]
    
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
#' @param speciesList a `vector` containing all information
#' @param path a `string` specifying the folder where the report should be saved
#' 
#' @return nothing
#'
reportConvergence <- function(path){
  #####################
  # Import dataframes #
  #####################
  
  # Read dataframe containing global trends
  dataTrend <- read.csv(file = paste0(path, "globalTrends.csv"), row.names = 1)
  
  # Read dataframe containing yearly variations
  dataVar <-  read.csv(file = paste0(path, "yearlyVariations.csv"), row.names = 1)
  
  ###########################
  # Measures of convergence #
  ###########################
  
  # Extract the number of species going under study
  nbSpecies = nrow(dataTrend)
  
  # Extract the number of species that have a global trend
  nbConvTrend <- nrow(dataTrend[!is.na(dataTrend$intercept),])
  percConvTrend <- round(100 * nbConvTrend / nbSpecies, 2)
  
  # Extract the number of species that have yearly variations  
  nbConvVar <- nrow(dataVar[!is.na(dataVar$intercept),])
  percConvVar <- round(100 * nbConvVar / nbSpecies, 2)
  
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
  
  cat(paste0("- La tendance globale a pu être estimée pour : ", nbConvTrend,
             " espèces sur ", nbSpecies," (", percConvTrend,"%).\n"), 
      file = fileReport, sep = "\n", append = T)
  
  cat(paste0("- Les variations annuelles ont pu être estimées pour ", nbConvVar, 
             " espèces sur ", nbSpecies," (", percConvVar,"%).\n"), 
      file = fileReport, sep = "\n", append = T)
  
  ######################################################
  # Give the list of species that encountered an issue #
  ######################################################
  cat("# 2. Liste des espèces problématiques", file = fileReport, sep = "\n", append = T)
  
  # cat("Un problème de convergence a été rencontré lors de l'estimation de la tendance globale des espèces suivantes : ", 
  #     file = fileReport, sep = "\n", append = T)
  # 
  # cat("Les variations d'abondance inter-annuelles n'ont pas été calculées pour ces epsèces.", 
  #     file = fileReport, sep = "\n", append = T)
  # 
  # cat("Par ailleurs, un problème de convergence a été rencontré lors de l'estimation des variations annuelles d'abondance pour les espèces suivantes : ", 
  #     file = fileReport, sep = "\n", append = T)
  # 
  # cat("Au final, X espèces sur Y (pp%) ont une tendance globale.", file = fileReport, sep = "\n", append = T)
  # 
  # cat("Et, X' espèces sur Y (p'p'%) ont des variations annuelles.", file = fileReport, sep = "\n", append = T)
  # 
  
  
}

