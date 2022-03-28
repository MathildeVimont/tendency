#' reportConvergence
#'
#' A function that creates a text file :
#' - describing key information on the estimation process
#' - specifying the convergence issues encountered
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
  
  if (length(spNoTrend) > 0){
    spNoVar <- spNoVar[-match(spNoTrend,spNoVar)] 
  }
  
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

