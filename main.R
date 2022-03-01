rm(list = ls())

######################################
# PARAMETERS : à vous de les changer #
######################################

# Chemin d'accès au package
dir = "C:/Users/STOC/Documents/Mathilde Vimont/Tendances/Code/partage/V1/"

# Quelle est la variable d'abondance à étudier ?
interestVar <- "abondance"

# Quels sont les effets fixes à considérer ?
fixedEffects <- c("year")

# Quels sont les effets aléatoires à considérer ?
randomEffects <- "site"

# Quels sont les effets emboîtés à considérer ?
nestedEffects <- NULL # list(c("transect","site"))

# Quelles sont les variables de contrôle (i.e, autres que l'année) catégorielles ?
factorVariables <- NULL

###########
# CONTEXT #
###########

# Création des répertoires généraux
resDir = paste0(dir, "results/")
scrDir = paste0(dir, "scripts/")
dataDir = paste0(dir, "data/")

# Création des répertoires de résultats
## Premier niveau
dir.create(path = paste0(resDir, "SPECIES/"), showWarnings = F)
dir.create(path = paste0(resDir, "GLOBAL/"), showWarnings = F)

## Deuxième niveau
dir.create(path = paste0(resDir, "GLOBAL/YEAR/"), showWarnings = F)
dir.create(path = paste0(resDir, "GLOBAL/MAP/"), showWarnings = F)
dir.create(path = paste0(resDir, "GLOBAL/TREND/"), showWarnings = F)

# Importation des librairies
source(paste0(scrDir, "basic/libraries.R"))

# Importation des fonctions
sapply(X = list.files(paste0(scrDir,"functions/"), pattern = "*.R"), 
       FUN = function(x) source(paste0(scrDir,"functions/", x), .GlobalEnv))

# Importation des paramètres généraux
source(paste0(scrDir, "basic/parameters.R"))

##################
# DATA TREATMENT #
##################

# Importation des données 
data <- importData(path = dataDir)

# Vérifier que les données contiennent bien toutes les variables 
check <- checkData(data, interestVar, fixedEffects, randomEffects)

if(check){
  # Vérification et complétion des absences dans les données
  # data <- fillAbsence(data = data, interestVar = interestVar, method = "once")
  
  # Choisir la distribution de manière automatique
  automaticDistrib <- detectDistrib(data, interestVar, distribution, zi)
  
  # Filtrer l'analyse sur une sous-liste d'espèce, si renseignée
  if(is.null(speciesList)){
    speciesList <- unique(unlist(data[, "species"]))
  }
  
  # Filtrer les données pour une plage temporelle, si renseignée
  if(!is.null(yearRange)){
    data <- data[data$year > yearRange[1] & data$year < yearRange[2],]
  }
  
  # Vérification de l'existence de coordonnées dans le jeu de données
  coord = FALSE
  if("longitude" %in% colnames(data) & "latitude" %in% colnames(data)){
    coord = TRUE
  }
  
  #################
  # DATA ANALYSIS #
  #################
  
  listAll <- list()
  for (sp in speciesList){
    
    cat("Species ", sp, " is going under analysis\n")
    
    # Création du répertoire dédié à l'espèce sp
    dir.create(path = paste0(resDir, "SPECIES/", sp), showWarnings = F)
    
    # Extraire les indices correspondant à l'espèce sp
    indSp <- which(data[,"species"] == sp)
    
    # Extraire les données issues de l'espèce sp
    dataSp <- data[indSp,]
    
    # Results
    rmarkdown::render(
      input  = paste0(scrDir, 'rmd/routine.Rmd'),
      output_dir = paste0(resDir, "SPECIES/", sp),
      output_file = "Analysis",
      params = list(sp = sp,
                    dataSp = dataSp,
                    interestVar = interestVar,
                    fixedEffects = fixedEffects,
                    randomEffects = randomEffects,
                    factorVariables = factorVariables,
                    distribution = automaticDistrib$distribution,
                    zi = automaticDistrib$zi,
                    nTry = nTry,
                    scaling = scaling,
                    coordinates = coord))
    
    saveAllTrends(listAll, path = paste0(resDir, "GLOBAL/"))
    
  }
  
}
