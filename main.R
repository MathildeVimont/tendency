rm(list = ls())

######################################
# PARAMETERS : à vous de les changer #
######################################

# Chemin d'accès au package
dir = "C:/Users/STOC/Documents/Mathilde Vimont/Tendances/Code/git/tendency/"

# Quelle est la variable d'abondance à étudier ?
interestVar <- "abondance"

# Quels sont les effets fixes à considérer ?
fixedEffects <- c("year") 

# Quels sont les effets aléatoires à considérer ?
randomEffects <- c("site")

# Quels sont les effets emboîtés à considérer ?
nestedEffects <- list() # list(c("transect","site"))

# Quelles sont les variables de contrôle (i.e, autres que l'année) catégorielles ?
factorVariables <- NULL 

# Quelle est la variable indépendante dont vous souhaitez étudier l'effet ?
effectVar <- "year" 

# Quelle est le niveau de référence qui doit être considéré pour cette variable ?
contrEffect <- "mean" # ou 2001 ou 2010 ou NULL


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
       FUN = function(x) source(paste0(scrDir,"functions/", x), encoding = "UTF-8", .GlobalEnv))

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
    
    # Lancer la routine d'analyses
    source(paste0(scrDir, "rmd/routine.R"))
    
  }
  reportConvergence(data = data, path = paste0(resDir, "GLOBAL/"))
}
