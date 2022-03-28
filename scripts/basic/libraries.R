###############################
# Installation des librairies #
###############################

needRestore <- TRUE

while(needRestore){
  issue <- catchConditions(renv::restore(confirm = FALSE))
  needRestore = !is.null(issue$error)
}

# # Liste des packages requis
# listPackages <- c("lubridate", "tidyverse", "data.table", "stringr", 
#                   "glmmTMB", "DHARMa", "knitr", "kableExtra", 
#                   "flexdashboard", "maps")
# 
# # Installation des packages non installés
# packagesToInstall <- setdiff(listPackages, rownames(installed.packages()))
# 
# if(length(packagesToInstall) > 0){
#   install.packages(packagesToInstall)  
# }


##############################
# Importation des librairies #
##############################

# Traitement & Visualisation de données
library(lubridate)
library(tidyverse)
library(data.table)
library(stringr)
library(maps)

# GLM
library(glmmTMB)
library(DHARMa)

# R MarkDown
library(knitr)
library(kableExtra)
library(flexdashboard)
