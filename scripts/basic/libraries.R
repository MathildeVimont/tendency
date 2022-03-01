###############################
# Installation des librairies #
###############################

# Liste des packages requis
listPackages <- c("lubridate", "tidyverse", "data.table", "stringr", 
                  "glmmTMB", "DHARMa", "knitr", "kableExtra", 
                  "flexdashboard", "maps")

# Installation des packages non installés
install.packages(setdiff(listPackages, rownames(installed.packages())))  

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
