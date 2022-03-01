####################################
# Paramètres : périmètre d'intérêt #
####################################

# Quelles espèces souhaitez-vous analyser ?
speciesList <- NULL

# Quelles sont les dates de début et de fin à prendre en compte ? 
# Format : c(dateDebut, dateFin)
yearRange <- NULL

#########################################
# Paramètres : méthodologie à appliquer #
#########################################

# Quelle hypothèse de distribution faire : gaussian, poisson, nbinom2, binomial ?
distribution <- NULL

# Doit-on centrer/réduire les variables numériques ?
scaling <- TRUE

# Dans le cas d'une distribution de poisson ou négative binomiale, doit-on faire un modèle zéro-enflé?
zi <- NULL # TRUE/FALSE

# Dans le cas d'une distribution binomiale, quel nombre d'essais (10 placettes = 10 essais) ?
nTry <- 10


