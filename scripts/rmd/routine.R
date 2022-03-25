
#############################
# STATISTIQUES DESCRIPTIVES #
#############################

# Distribution temporelle des observations

plotYear <- tempDistrPlot(dataSp, interestVar, sp, path = paste0(resDir, "GLOBAL/YEAR/"))

# Distribution spatiale des observations
if(isTRUE(coord)){
  plotMap <- makeMap(data = dataSp, type = "site", shape = "square",
                     interestVar = interestVar, sp = sp, 
                     path = paste0(resDir, "GLOBAL/MAP/"))
}

####################
# TENDANCE GLOBALE #
####################

cat("- Global trend model in process ...\n")

# Estimation de la tendance annuelle   
annualModel <- makeGLM(data = dataSp, 
                       interestVar = interestVar,
                       fixedEffects = fixedEffects,
                       randomEffects = randomEffects,
                       nestedEffects = nestedEffects,
                       factorVariables = factorVariables, 
                       distribution = automaticDistrib$distribution,
                       zi = automaticDistrib$zi,
                       nTry = nTry,
                       scaling = scaling)

# Calculer le vif pour le modèle annuel
vif <- catchConditions(measureVIF(model = annualModel))

# Formattage des sorties du modèle
annualSum <- summaryOutput(model = annualModel, 
                           distribution = automaticDistrib$distribution,
                           factorVariables = factorVariables,
                           transform = T, 
                           rescale = scaling, 
                           data = dataSp,
                           fixedEffects = fixedEffects)

# Calcul des coefficients de tendance
coefs <- analyseCoef(model = annualModel,
                     rescale = scaling,
                     data = dataSp,
                     distribution = automaticDistrib$distribution, 
                     effectVar = effectVar,
                     varRange = max(dataSp[,effectVar]) - min(dataSp[,effectVar]))

cat(ifelse(identifyConvIssue(annualModel), " --> CONVERGENCE ISSUE\n"," --> DONE\n"))

##############################
# VARIATIONS INTER-ANNUELLES #
##############################

cat("- Yearly variations model in process ...\n")

# Calcul des variations interannuelles  
interAnnualModel <- makeGLM(data = dataSp, 
                            interestVar = interestVar,
                            fixedEffects = fixedEffects,
                            randomEffects = randomEffects,
                            factorVariables = c(effectVar, factorVariables), 
                            distribution = automaticDistrib$distribution,
                            zi = automaticDistrib$zi,
                            contr = c(contrEffect, rep(NA, length(factorVariables))),
                            nTry = nTry,
                            scaling = scaling,
                            intercept = TRUE)

# Formattage des résultats de variations inter-annuelles  
interAnnualSum <- summaryOutput(model = interAnnualModel,
                                distribution = automaticDistrib$distribution, 
                                factorVariables = c(effectVar, factorVariables),
                                transform = TRUE,
                                rescale = scaling,
                                data = dataSp, 
                                fixedEffects = fixedEffects,
                                contr = c(contrEffect, rep(NA, length(factorVariables))))

cat(ifelse(identifyConvIssue(interAnnualModel), " --> CONVERGENCE ISSUE\n"," --> DONE\n"))

###########################
# VISUALISATION & SORTIES #
###########################

cat("- Graphical outputs\n")

# Graphique de la variation interannuelle de l'abondance
plotTrend <- plotGLM(summary = annualSum, modelCat = interAnnualModel, 
                     summaryCat = interAnnualSum, effectVar = effectVar, 
                     distribution = automaticDistrib$distribution, 
                     sp = sp, type = "relative", coefs = coefs, contr = contrEffect,
                     path = paste0(resDir, "GLOBAL/TREND/"))

# Sauvegarde de la tendance globale
saveGlobalTrends(sp = sp, data = dataSp, interestVar = interestVar, annualMod = annualModel,
                 annualSum = annualSum, coefs = coefs, path = paste0(resDir, "GLOBAL/"), vif = vif)

# Save yearly variations  
saveYearlyVariations(sp = sp, data = dataSp, interestVar = interestVar, 
                     interAnnualMod = interAnnualModel, interAnnualSum = interAnnualSum, 
                     contr = contrEffect, effectVar = effectVar, path = paste0(resDir, "GLOBAL/"))

cat(" --> DONE\n")
