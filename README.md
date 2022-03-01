README
================
Mathilde Vimont
01 mars, 2022

# I. Introduction

Cette suite d’outils R doit permettre d’analyser les données d’abondance
d’espèces, et d’extraire une tendance d’évolution dans le temps. Le
calcul de cette tendance passe notamment par des modèles de régression
linéaire généralisée mixte, détaillés dans la partie **III.4.** du
document.

Vous trouverez dans cette suite d’outils :

-   un ensemble de fonctions permettant la réalisation de ces types de
    modèle, la mise en forme des résultats et l’extraction des
    tendances, ainsi que la visualisation des variations d’abondance
    dans le temps ;

-   un script `main.R` permettant d’analyser en routine un ensemble
    d’espèces, et d’exporter proprement l’ensemble des résultats. Ce
    script sollicite un fichier `routine.Rmd`, qui exporte les résultats
    de l’analyse, y compris une analyse de la qualité du modèle et les
    problèmes de convergence éventuellement rencontrés.

La suite de ce README a pour objectif de documenter les différentes
fonctions mises en place dans ce package, et de donner des exemples
d’utilisation de ces fonctions.

# II. Installer les fonctions

Un certain nombre de packages sont nécessaires au bon fonctionnement des
calculs de tendance. Ils sont regroupés dans le fichier *librairies.R*,
dont le chemin est le suivant **scripts/basic** à partir de la racine
`rootDir`. NB : à vous de changer le chemin du dossier racine pour
pointer vers le dossier contenant le *main.R*.

``` r
rootDir <- "C:/Users/STOC/Documents/Mathilde Vimont/Tendances/Code/partage/V1/"

scrDir <- paste0(rootDir, "scripts/")
dataDir <- paste0(rootDir, "data/STERF/")

source(paste0(scrDir, "basic/libraries.R"))
```

En attendant que cet ensemble de fonctions soit contenu dans un package,
il est nécessaire de les charger une par une. La ligne de commande
suivante permet de les charger :

``` r
sapply(X = list.files(paste0(scrDir,"functions/"), pattern = "*.R"), 
       FUN = function(x) source(paste0(scrDir,"functions/", x), .GlobalEnv))
```

    ##         analyseCoef.R analyseZeros.R catchConditions.R checkData.R convIssue.R
    ## value   ?             ?              ?                 ?           ?          
    ## visible FALSE         FALSE          FALSE             FALSE       FALSE      
    ##         detectDistrib.R fillAbsence.R importData.R makeGLM.R makeMap.R
    ## value   ?               ?             ?            ?         ?        
    ## visible FALSE           FALSE         FALSE        FALSE     FALSE    
    ##         measureVIF.R plotDispersion.R plotGLM.R rescaleParam.R saveAllTrends.R
    ## value   ?            ?                ?         ?              ?              
    ## visible FALSE        FALSE            FALSE     FALSE          FALSE          
    ##         scaleData.R summaryOutput.R tempDistrPlot.R testDharma.R
    ## value   ?           ?               ?               ?           
    ## visible FALSE       FALSE           FALSE           FALSE       
    ##         testDispersion.R transformDistrib.R writeFormula.R
    ## value   ?                ?                  ?             
    ## visible FALSE            FALSE              FALSE

# III. Données d’abondance

## 1. Chargement des données

``` r
data <- importData(path = dataDir)
```

Les données sont donc maintenant disponibles dans R, sous le nom de :
`data`.

## 2. Format du jeu de données

``` r
head(data)
```

    ##            species       date month year count site transect latitude longitude
    ## 1 Lysandra coridon 2006-07-19     7 2006    12  419     1072 44.50569  5.646798
    ## 2 Lysandra coridon 2006-07-19     7 2006    12  426     1058 44.50184  5.586767
    ## 3 Lysandra coridon 2006-07-20     7 2006    12  539      758 48.39638  2.260800
    ## 4 Lysandra coridon 2006-07-20     7 2006    12  539      759 48.39460  2.258380
    ## 5 Lysandra coridon 2006-07-25     7 2006    12  541      740 48.38324  2.849093
    ## 6 Lysandra coridon 2006-07-26     7 2006    12  419     1075 44.50564  5.647210
    ##   code_habitat                    habitat                  ID
    ## 1            3 Pelouses, marais et landes 2006-07-19 419 1072
    ## 2            4          Milieux agricoles 2006-07-19 426 1058
    ## 3            3 Pelouses, marais et landes  2006-07-20 539 758
    ## 4            3 Pelouses, marais et landes  2006-07-20 539 759
    ## 5            3 Pelouses, marais et landes  2006-07-25 541 740
    ## 6            3 Pelouses, marais et landes 2006-07-26 419 1075

Ce jeu de données contient de nombreuses informations relatives aux
observations et conditions d’observation des espèces. Parmi celles
nécessaires au bon déroulé de l’analyse :

-   le champ `species` est obligatoire, et doit contenir le
    nom/l’identifiant de l’espèce observée ;

-   le champ `year` est obligatoire, et doit contenir l’année
    d’observation de l’espèce ;

-   le champ `transect` (ou `point`) est facultatif, et constitue
    l’échelle la plus fine d’observation. Elle est accompagnée d’un
    champ `site` obligatoire, qui est associé à un ou plusieurs
    `transect` (ou `point`) ;

-   le champ `ID` est obligatoire, et correspond à un identifiant unique
    **date + site (+ point/transect)** ;

-   les champs `longitude` et `latitude` sont facultatifs, mais leur
    présence dans le jeu de données permet de créer une carte de la
    répartition des sites en France.

*NB : aucun nom de colonne n’est imposé pour le champ contenant
l’information d’abondance. Ici, il s’agit de la colonne `count`.*

## 3. Gestion des absences

Les jeux de données d’abondance peuvent parfois être transmis sans
données d’absence, ou bien à cause du volume qu’elles représenteraient,
ou bien parce que le protocole ne prévoit pas l’enregistrement des
absences. La fonction `fillAbsence` a été imaginée pour reconstruire ces
absences, selon deux méthodes :

-   `method = "once"` reconstruit seulement les absences pour les sites
    où l’espèce a été vue au moins une fois ;

-   `method = "all"` reconstruit les absences pour tous les sites et
    années de visite, indépendamment de la présence de l’espèce.
    **&lt;!&gt; Pas encore implémenté**

``` r
dataAll <- fillAbsence(data = data, interestVar = "count", method = "once")
```

    ## Absences have been correctly filled with 0s

*NB : le bon fonctionnement de cette fonction nécessite à minima les
champs `year`, `site`, `species` et `ID`.*

``` r
# Nombre de données sans absence
nrow(data)
```

    ## [1] 949255

``` r
# Nombre de données avec absence
nrow(dataAll)
```

    ## [1] 1004222

# IV. Régression & tendance générale

Pour l’exemple, on s’intéresse particulièrement à l’espèce **Lysandra
coridon**.

``` r
dataSp <- dataAll[dataAll$species == "Lysandra coridon",]

# Nombre de données pour l'espèce
nrow(dataSp)
```

    ## [1] 8980

## 1. Choisir la variable d’intérêt et les variables explicatives

Une des premières étapes est d’identifier :

-   la variable que l’on cherche à modéliser i.e, la variable d’intérêt
    `interestVar`. Ici nous voulons modéliser l’abondance stockée dans
    le champ *count* ;

``` r
summary(dataSp$count)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.0000  0.0000  0.0000  0.9994  0.0000 12.0000

-   le(s) variable(s) que l’on souhaite considérer comme des effets
    fixes `fixedEffects` ? Ici, nous nous intéressons particulièrement à
    l’effet de l’année *year* sur l’abondance mais nous souhaitons aussi
    contrôler pour l’effet de la localisation au travers des champs
    *longitude* et *latitude* ;

``` r
# Year
summary(dataSp$year)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    2006    2010    2013    2013    2015    2020

``` r
# Longitude
summary(dataSp$longitude)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.2256  2.3426  3.9345  4.0208  6.1012  7.0518

``` r
# Latitude
summary(dataSp$latitude)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   43.23   43.94   47.09   46.27   48.39   50.00

-   le(s) variable(s) que l’on souhaite considérer comme des effets
    aléatoires `randomEffects` ? Ici, le *site* peut avoir une influence
    forte sur l’abondance perçue. Il s’agit d’une variable catégorielle
    à beaucoup de niveaux. La considérer comme un effet aléatoire nous
    permet d’une part de garder de la puissance dans le calcul de
    tendances, et d’autre part de généraliser les résultats à l’ensemble
    des sites de France.

``` r
table(dataSp$site)[1:10]
```

    ## 
    ## 134 399 400 401 410 412 417 419 423 424 
    ##   1   6 575 177  23  63 225  18   4  21

Par ailleurs, la question peut se poser d’utiliser ou non un *intercept*
dans la régression. Il est à noter qu’il est globalement conseillé de
laisser cet intercept, si on a pas de bonne raison de l’enlever, au
risque d’avoir des comportements inattendus du modèle. Deux raisons
notamment peuvent être valables pour le retirer :

1.  s’il y a une raison biologique justifiée à ce que la régression
    passe par 0 ;

2.  si nous souhaitons accéder à l’abondance moyenne de la catégorie de
    référence d’une variable explicative catégorielle.

Une fois identifiées ces différentes variables, la fonction
`writeFormula` permet d’écrire la formule de régression adaptée à ce
choix de variable, ce qui donne en l’occurrence :

``` r
writeFormula(interestVar=  "count",
             fixedEffects = c("year", "longitude", "latitude"),
             randomEffects = "site",  
             intercept = TRUE,
             nestedEffects = list(), # non encore implémenté
             interactions = list())  # non encore implémenté
```

    ## count ~ 1 + year + longitude + latitude + (1 | site)
    ## <environment: 0x0000000029db3130>

On pourrait également ajouter :

-   des interactions entre variables, quand on pense que l’effet d’une
    variable peut varier en fonction de la valeur d’une autre variable.
    C’est l’intérêt de l’argument `interactions`, mais dont les
    fonctionnalités n’ont pas encore été implémentées ;

-   des effets emboîtés, liés à l’échantillonnage. Par exemple, les
    points ne sont pas les mêmes d’un site à l’autre, ce qui pourrait
    donner un effet emboîté **point\|site**, que nous ne testerons pas
    ici. C’est l’intérêt de l’argument `nestedEffects`, mais dont les
    fonctionnalités n’ont pas encore été implémentées.

## 2. Vérifier que les données contiennent toutes les variables

Une fois que les variables à inclure dans la régression sont définies,
une étape simple de contrôle consiste à vérifier que toutes ces
variables sont bien contenues dans le jeu de données étudié. Cette
vérification peut être faite à partir de la fonction `checkData` :

``` r
checkData(dataSp, type = "transect", 
          interestVar=  "count",
          fixedEffects = c("year", "longitude", "latitude"), 
          randomEffects = "site")
```

    ## Your dataframe has proper column names
    ## You can proceed to further analysis !

    ## [1] TRUE

## 3. Choisir la distribution

Une des étapes importantes de la régression est le choix de la
**distribution des résidus**. Un régression linéaire classique repose
sur l’hypothèse selon laquelle les résidus de la régression suivent une
loi normale, ce qui est plutôt bien adapté à une variable à expliquer
continue.

Cependant, les données d’abondance peuvent être particulières : il
existe par exemple des données de *présence/absence* (0/1) ou encore des
données de *comptage*, comme c’est le cas dans notre jeu de données.
Cela pose la question de la pertinence de la distribution normale pour
ce type de variable.

Les régressions linéaires généralisées **(GLM)** permettent un ensemble
d’autres distributions, dont certaines ont été implémentées dans cet
outil :

-   la **distribution binomiale**, particulièrement adaptée aux données
    de présence/absence. *&lt;!&gt; non encore implémenté* ;

-   la **distribution de Poisson**, particulièrement adaptée aux données
    de comptage, mais qui impose néanmoins une contrainte très forte
    d’égalité entre moyenne et variance, ce qui peut entraîner des
    problèmes de sur-dispersion ;

-   la **distribution négative binomiale**, adaptée aux données de
    comptage et qui repose sur un paramètre de dispersion supplémentaire
    permettant de surmonter certains problèmes rencontrés avec la
    distribution de Poisson.

La fonction `detectDistrib` permet de proposer une distribution
pertinente à l’utilisateur, si celui-ci ne sait pas laquelle choisir.
Pour les données de comptage, la négative binomiale zéro-enflée
(intercept seul) est d’office proposée, pour gérer la sur-dispersion
courante et la présence d’un grand nombre de 0.

``` r
distribParam = detectDistrib(data = dataSp, interestVar = "count", 
                             distribution = NULL, zi = NULL)
 
# Quelle est la distribution choisie ? 
distribParam$distribution
```

    ## [1] "nbinom2"

``` r
# Besoin de faire un modèle zéro-englé ?
distribParam$zi
```

    ## [1] TRUE

## 4. Faire la régression

Une fois que l’on a formaté les données, et que l’on a déterminé les
variables et la distribution des résidus, la fonction `makeGLM` permet
de lancer la régression. Cette fonction repose sur le package `glmmTMB`,
qui a plusieurs avantages notamment :

-   l’implémentation de nombreuses distributions (dont la négative
    binomiale) ;

-   des temps de calcul plutôt faibles.

``` r
mod <- makeGLM(data = dataSp, 
               interestVar = "count", 
               fixedEffects = c("year","longitude","latitude"),
               randomEffects = "site", 
               distribution = "nbinom2",
               zi = TRUE,
               intercept = TRUE)  
```

La fonction doit retourner une liste de 3 éléments :

-   `mod$value`, donne les résultats de la régression si aucune erreur
    n’a été rencontrée pendant le processus d’estimation des paramètres
    ;

``` r
mod$value
```

    ## Formula:          count ~ 1 + year + longitude + latitude + (1 | site)
    ## Zero inflation:         ~1
    ## Data: data
    ##      AIC      BIC   logLik df.resid 
    ## 16120.60 16170.32 -8053.30     8973 
    ## Random-effects (co)variances:
    ## 
    ## Conditional model:
    ##  Groups Name        Std.Dev.
    ##  site   (Intercept) 1.138   
    ## 
    ## Number of obs: 8980 / Conditional model: site, 92
    ## 
    ## Dispersion parameter for nbinom2 family (): 1.58 
    ## 
    ## Fixed Effects:
    ## 
    ## Conditional model:
    ## (Intercept)         year    longitude     latitude  
    ##   141.76902     -0.07040     -0.09848      0.01862  
    ## 
    ## Zero-inflation model:
    ## (Intercept)  
    ##      0.8771

-   `mod$warnings`, liste l’ensemble des alertes rencontrées pendant le
    processus d’estimation des paramètres. Si une des erreurs concerne
    un **problème de convergence**, la fiabilité du modèle est à
    remettre en question ;

``` r
mod$warnings
```

    ## NULL

-   `mod$error`, liste l’ensemble des erreurs rencontrées pendant le
    processus d’estimation des paramètres.

``` r
mod$error
```

    ## NULL

Le processus d’estimation des paramètres peut être perturbé par la
présence de paramètres numériques avec des plages de valeurs importantes
et variables d’un paramètre à l’autre (ex : température entre 0 et 40°C
vs. année entre 2000 et 2020). Pour cela, une astuce simple peut être de
centrer/réduire les variables numériques. Cela est possible dans la
fonction `makeGLM` au travers du paramètre `scaling = TRUE`.

``` r
modScaled <- makeGLM(data = dataSp, 
                     interestVar = "count", 
                     fixedEffects = c("year","longitude","latitude"),
                     randomEffects = "site", 
                     distribution = "nbinom2",
                     zi = TRUE,
                     intercept = TRUE,
                     scaling = TRUE)
```

## 5. Formatter les résultats

Une fois la régression réalisée, nous souhaitons extraire les résultats
(estimations, erreurs, p-value, …) dans un format plus lisible pour
l’utilisateur. La fonction `summaryOutput` doit permettre ce reformatage
et contient un certain nombre de paramètres qui permettent d’affiner
plus ou moins la transformation :

-   le paramètre `rescale` permet de dé-centrer/réduire les coefficients
    et erreurs standards. Ce paramètre n’a de sens que si les variables
    explicatives numériques ont été centrées-réduites dans un premier
    temps lors de la régression. Avec ce paramètre mis à `TRUE`, doivent
    être fournis le **jeu de données initial** (paramètre `data`) ainsi
    que l’ensemble des **effets fixes** sélectionnés (paramètre
    `fixedEffects`);

-   le paramètre `transform` permet de transformer les coefficients
    lorsque la fonction de lien est différente de l’identité (i.e, quand
    il s’agit d’un log ou logit). C’est le cas pour toutes les
    distributions autres que la gaussienne.

Les estimations brutes sont regroupées dans le tableau suivant. Elles
sont difficiles à interpréter car les variables ont été initialement
centrées-réduites.

``` r
summaryOutput(model = modScaled$value, 
              distribution = "poisson",
              transform = FALSE, 
              rescale = FALSE)
```

    ##             Estimates Standard Errors    IC_inf    IC_sup P Values Significant
    ## (Intercept)  5.46e-01        1.40e-01  2.71e-01  8.20e-01 9.95e-05         Yes
    ## year        -2.63e-01        3.17e-02 -3.25e-01 -2.01e-01 8.97e-17         Yes
    ## longitude   -1.85e-01        1.62e-01 -5.02e-01  1.33e-01 2.54e-01          No
    ## latitude     4.00e-02        1.84e-01 -3.21e-01  4.01e-01 8.28e-01          No

Les estimations dé-centrées/réduites sont regroupées dans le tableau
suivant. Elles sont à interpréter comme suit : *quand la variable year
augmente de 1, le **log** de l’abondance augmente de **-0.0156***.

``` r
summaryOutput(model = modScaled$value, 
              distribution = "poisson",
              transform = FALSE, 
              rescale = TRUE,
              data = dataSp,
              fixedEffects = c("year","longitude","latitude"))
```

    ##             Estimates Standard Errors    IC_inf    IC_sup P Values Significant
    ## (Intercept)  3.82e+01       -6.45e+00  5.09e+01  2.56e+01 9.95e-05         Yes
    ## year        -7.04e-02        8.46e-03 -8.70e-02 -5.38e-02 8.97e-17         Yes
    ## longitude   -9.85e-02        8.63e-02 -2.68e-01  7.07e-02 2.54e-01          No
    ## latitude     1.86e-02        8.57e-02 -1.49e-01  1.87e-01 8.28e-01          No

Les estimations dé-centrées/réduites et retransformées sont regroupées
dans le tableau suivant. Elles sont à interpréter comme suit : *quand la
variable year augmente de 1, l’abondance est multipliée par **0.943***.

``` r
summaryOutput(model = modScaled$value, 
              distribution = "poisson",
              transform = TRUE, 
              rescale = TRUE,
              data = dataSp,
              fixedEffects = c("year","longitude","latitude"))
```

    ##             Estimates Standard Errors   IC_inf   IC_sup P Values Significant
    ## (Intercept)  4.01e+16        1.59e-03 1.23e+22 1.31e+11 9.95e-05         Yes
    ## year         9.32e-01        1.01e+00 9.17e-01 9.48e-01 8.97e-17         Yes
    ## longitude    9.06e-01        1.09e+00 7.65e-01 1.07e+00 2.54e-01          No
    ## latitude     1.02e+00        1.09e+00 8.61e-01 1.21e+00 8.28e-01          No

## 6. Extraire la tendance

Nous cherchons à savoir plus précisément comment a évolué l’abondance de
l’espèce entre la première et la dernière année d’observation. La
fonction `analyseCoef` permet de calculer deux indicateurs intéressants
: la tendance `trend` et le pourcentage de variation `perc`, dont
l’interprétation est explicitée par la suite. Elle permet aussi
d’extraire les bornes inférieure `percInf` et supérieure `percSup` de
l’intervalle de confiance autour du pourcentage de variation.

``` r
varYear <- max(dataSp$year) - min(dataSp$year) + 1

coefficients <- analyseCoef(model = modScaled$value,
                            rescale = T, 
                            data = dataSp,
                            distribution = "poisson",
                            effectVar = "year",
                            varRange = varYear)
```

Entre la première et la dernière année, l’abondance a été multipliée par
:

``` r
coefficients$trend
```

    ## [1] 0.35

Entre la première et la dernière année, l’abondance a évolué de :

``` r
paste(coefficients$perc, "%")
```

    ## [1] "-65.22 %"

L’intervalle de confiance autour de cette estimation est le suivant :

``` r
paste(coefficients$percInf,"/", coefficients$percSup, "%")
```

    ## [1] "-72.88 / -55.39 %"

# V. Régression & variations annuelles

## 1. Régression

Dans la partie précédente, nous avons calculé la tendance globale
d’évolution de l’abondance. Nous cherchons maintenant à étudier les
variations d’abondance qui peuvent exister d’une année sur l’autre. Pour
cela, nous pouvons réaliser une régression où la variable **year** est
cette fois considérée comme une **variable catégorielle**. Cela est
rendu possible par le paramètre `factorVariables` dans la fonction
`makeGLM` qui permet de déclarer toutes les variables qui doivent être
traitées comme des variables catégorielles.

``` r
modCat <- makeGLM(data = dataSp, 
                  interestVar = "count", 
                  fixedEffects = c("year", "longitude", "latitude"),
                  randomEffects = "site",
                  factorVariables = "year", 
                  distribution = "nbinom2",
                  zi = TRUE,
                  scaling = T)
```

Dans ce cadre, les coefficients associés à chaque année permettront de
voir les variations d’abondance moyenne par rapport à l’année de
référence qu’est la première année. Cela a plusieurs conséquences :

-   le coefficient associé à l’année de référence, ici la première
    année, est estimé à 0, et n’a pas d’erreurs standards associées ;

-   la cohérence de l’estimation et des erreurs associées à chaque année
    dépend grandement du nombre d’observations pour l’année de
    référence, ce qui peut poser problème dans un contexte où la
    première année n’est pas forcément une bonne candidate pour l’effort
    d’observation ;

-   les coefficients ne correspondent par à une abondance moyenne pour
    chaque année, mais bien à un écart d’abondance par rapport à l’année
    de référence.

``` r
summaryCat <- summaryOutput(model = modCat$value, 
                            distribution = "nbinom2", 
                            factorVariables = "year", 
                            transform = TRUE, 
                            rescale = TRUE,
                            data = dataSp, 
                            fixedEffects = c("year", "longitude", "latitude"))

summaryCat
```

    ##             Estimates Standard Errors   IC_inf   IC_sup P Values Significant
    ## (Intercept)  3.88e+00        1.49e-01 1.62e+02 9.30e-02 1.12e-05         Yes
    ## year : 2007  1.05e+00        1.16e+00 7.80e-01 1.41e+00 7.48e-01          No
    ## year : 2008  1.08e+00        1.16e+00 8.15e-01 1.43e+00 5.88e-01          No
    ## year : 2009  1.05e+00        1.14e+00 8.06e-01 1.37e+00 7.15e-01          No
    ## year : 2010  1.37e+00        1.15e+00 1.04e+00 1.81e+00 2.70e-02         Yes
    ## year : 2011  9.31e-01        1.14e+00 7.17e-01 1.21e+00 5.92e-01          No
    ## year : 2012  9.50e-01        1.14e+00 7.40e-01 1.22e+00 6.88e-01          No
    ## year : 2013  9.96e-01        1.14e+00 7.68e-01 1.29e+00 9.77e-01          No
    ## year : 2014  7.33e-01        1.14e+00 5.69e-01 9.45e-01 1.65e-02         Yes
    ## year : 2015  7.90e-01        1.15e+00 6.04e-01 1.03e+00 8.49e-02          No
    ## year : 2016  7.07e-01        1.16e+00 5.26e-01 9.51e-01 2.21e-02         Yes
    ## year : 2017  4.77e-01        1.18e+00 3.48e-01 6.55e-01 4.69e-06         Yes
    ## year : 2018  5.45e-01        1.20e+00 3.81e-01 7.79e-01 8.67e-04         Yes
    ## year : 2019  3.55e-01        1.23e+00 2.37e-01 5.31e-01 4.70e-07         Yes
    ## year : 2020  3.33e-01        1.26e+00 2.12e-01 5.23e-01 1.76e-06         Yes
    ## longitude    8.89e-01        1.09e+00 7.47e-01 1.06e+00 1.82e-01          No
    ## latitude     9.84e-01        1.09e+00 8.29e-01 1.17e+00 8.52e-01          No

NB : lorsqu’il n’y a qu’une seule variable catégorielle dans le modèle,
comme c’est le cas ici, on peut facilement accéder à l’abondance moyenne
de chaque année en retirant l’intercept du modèle.

``` r
modCatNoInt <- makeGLM(data = dataSp, 
                  interestVar = "count", 
                  fixedEffects = c("year", "longitude", "latitude"),
                  randomEffects = "site",
                  factorVariables = "year", 
                  distribution = "nbinom2",
                  zi = TRUE,
                  scaling = TRUE,
                  intercept = FALSE)

summaryCatNoInt <- summaryOutput(model = modCatNoInt$value, 
                                 distribution = "nbinom2", 
                                 factorVariables = "year", 
                                 transform = TRUE, 
                                 rescale = TRUE,
                                 data = dataSp, 
                                 fixedEffects = c("year", "longitude", "latitude"))
summaryCatNoInt
```

    ##             Estimates Standard Errors   IC_inf   IC_sup P Values Significant
    ## year : 2006  2.12e+00        1.19e+00 1.52e+00 2.96e+00 1.12e-05         Yes
    ## year : 2007  2.23e+00        1.20e+00 1.56e+00 3.17e+00 9.68e-06         Yes
    ## year : 2008  2.29e+00        1.19e+00 1.63e+00 3.23e+00 2.07e-06         Yes
    ## year : 2009  2.23e+00        1.18e+00 1.60e+00 3.10e+00 2.07e-06         Yes
    ## year : 2010  2.90e+00        1.18e+00 2.09e+00 4.03e+00 1.99e-10         Yes
    ## year : 2011  1.97e+00        1.18e+00 1.43e+00 2.72e+00 2.95e-05         Yes
    ## year : 2012  2.01e+00        1.17e+00 1.47e+00 2.75e+00 1.16e-05         Yes
    ## year : 2013  2.11e+00        1.17e+00 1.54e+00 2.89e+00 3.37e-06         Yes
    ## year : 2014  1.55e+00        1.18e+00 1.13e+00 2.13e+00 6.31e-03         Yes
    ## year : 2015  1.67e+00        1.18e+00 1.21e+00 2.32e+00 2.01e-03         Yes
    ## year : 2016  1.50e+00        1.19e+00 1.06e+00 2.12e+00 2.18e-02         Yes
    ## year : 2017  1.01e+00        1.20e+00 7.05e-01 1.45e+00 9.48e-01          No
    ## year : 2018  1.16e+00        1.23e+00 7.72e-01 1.73e+00 4.83e-01          No
    ## year : 2019  7.52e-01        1.25e+00 4.86e-01 1.16e+00 2.00e-01          No
    ## year : 2020  7.07e-01        1.28e+00 4.38e-01 1.14e+00 1.54e-01          No
    ## longitude    8.89e-01        1.09e+00 7.47e-01 1.06e+00 1.82e-01          No
    ## latitude     9.84e-01        1.09e+00 8.29e-01 1.17e+00 8.52e-01          No

## 2. Visualisation

Maintenant que nous avons récupéré les coefficients associés à chaque
année, nous aimerions les représenter sur un graphique pour visualiser
les variations d’abondance au cours du temps. Pour cela, la fonction
`plotGLM` permet de représenter sur le même graphique :

-   les coefficients associés à chaque année (point) ;

-   les intervalles de confiance des estimations (barres d’erreur) ;

-   la significativité de la différence entre l’abondance moyenne à
    l’année X et l’abondance à l’année de référence (triangle vs. rond).

``` r
plotGLM(summary = summaryCat, effect = "year", distribution = "nbinom2", type = "relative")
```

![](README_files/figure-gfm/plotGLM-1.png)<!-- -->

On peut aussi décider de représenter les variations d’abondance absolue
:

``` r
plotGLM(summary = summaryCatNoInt, effect = "year", distribution = "nbinom2", type = "absolute")
```

![](README_files/figure-gfm/plotGLMNoInt-1.png)<!-- -->

# VI. Contrôle de la qualité du modèle

Un certain nombre de contrôles doivent être effectués afin de s’assurer
de la bonne qualité du modèle et donc des estimations. Ils sont
détaillés dans cette section.

## 1. VIF

La multi-colinéarité est un phénomène qui traduit le fait qu’une des
variables explicatives est **linéairement liée aux autres variables**.
Cela rend les paramètres estimés pour ces variables instables, et peut
même cacher un effet significatif de ces variables.

La fonction `measureVIF` permet de calculer le **VIF (Variance Inflation
Factor)**, qui est un indicateur souvent utilisé dans le cadre de la
multi-colinéarité.

``` r
vif <- measureVIF(model = modScaled$value)

vif
```

    ##     year longitude latitude
    ## VIF    1      1.73     1.72

Il n’existe pas de consensus autour d’une valeur seuil : 2 est très
conservatif, 10 très peu conservatif, 5 est une valeur intermédiaire que
nous recommadons ici, mais l’utilisateur est libre de gérer la
multi-colinéarité comme il le souhaite.

NB : dans certains cas, une forte colinéarité n’est pas forcément un
problème, c’est le cas quand il s’agit :

-   de variables contrôles dont l’effet ne nous intéresse pas, et
    qu’elles ne sont pas colinéaires à une variable d’intérêt, elles
    peuvent être laissées dans le modèle ;

-   de variables catégorielles avec beaucoup de niveaux, dont un avec
    peu d’observations ;

-   de variables “polynomiales” (ex : longitude et longitude^2).

Si par contre une variable est fortement colinéaire avec une variable
explicative d’intérêt, il est **judicieux de la retirer du modèle**.

## 2. Dispersion des résidus et zéro-inflation

### a) Quand et pourquoi s’y intéresser ?

Dans le cadre d’une régression où la dispersion des résidus n’est pas
présumée normale (e.g binomiale ou de poisson), un des éléments à
regarder est la **dispersion des résidus**. En effet, dans le cadre de
ces distributions alternatives, une hypothèse forte est faite sur la
variance des résidus :

-   pour la distribution de Poisson, la variance des résidus doit être
    égale à la moyenne des résidus : *moyenne(résidus) = Var(résidus)* ;

-   pour la régression binomiale, la variance est définie par rapport à
    la probabilité du succès (i.e, dans notre cas la probabilité
    d’osberver l’espèce) : *Var(résidus) = np(1-p)*.

Ainsi une variance très supérieure à sa valeur théorique est qualifiée
de **sur-dispersion des résidus**. Cette sur-dispersion est
problématique, car elle est associée à une sous-estimation des erreurs
standard et à une estimation de p-value trop faibles. En bref, des
effets peuvent paraître significatifs alors qu’ils ne le sont pas
vraiment !

Dans le cas des données de comptage, cette sur-dispersion peut aller de
pair avec un phénomène appelé en anglais *zero-inflation*, qui
correspond à une sur-représentation des 0 dans le jeu de données.

### b) Identifier un problème dans le modèle

Pour se doner une idée empirique de la distribution théorique que
devraient suivre les données si elles suivaient parfaitement une loi de
Poisson (resp. binomiale), nous avons implémenté la fonction
`plotDispersion` :

``` r
plotDispersion(data = dataSp, interestVar = "count", distribution = "nbinom2")
```

![](README_files/figure-gfm/plotDisp-1.png)<!-- -->

Qui doit permettre de mettre en évidence le double problème de
sur-dispersion et sur-représentation des 0.

NB : la fonction `testDispersion` permet de se faire une idée d’un
éventuel problème de dispersion dans le modèle, mais elle ne fonctionne
que dans le cas où il n’y a pas de partie zero-enflée dans le modèle

``` r
disp <- testDispersion(model = modScaled$value)
disp
```

Les sorties de la fonctionne comprennent une colonne `ratio` qui permet
de caractériser la dispersion des résidus :

-   si ratio &gt; 1, alors les résidus sont sur-dispersés ;

-   si ratio &lt; 1, alors les résidus sont sous-dispersés.

En plus de cela, la colonne `p` permet de vérifier si cette différence
par rapport à 1 est significative ou non.

``` r
# Ratio de dispersion
disp$Ratio

# Probabilité qu'il y ait sous/sur-dispersion 
disp$`P Values`
```

### c) Quelles causes et solutions ?

La sur-dispersion des résidus peut être liée à différents phénomènes
notamment :

-   l’oubli de certaines variables explicatives dans le modèle ;

-   la présence de valeurs extrêmes ;

-   la présence d’un grand nombre de 0 dans le jeu de données, aussi
    appelée **zero-inflation** (cf. VI.3).

Parmi les solutions qui existent pour résoudre (au moins partiellement
ce problème), on trouve :

-   le changement de distribution (troquer *poisson contre négative
    binomiale* ou *binomial contre quasi-binomial*), qui permet de
    rajouter de la flexibilité dans l’écart entre moyenne et variance.
    Il s’agit d’une des solutions les plus simples pour améliorer la
    qualité du modèle dans un cas de sur-dispersion des résidus ;

-   la prise en compte des 0 dans le modèle, au travers l’utilisation
    d’un modèle dit *zero-inflated*, qui combine (1) une régression
    binomiale, pour gérer les absences vs. les présences et (2) une
    régression de Poisson ou négative-binomiale, pour gérer les
    variations d’abondance.

**&lt;!&gt; Le choix par défaut pour les données de comptage, est de
réaliser une régression avec une distribution négative-binomiale et une
partie zéro-enflée.**

## 3. Auto-corrélation spatiale

\[ A VENIR \]

# VII. Fonction annexe : gérer les erreurs

Lors d’analyses en routine sur un grand nombre d’espèces, des erreurs
peuvent se produire en cours de route et arrêter les analyses. Pour
maintenir les analyses, tout en gardant une traçe des erreurs / alertes,
la fonction `catchConditions` permet d’encapsuler des fonctions
potentiellement instables et d’extraire les alertes ou erreurs
rencontrées. C’est le cas dans `makeGLM`, dans laquelle la fonction
`glmmTMB` y est encapsulée.

Construisons d’abord une fonction simple, qui peut présenter des alertes
ou des warnings :

``` r
# Fonction permettant de calculer la somme de x et y
fun <- function(x, y){
  
  # Alerte si l'une des deux variables est NA
  if(is.na(x)|is.na(y)){
    warning("NAs will be produced")
  }
  else{
    # Erreur si l'une des deux variables n'est pas numérique
    if(!is.numeric(x)|!is.numeric(y)){
      stop("x and y should be numeric values")
    }
  }
  
  return(x + y)
}
```

Le premier exemple montre le résultat de l’encapsulation lorsque le
déroulé de la fonction se produit sans alerte ni erreur : dans ce cas
là, seul le champ `value` renvoie une valeur non `NULL`, qui correspond
bien au résultat de la fonction :

``` r
catchConditions(expr = fun(x = 5, y = 2))
```

    ## $value
    ## [1] 7
    ## 
    ## $warnings
    ## NULL
    ## 
    ## $error
    ## NULL

Le deuxième exemple montre le résultat de l’encapsulation lorsqu’une
alerte se produit pendant le déroulé de la fonction : dans ce cas là, le
champ `value` renvoie une valeur qui correspond au résultat de la
fonction, et le champ `warnings` renvoie le message d’alerte affiché
pendant la procédure :

``` r
catchConditions(expr = fun(x = 5, y = NA))
```

    ## $value
    ## [1] NA
    ## 
    ## $warnings
    ## [1] "NAs will be produced"
    ## 
    ## $error
    ## NULL

Le dernier exemple montre le résultat de l’encapsulation lorsqu’une
erreur se produit pendant le déroulé de la fonction : dans ce cas là,
seul le champ `error` est non `NULL`, et contient le message d’erreur
affiché pendant la procédure.

*NB : le résultat ne peut pas être calculé, pour autant l’erreur ne
stoppe plus le processus !*

``` r
catchConditions(expr = fun(x = 5, y = "2"))
```

    ## $value
    ## NULL
    ## 
    ## $warnings
    ## NULL
    ## 
    ## $error
    ## [1] "x and y should be numeric values"
