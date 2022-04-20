README
================
Mathilde Vimont
20 avril, 2022

# Avant-propos

Cette suite d’outils R doit permettre d’analyser les données d’abondance
d’espèces, et d’extraire une tendance d’évolution dans le temps. Le
calcul de cette tendance passe notamment par des modèles de régression
linéaire généralisée mixte. Un ensemble de fonctions a ainsi été
implémenté pour réaliser ce type de modèles, mettre en fome les
résultats et créer un certain nombre de visualisations des données et
des résultats.

La suite de ce README a un double objectif :

-   présenter la routine d’analyses de tendance implémentée dans les
    fichiers `main.R` et `routine.R`, pouvant être utilisée par
    l’utilisateur pour tout calcul de tendances sur un ensemble
    d’espèces ;

-   documenter les différentes fonctions mises en place dans ce projet
    et l’ensemble de leurs fonctionnalités, en donnant des exemples
    concrets de leur utilisation.

# I. Installer l’environnement

## 1. Répertoires

L’outil est organisé de sorte que chaque aspect du projet soit contenu
dans un répertoire spécifique. Le répertoire racine où se trouve le
README, doit être renseigné par l’utilisateur sous le nom de `rootDir`.
L’ensemble des scripts peuvent être trouvés dans le répertoire
**scripts**, dont le chemin sera nommé `scrDir` dans la suite et
l’ensemble des données à analyser doivent être placées dans le
répertoire **data** dont le chemin sera contenu dans `dataDir`.

``` r
rootDir <- "C:/Users/mvimont01/Documents/MNHN/Tendances/Code/tendency/"

scrDir <- paste0(rootDir, "scripts/")
dataDir <- paste0(rootDir, "data/")
```

## 2. Fonctions

L’ensemble des fonctions mises en place dans cet outil sont accessibles
au niveau du chemin suivant **scripts/functions** à partir de la racine
`rootDir`. Le bloc de code suivant permet de charger l’ensemble de ces
fonctions.

``` r
sapply(X = list.files(paste0(scrDir,"functions/"), pattern = "*.R"), 
       FUN = function(x) source(paste0(scrDir,"functions/", x), .GlobalEnv))
```

    ##         addMissingEstimate.R affectCatEBCC.R analyseCoef.R analyseZeros.R
    ## value   ?                    ?               ?             ?             
    ## visible FALSE                FALSE           FALSE         FALSE         
    ##         catchConditions.R checkData.R detectDistrib.R fillAbsence.R
    ## value   ?                 ?           ?               ?            
    ## visible FALSE             FALSE       FALSE           FALSE        
    ##         identifyConvIssue.R importData.R makeGLM.R makeMap.R measureVIF.R
    ## value   ?                   ?            ?         ?         ?           
    ## visible FALSE               FALSE        FALSE     FALSE     FALSE       
    ##         plotDispersion.R plotGLM.R reportConvergence.R rescaleParam.R
    ## value   ?                ?         ?                   ?             
    ## visible FALSE            FALSE     FALSE               FALSE         
    ##         saveGlobalTrends.R saveYearlyVariations.R scaleData.R setContrasts.R
    ## value   ?                  ?                      ?           ?             
    ## visible FALSE              FALSE                  FALSE       FALSE         
    ##         summaryOutput.R tempDistrPlot.R testDharma.R testDispersion.R
    ## value   ?               ?               ?            ?               
    ## visible FALSE           FALSE           FALSE        FALSE           
    ##         transformDistrib.R writeFormula.R
    ## value   ?                  ?             
    ## visible FALSE              FALSE

## 3. Librairies

Un certain nombre de packages sont nécessaires au bon fonctionnement des
calculs de tendance. Pour éviter tout problème lié aux versions des
packages, toute première utilisation de cet outil nécessite
l’installation des packages aux versions telles que spécifiées dans le
fichier `renv.lock` via l’utilisation de la commande suivante :

``` r
needRestore <- TRUE

while(needRestore){
  issue <- catchConditions(renv::restore())
  needRestore <- !is.null(issue$error)
}
```

Cette commande peut **prendre du temps** à tourner. Une fois terminée,
il faut éteindre R / RStudio pour que l’environnement soit correctement
installé. Une fois R éteint et rallumé, et dans le cas où l’utilisateur
ne souhaite pas utiliser la routine de calcul décrite en section
**III**, les étapes précédentes doivent être réitérées, et les packages
chargés. Les packages nécessaires au bon fonctionnement du calcul de
tendances sont regroupés dans le fichier *librairies.R* dont le chemin
est le suivant **scripts/basic** à partir de la racine `rootDir`. La
commande suivante permet de charger l’ensemble des packages :

``` r
source(paste0(scrDir, "basic/libraries.R"))
```

    ## * The library is already synchronized with the lockfile.

## 4. Données

L’ensemble des données doit être déposé dans le répertoire **data**.
Elles peuvent contenir de nombreuses informations relatives aux
observations et conditions d’observation des espèces. Parmi celles
nécessaires au bon déroulé de l’analyse :

-   le champ `species` est **obligatoire**, et doit contenir le
    nom/l’identifiant de l’espèce observée. **Attention**, il est
    préférable d’éviter tout caractère spécial dans ce champ ;

-   le champ `year` est **obligatoire**, et doit contenir l’année
    d’observation de l’espèce ;

-   le champ `transect` (ou `point`) est **facultatif**, et constitue
    l’échelle la plus fine d’observation. Elle doit être accompagnée
    d’un champ `site` **obligatoire**, qui est associé à un ou plusieurs
    `transect` (ou `point`) ;

-   le champ `ID` est **obligatoire**, et correspond à un identifiant
    unique **date + site (+ point/transect)** ;

-   les champs `longitude` et `latitude` sont **facultatifs**, mais leur
    présence dans le jeu de données permet de créer une carte de la
    répartition des sites en France ;

-   un champ contenant l’information d’abondance / comptage / activité,
    dont le nom de colonne n’est pas imposé.

| ID          | species              | year | site | point |
|-------------|:---------------------|-----:|-----:|------:|
| 2020_A1_a11 | Pigeon ramier        | 2020 |   A1 |   a11 |
| 2019_A1_a24 | Mésange charbonnière | 2019 |   A2 |   a24 |
| 2020_A1_a18 | Pinson des arbres    | 2020 |   A1 |   a18 |

# II. Fonctions individuelles

Une fois que l’utilisateur a pris connaissance de l’installation de
l’environnement de cet outil, et du format des données nécessaire à son
bon fonctionnement, l’utilisateur a deux possibilités :

-   ou bien utiliser la routine d’analyses accessible dans le `main.R`,
    permettant l’analyse des tendances de différentes espèces en
    routine. Il peut alors prendre connaissance de cette section pour
    bien comprendre le fonctionnement de cette routine de calculs ;

-   ou bien utiliser les fonctionnalités de l’outil indépendamment de la
    routine, auquel cas l’utilisateur est encouragé à lire l’encart
    **Fonctions individuelles**.

## 1. Charger les données

La fonction `importData` permet d’importer les données présentes dans le
répertoire **data** même si les données sont séparées en plusieurs
sous-jeux de données.

``` r
data <- importData(path = dataDir)
```

Les données sont donc maintenant disponibles dans R, sous le nom de :
`data`.

``` r
head(data)
```

    ##     site year         ID species abondance longitude latitude
    ## 1 010100 2007 2007010100  APUAPU         0  5.107878 46.39010
    ## 2 010120 2008 2008010120  APUAPU         2  4.925090 46.37641
    ## 3 010120 2009 2009010120  APUAPU         0  4.925090 46.37641
    ## 4 010120 2010 2010010120  APUAPU         0  4.925090 46.37641
    ## 5 010168 2021 2021010168  APUAPU         0  6.040569 46.32747
    ## 6 010265 2021 2021010265  APUAPU         0  5.622082 46.28633

## 2. Gérer les absences

Les jeux de données d’abondance peuvent parfois être transmis sans
données d’absence, ou bien à cause du volume qu’elles représenteraient,
ou bien parce que le protocole ne prévoit pas l’enregistrement des
absences. La fonction `fillAbsence` a été imaginée pour reconstruire ces
absences, selon deux méthodes :

-   `method = "once"` reconstruit seulement les absences pour les sites
    où l’espèce a été vue au moins une fois ;

-   `method = "all"` reconstruit les absences pour tous les sites x
    années de visite, indépendamment de la présence de l’espèce. **\<!\>
    pas encore implémenté**

``` r
dataAll <- fillAbsence(data = data, interestVar = "count", method = "once")
```

    ## Absences have been correctly filled with 0s

## 3. Choisir les variables à inclure

Cette étape ne repose pas sur une fonction existant dans le modèle, mais
elle doit être bien réfléchie par l’utilisateur pour que les modèles
utilisés soient pertinents. Elle consiste à choisir les variables à
inclure dans le modèle. Cela passe par l’identification :

-   de la variable que l’on cherche à modéliser i.e, la variable à
    expliquer `interestVar`. Ici nous voulons modéliser l’abondance
    accessible dans le champ *count* ;

``` r
summary(dataAll$abondance)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   0.000   0.000   0.000   7.425   6.000 342.000

-   le(s) variable(s) que l’on souhaite considérer comme des effets
    fixes `fixedEffects` ? Ici, nous nous intéressons particulièrement à
    l’effet de l’année *year* sur l’abondance mais nous souhaitons aussi
    contrôler pour l’effet de la localisation au travers des champs
    *longitude* et *latitude* ;

``` r
# Year
summary(dataAll$year)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    2001    2007    2011    2011    2016    2021

``` r
# Longitude
summary(dataAll$longitude)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ## -5.0665  0.9357  3.2656  2.9221  4.9879  8.1687      28

``` r
# Latitude
summary(dataAll$latitude)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##   42.45   45.51   46.75   46.74   48.22   51.04      28

-   le(s) variable(s) que l’on souhaite considérer comme des effets
    aléatoires `randomEffects` ? Ici, le *site* peut avoir une influence
    forte sur l’abondance perçue. Il s’agit d’une variable catégorielle
    à beaucoup de niveaux. La considérer comme un effet aléatoire nous
    permet d’une part de garder de la puissance dans le calcul de
    tendances, et d’autre part de généraliser les résultats à l’ensemble
    des sites de France.

``` r
table(dataAll$site)[1:10]
```

    ## 
    ## 010100 010120 010168 010265 010295 010475 010487 010492 010506 010511 
    ##      1      3      1      1     10      1      1      5      2     13

On pourrait également ajouter :

-   des *effets emboîtés*, liés à l’échantillonnage. Par exemple, les
    points ne sont pas les mêmes d’un site à l’autre, ce qui pourrait
    donner un effet emboîté **point\|site**, que nous ne testerons pas
    ici ;

-   des *effets polynomiaux*, par exemple intégrer à la fois la
    **longitude** et la **longitude^2** ;

-   des *interactions* entre variables, quand on pense que l’effet d’une
    variable peut varier en fonction de la valeur d’une autre variable
    *(\<!\> non implémenté)* ;

## 4. Choisir la distribution

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
    de présence/absence. *\<!\> non encore implémenté* ;

-   la **distribution de Poisson**, particulièrement adaptée aux données
    de comptage, mais qui impose néanmoins une contrainte très forte
    d’égalité entre moyenne et variance, ce qui peut entraîner des
    problèmes de sur-dispersion ;

-   la **distribution négative binomiale**, adaptée aux données de
    comptage et qui repose sur un paramètre de dispersion supplémentaire
    permettant de surmonter certains problèmes rencontrés avec la
    distribution de Poisson.

La fonction `detectDistrib` permet de proposer une distribution
pertinente à l’utilisateur, si celui-ci ne sait pas laquelle choisir. En
l’occurrence, il impose :

-   dans le cas de données continues, une loi **normale** ;

-   dans le cas de données de comptage (valeurs positives et discrètes),
    une loi **négative binomiale** adaptée aux données de comptage mais
    plus flexible que la *loi de Poisson* (hypothèse forte d’égalité des
    moyenne et variance). Elle impose également une partie zéro-enflée
    au modèle, avec un simple intercept, pour gérer la grande quantité
    de 0 souvent associée à ces données ;

-   dans le cas de probabilités ou de données binaires, une loi
    **binomiale** *(\<!\> à implémenter)*.

``` r
distribParam = detectDistrib(data = dataAll, interestVar = "abondance", 
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

## 4. Modéliser la tendance globale

La fonction `makeGLM` permet de réaliser des modèles linéaires
généralisés mixtes. Cette fonction repose sur le package `glmmTMB`, qui
a plusieurs avantages notamment l’implémentation de nombreuses
distributions (dont la négative binomiale) et des temps de calcul plutôt
faibles.

La fonction `makeGLM` contient de nombreux paramètres, notamment
concernant la modélisation qui peut être faite :

-   **interestVar** permet de spécifier la colonne de la variable à
    expliquer ;

-   **fixedEffects** permet de spécifier l’ensemble des effets fixes, y
    compris les variables à traiter comme variables catégorielles ;

-   **randomEffects** permet de renseigner les variables à traiter comme
    des effets aléatoires ;

-   **factorVariables** permet de renseigner les variables à traiter
    comme des catégorielles parmi les effets fixes ;

-   **distribution** permet de renseigner la distribution des résidus
    parmi : *gaussian*, *poisson* et *nbinom2* ;

-   **intercept** permet de spécifier si oui ou non, un intercept doit
    être considéré dans le modèle ;

-   **zi** permet de spécifier si oui ou non, un modèle zéro-enflé avec
    intercept doit être évalué pour le modèle.

``` r
mod <- makeGLM(data = dataAll, 
               interestVar = "abondance", 
               fixedEffects = c("year","longitude","latitude"),
               randomEffects = "site", 
               nestedEffects = list(),
               factorVariables = NULL,
               poly = NULL,
               distribution = "nbinom2",
               zi = TRUE,
               intercept = TRUE)  
```

La fonction retourne une liste de 3 éléments :

-   `mod$value`, donne les résultats de la régression si aucune erreur
    n’a été rencontrée pendant le processus d’estimation des paramètres
    ;

``` r
mod$value
```

    ## Formula:          abondance ~ 1 + year + longitude + latitude + (1 | site)
    ## Zero inflation:             ~1
    ## Data: data
    ##       AIC       BIC    logLik  df.resid 
    ##  73949.62  74003.86 -36967.81     17123 
    ## Random-effects (co)variances:
    ## 
    ## Conditional model:
    ##  Groups Name        Std.Dev.
    ##  site   (Intercept) 1.971   
    ## 
    ## Number of obs: 17130 / Conditional model: site, 3042
    ## 
    ## Dispersion parameter for nbinom2 family (): 0.902 
    ## 
    ## Fixed Effects:
    ## 
    ## Conditional model:
    ## (Intercept)         year    longitude     latitude  
    ##   102.70442     -0.04628     -0.04375     -0.18943  
    ## 
    ## Zero-inflation model:
    ## (Intercept)  
    ##      -1.728

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

*A noter :* le processus d’estimation des paramètres peut être perturbé
par la présence de paramètres numériques avec des plages de valeurs
importantes et variables d’un paramètre à l’autre (ex : température
entre 0 et 40°C vs. année entre 2000 et 2020). Pour cela, une astuce
simple peut être de centrer/réduire les variables numériques. Cela est
possible dans la fonction `makeGLM` au travers du paramètre
`scaling = TRUE`.

``` r
modScaled <- makeGLM(data = dataAll, 
                     interestVar = "abondance", 
                     fixedEffects = c("year","longitude","latitude"),
                     randomEffects = "site", 
                     nestedEffects = list(),
                     factorVariables = NULL,
                     poly = NULL,
                     distribution = "nbinom2",
                     zi = TRUE,
                     intercept = TRUE,
                     scaling = TRUE)
```

## 5. Modéliser les variations inter-annuelles

La modélisation des variations inter-annuelles peut être réalisée en
considérant la variable temporelle `year` comme une **variable
catégorielle**. Pour cela, il suffit d’ajouter la variable *year* au
paramètre `factorVariables` de la fonction `makeGLM`. Il est à noter que
les coefficients associés à chaque année ne correspondent pas à une
abondance absolue de chaque année, mais bien à une abondance relative
par rapport à une année de référence. Par défaut, l’année de référence
est fixée à la première année, mais la référence peut être changée au
travers de l’argument `contr` de la fonction. Ce paramètre accepte
plusieurs valeurs :

-   *2001, …, 2021* : une année spécifique choise comme la référence ;

-   *mean* : la moyenne des années comme la référence. L’avantage de
    choisir cette référence est de s’extraire des problèmes
    d’échantillonnage potentiellement rencontrés d’une année sur
    l’autre.

``` r
modCat <- makeGLM(data = dataAll, 
                  interestVar = "abondance", 
                  fixedEffects = c("year", "longitude", "latitude"),
                  randomEffects = "site",
                  nestedEffects = list(),
                  factorVariables = "year", 
                  distribution = "nbinom2",
                  contr = "mean",
                  zi = TRUE,
                  scaling = TRUE,
                  intercept = TRUE)
```

    ## Contrast associated with the variable 'year' has been set to the mean of all levels

## 6. Formatter les sorties des modèles

Une fois la régression réalisée, nous souhaitons extraire les résultats
(estimations, erreurs, p-value, …) dans un format plus lisible pour
l’utilisateur. La fonction `summaryOutput` permet de faire ce
reformatage et contient un certain nombre de paramètres :

-   le paramètre `rescale` permet de dé-centrer/réduire les coefficients
    et erreurs standards. Ce paramètre n’a de sens que si les variables
    explicatives numériques ont été centrées-réduites dans un premier
    temps lors de la régression ;

-   le paramètre `transform` permet de transformer les coefficients
    lorsque la fonction de lien est différente de l’identité i.e, quand
    il s’agit d’un log ou logit, notamment dans le cas de distribution
    non gaussiennes (binomiale, poisson, négative binomiale, …)

Si on applique la fonction aux tendances globales, on peut retrouver les
estimations brutes dans le tableau suivant. Elles sont difficiles à
interpréter car les variables ont été initialement centrées-réduites :

``` r
summaryOutput(model = modScaled, data = dataAll, 
              distribution = "nbinom2",
              fixedEffects = c("year", "longitude", "latitude"),
              factorVariables = NULL, poly = NULL,
              transform = FALSE, 
              rescale = FALSE)
```

    ##               Estimates Standard Errors      IC_inf      IC_sup   P Values
    ## (Intercept)  6.3212e-01      4.7505e-02  5.3901e-01  7.2523e-01 2.1311e-40
    ## year        -2.5393e-01      1.7691e-02 -2.8861e-01 -2.1926e-01 1.0039e-46
    ## longitude   -1.1659e-01      4.2812e-02 -2.0050e-01 -3.2677e-02 6.4643e-03
    ## latitude    -3.4066e-01      4.0008e-02 -4.1908e-01 -2.6225e-01 1.6673e-17
    ##             Significatif
    ## (Intercept)          Oui
    ## year                 Oui
    ## longitude            Oui
    ## latitude             Oui

Les estimations dé-centrées/réduites sont regroupées dans le tableau
suivant. Elles sont à interpréter comme suit : *quand la variable year
augmente de 1, le **log** de l’abondance augmente de **-0.04628***.

``` r
summaryOutput(model = modScaled, data = dataAll,
              distribution = "nbinom2",
              fixedEffects = c("year", "longitude", "latitude"),
              factorVariables = NULL, poly = NULL,
              transform = FALSE, 
              rescale = TRUE)
```

    ##               Estimates Standard Errors      IC_inf      IC_sup   P Values
    ## (Intercept)  2.2571e+01     -1.7304e+00  2.5962e+01  1.9179e+01 2.1311e-40
    ## year        -4.6284e-02      3.2244e-03 -5.2604e-02 -3.9964e-02 1.0039e-46
    ## longitude   -4.3742e-02      1.6062e-02 -7.5224e-02 -1.2260e-02 6.4643e-03
    ## latitude    -1.8942e-01      2.2246e-02 -2.3303e-01 -1.4582e-01 1.6673e-17
    ##             Significatif
    ## (Intercept)          Oui
    ## year                 Oui
    ## longitude            Oui
    ## latitude             Oui

Les estimations dé-centrées/réduites et retransformées sont regroupées
dans le tableau suivant. Elles sont à interpréter comme suit : *quand la
variable year augmente de 1, l’abondance est multipliée par **0.9547***.

``` r
sum <- summaryOutput(model = modScaled, data = dataAll,
                     distribution = "nbinom2",
                     fixedEffects = c("year","longitude","latitude"),
                     factorVariables = NULL, poly = NULL,
                     transform = TRUE, 
                     rescale = TRUE)
sum
```

    ##              Estimates Standard Errors     IC_inf     IC_sup   P Values
    ## (Intercept) 6.3449e+09      1.7722e-01 1.8852e+11 2.1355e+08 2.1311e-40
    ## year        9.5477e-01      1.0032e+00 9.4876e-01 9.6082e-01 1.0039e-46
    ## longitude   9.5720e-01      1.0162e+00 9.2754e-01 9.8782e-01 6.4643e-03
    ## latitude    8.2744e-01      1.0225e+00 7.9213e-01 8.6431e-01 1.6673e-17
    ##             Significatif
    ## (Intercept)          Oui
    ## year                 Oui
    ## longitude            Oui
    ## latitude             Oui

Par ailleurs, on peut appliquer la même fonction aux variations
inter-annuelles :

``` r
sumCat <- summaryOutput(model = modCat,
                        distribution = "nbinom2",
                        factorVariables = "year", poly = NULL,
                        transform = TRUE, 
                        rescale = TRUE,
                        data = dataAll,
                        fixedEffects = c("year","longitude","latitude"),
                        contr = "mean")

sumCat
```

    ##              Estimates Standard Errors     IC_inf     IC_sup   P Values
    ## (Intercept) 2.6418e+02      5.7751e-01 7.7489e+02 9.0066e+01 1.3167e-35
    ## year : 2002 1.2770e+00      1.0747e+00 1.1087e+00 1.4707e+00 6.9338e-04
    ## year : 2003 1.2164e+00      1.0622e+00 1.0808e+00 1.3690e+00 1.1635e-03
    ## year : 2004 1.4925e+00      1.0577e+00 1.3372e+00 1.6658e+00 9.0318e-13
    ## year : 2005 1.5212e+00      1.0546e+00 1.3707e+00 1.6882e+00 2.9869e-15
    ## year : 2006 1.4017e+00      1.0554e+00 1.2611e+00 1.5579e+00 3.8164e-10
    ## year : 2007 1.1966e+00      1.0551e+00 1.0773e+00 1.3292e+00 8.1110e-04
    ## year : 2008 1.1854e+00      1.0518e+00 1.0736e+00 1.3087e+00 7.6140e-04
    ## year : 2009 1.4608e+00      1.0521e+00 1.3225e+00 1.6136e+00 8.1401e-14
    ## year : 2010 1.1666e+00      1.0528e+00 1.0547e+00 1.2904e+00 2.7509e-03
    ## year : 2011 1.0126e+00      1.0555e+00 9.1090e-01 1.1257e+00 8.1650e-01
    ## year : 2012 1.2045e+00      1.0581e+00 1.0784e+00 1.3454e+00 9.7585e-04
    ## year : 2013 8.6102e-01      1.0622e+00 7.6496e-01 9.6913e-01 1.3156e-02
    ## year : 2014 8.5388e-01      1.0593e+00 7.6269e-01 9.5598e-01 6.1209e-03
    ## year : 2015 6.7209e-01      1.0658e+00 5.9319e-01 7.6148e-01 4.4660e-10
    ## year : 2016 7.8593e-01      1.0671e+00 6.9197e-01 8.9263e-01 2.0846e-04
    ## year : 2017 8.0010e-01      1.0633e+00 7.0944e-01 9.0235e-01 2.7829e-04
    ## year : 2018 7.7305e-01      1.0630e+00 6.8584e-01 8.7134e-01 2.4958e-05
    ## year : 2019 6.6425e-01      1.0642e+00 5.8793e-01 7.5047e-01 5.0307e-11
    ## year : 2020 4.9313e-01      1.1026e+00 4.0721e-01 5.9718e-01 4.5546e-13
    ## year : 2021 7.0638e-01      1.0645e+00 6.2498e-01 7.9838e-01 2.6272e-08
    ## longitude   9.5733e-01      1.0162e+00 9.2763e-01 9.8799e-01 6.7063e-03
    ## latitude    8.2729e-01      1.0226e+00 7.9191e-01 8.6426e-01 1.8681e-17
    ##             Significatif
    ## (Intercept)          Oui
    ## year : 2002          Oui
    ## year : 2003          Oui
    ## year : 2004          Oui
    ## year : 2005          Oui
    ## year : 2006          Oui
    ## year : 2007          Oui
    ## year : 2008          Oui
    ## year : 2009          Oui
    ## year : 2010          Oui
    ## year : 2011          Non
    ## year : 2012          Oui
    ## year : 2013          Oui
    ## year : 2014          Oui
    ## year : 2015          Oui
    ## year : 2016          Oui
    ## year : 2017          Oui
    ## year : 2018          Oui
    ## year : 2019          Oui
    ## year : 2020          Oui
    ## year : 2021          Oui
    ## longitude            Oui
    ## latitude             Oui

## 7. Extraire les coefficients

Nous cherchons à savoir plus précisément comment a évolué l’abondance de
l’espèce entre la première et la dernière année d’observation. La
fonction `analyseCoef` permet de calculer deux indicateurs intéressants
: la tendance `trend` et le pourcentage de variation `perc`, dont
l’interprétation est explicitée par la suite. Elle permet aussi
d’extraire les bornes inférieure `percInf` et supérieure `percSup` de
l’intervalle de confiance autour du pourcentage de variation.

``` r
coefficients <- analyseCoef(model = modScaled,
                            rescale = TRUE, 
                            data = dataAll,
                            distribution = "nbinom2",
                            effectVar = "year")
```

Entre la première et la dernière année, l’abondance a été multipliée par
:

``` r
coefficients$trend
```

    ## [1] 0.4

Entre la première et la dernière année, l’abondance a évolué de :

``` r
paste(coefficients$perc, "%")
```

    ## [1] "-60.37 %"

L’intervalle de confiance autour de cette estimation est le suivant :

``` r
paste(coefficients$percInf,"/", coefficients$percSup, "%")
```

    ## [1] "-65.08 / -55.03 %"

## 8. Contrôle de la qualité du modèle : VIF

La multi-colinéarité est un phénomène qui traduit le fait qu’une des
variables explicatives est **linéairement liée aux autres variables**.
Cela rend les paramètres estimés pour ces variables instables, et peut
même cacher un effet significatif de ces variables. La fonction
`measureVIF` permet de calculer le **VIF (Variance Inflation Factor)**,
qui est un indicateur souvent utilisé dans le cadre de la
multi-colinéarité.

``` r
vif <- measureVIF(model = modScaled)

vif
```

    ##     year longitude latitude
    ## VIF    1      1.02     1.01

Il n’existe pas de consensus autour d’une valeur seuil : 2 est très
conservatif, 10 très peu conservatif, 5 est une valeur intermédiaire que
nous recommadons ici, mais l’utilisateur est libre de gérer la
multi-colinéarité comme il le souhaite.

*NB* : dans certains cas, une forte colinéarité n’est pas forcément un
problème, c’est le cas quand il s’agit :

-   de variables contrôles dont l’effet ne nous intéresse pas, et
    qu’elles ne sont pas colinéaires à une variable d’intérêt, elles
    peuvent être laissées dans le modèle ;

-   de variables catégorielles avec beaucoup de niveaux, dont un niveau
    avec peu d’observations ;

-   de variables “polynomiales” (ex : longitude et longitude^2).

Si par contre une variable est fortement colinéaire avec une variable
explicative d’intérêt, il est **judicieux de la retirer du modèle**.

## 9. Représenter les tendances

Maintenant que nous avons récupéré la tendance globale, les coefficients
associés à chaque année, et que nous avons vérifié la qualité du modèle,
nous aimerions les représenter sur un graphique pour visualiser les
variations d’abondance au cours du temps. Pour cela, la fonction
`plotGLM` permet de représenter sur le même graphique :

-   les coefficients associés à chaque année (point orange) avec leurs
    intervalles de confiance ;

-   la tendance globale et sa significativité (courbe rouge).

``` r
plotGLM(summary = sum, modelCat = modCat, summaryCat = sumCat, effectVar = "year", distribution = "nbinom2",
        type = "relative", sp = unique(dataAll$species), coefs = coefficients, contr = "mean", path = NULL)
```

![](README_files/figure-gfm/plotGLM-1.png)<!-- -->

Si le paramètre `path` est renseigné, alors la figure sera sauvegardée
dans le répertoire associé.

## 10. Fonction annexe : gérer les erreurs

Lors d’analyses en routine sur un grand nombre d’espèces, des erreurs
peuvent se produire en cours de route et arrêter les analyses. Pour
maintenir les analyses, tout en gardant une trace des erreurs / alertes,
la fonction `catchConditions` permet d’encapsuler des fonctions
potentiellement instables et d’extraire les alertes ou erreurs
rencontrées. C’est le cas dans `makeGLM`, dans laquelle la fonction
`glmmTMB` y est encapsulée.

Construisons d’abord une fonction simple, qui peut présenter des alertes
ou des erreurs :

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

# III. Routine d’analyses

## 1. Paramètres utilisateurs

Un certain nombre de paramètres doivent être choisis par l’utilisateur
pour le bon fonctionnement de la routine. Ils sont à renseigner au début
du script `main.R` et concernent notamment :

-   le chemin `rootDir` vers le dossier contenant le script `main.R` ;

-   les différentes variables explicatives et à expliquer à inclure dans
    l’analyse ;

-   un niveau de référence `contr` pour les variations annuelles.

## Paramètres automatiques

Dans la routine, certains paramètres sont automatiquement choisis pour
éviter de sur-solliciter l’utilisateur. C’est le cas notamment :

-   de la liste d’espèces d’intérêt `speciesList`, qui est fixée
    automatiquement à l’ensemble des valeurs prises dans la colonne
    `species` ;

-   de la profondeur de données utilisée `yearRange`, qui est fixée
    automatiquement à l’ensemble des valeurs prises dans la colonne
    `year` ;

-   de l’obligation de centrer/réduire les variables numériques
    `scaling`, automatiquement fixé à `TRUE` ;

-   de la distribution choisie dans le modèle, qui est fixée
    automatiquement en fonction des valeurs prises dans la colonne
    d’**abondance** via la fonction `detectDistrib` décrite en **II.
    4.**.

A noter : l’ensemble de ces paramètres est en fait modifiables dans le
fichier *parameters.R*, accessible dans le sous-dossier
**scripts/basic/**.

## Que fait la routine ?

A l’issue de la routine, un certain nombre de documents sont rendus
disponibles. D’abord, pour chaque espèce :

-   un graphique dans le dossier **resultats/YEAR**, représentant le
    nombre de sites visités par an, avec l’information de présence ou
    d’absence sur chaque site ;

![](C:/Users/mvimont01/Documents/MNHN/Tendances/Code/tendency/results/YEAR/APUAPU.png)<!-- -->

-   si les coordonnées sont disponibles, une carte de France dans le
    dossier **resultats/MAP**, où sont représentés les sites où l’espèce
    a été vue ;

![](C:/Users/mvimont01/Documents/MNHN/Tendances/Code/tendency/results/MAP/APUAPU.png)<!-- -->

-   si les modèles ont convergé, un graphique dans le dossier
    **resultats/TREND**, représentant les variations annuelles
    d’abondance et la tendance globale ;

![](C:/Users/mvimont01/Documents/MNHN/Tendances/Code/tendency/results/TREND/APUAPU.png)<!-- -->

En parallèle, les résultats des modèles sont sauvegardés dans les
fichiers :

-   `globalTrends.csv`, qui contient les résultats de tendance globale
    pour l’ensemble des espèces :

| species           | nbObs | nbYear | totAbundance | vif | intercept | interceptSE | interceptInf | interceptSup | interceptPval | estimate | estimateSE | estimateInf | estimateSup | estimatePval | trend | trendInf | trendSup | significance | category             |
|-------------------|-------|--------|--------------|-----|-----------|-------------|--------------|--------------|---------------|----------|------------|-------------|-------------|--------------|-------|----------|----------|--------------|----------------------|
| Pigeon ramier     | 17062 | 19     | 178921       | NA  | 1.81e-05  | 0.73734     | 3.2981e-05   | 9.9887e-06   | 0             | 1.0357   | 1.0009     | 1.0339      | 1.0375      | 0            | 94.75 | 88.55    | 101.15   | Oui          | Augmentation modérée |
| Pinson des arbres | 17062 | 19     | 233544       | NA  | 9.4173    | 0.8187      | 13.938       | 6.3628       | 0             | 1.0004   | 1.0006     | 0.99926     | 1.0015      | 0.49979      | 0.75  | -1.4     | 2.94     | Non          | Stable               |

-   `yearlyVariations.csv`, qui contient les résultats des variations
    annuelles pour l’ensemble des espèces :

| species       | nbObs | nbYear | totAbundance | contrast | year      | estimate | estimateSE | estimateInf | estimateSup | pval       | significance |
|---------------|-------|--------|--------------|----------|-----------|----------|------------|-------------|-------------|------------|--------------|
| Pigeon ramier | 17062 | 19     | 178921       | mean     | intercept | 7.6867   | 1.0162     | 7.449       | 7.9319      | 0          | Oui          |
| Pigeon ramier | 17062 | 19     | 178921       | mean     | 2003      | 0.7136   | 1.0189     | 0.68791     | 0.74025     | 9.4767e-73 | Oui          |
| Pigeon ramier | 17062 | 19     | 178921       | mean     | 2004      | 0.77285  | 1.0167     | 0.74811     | 0.79841     | 2.4289e-54 | Oui          |

## IV. Fonctionnalités à venir

-   choix d’une distribution binomiale ;

-   possibilité d’interactions entre variables ;

-   correction pour l’auto-corrélation spatiale.
