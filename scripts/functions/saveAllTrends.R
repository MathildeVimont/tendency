#' saveAllTrends
#'
#' A function that agregates all model results from different species into a single dataframe
#' No dataframe is returned, but it is saved as a .csv document
#' 
#' @param list a `list` all information that should be saved, per species
#' @param path a `string` containing the path where the resulting dataframe should be saved
#' 
#' @return nothing
#'
#' @export
#'
#' @example 
saveAllTrends <- function(list, path = NULL){
  
  ########################
  # INITIALIZE DATAFRAME #
  ########################
  data <- matrix(nrow = length(list), ncol = 8)
  data <- as.data.frame(data)
  colnames(data) <- c("species", "nbObs", "nbYear", "totCount", "trend", 
                      "trendInf", "trendSup", "significant")
  
  ##################
  # FILL DATAFRAME #
  ##################
  
  # Extract species names
  data$species <- names(list)
  
  # Extract global information on the data related to this species
  data$nbObs <- sapply(list, function(x) {ifelse(is.null(x$nbObs), NA, x$nbObs)})
  data$nbYear <- sapply(list, function(x) {ifelse(is.null(x$varRange), NA, x$varRange)})
  data$totCount <- sapply(list, function(x) {ifelse(is.null(x$varRange), NA, x$totCount)})
  
  # Extract trend and upper/lower value of its interval confidence
  data$trend <- unlist(sapply(list, function(x){ ifelse(is.null(x$coefs$perc), NA, x$coefs$perc)}))
  data$trendInf <- unlist(sapply(list, function(x){ ifelse(is.null(x$coefs$percInf), NA, x$coefs$percInf) }))
  data$trendSup <- unlist(sapply(list, function(x){ ifelse(is.null(x$coefs$percSup), NA, x$coefs$percSup) }))
  
  # Extract significance of the trend
  data$significant <- unlist(sapply(list, function(x){ ifelse(is.null(x$annualSum["year",]$Significant), NA, x$annualSum["year",]$Significant) }))
  
  ##################
  # SAVE DATAFRAME #
  ##################
  write.csv(x = data, file = paste0(path, "allTrends.csv"))
}
