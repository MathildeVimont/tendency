#' fillAbsence
#'
#' A function that fills the data.frame with absences when they are not informed
#' @param data
#' A `data.frame` containing observations of species. It should contain :
#' - species : char, name or code of the species
#' - count : int, number or proportion of individuals seen
#' - ID : char, a unique identifier containing information on : date - site - point
#'
#' @param interestVar a `string` specifying the column with abundance information 
#' @param method
#' A `string` defining the method chosen to reconstruct absences
#' "once" means reconstructing absences on site where the species was seen once at least
#' "all" means reconstructing absences for all sites
#'
#' @return
#' A dataframe filled with counting data set to 0 for time/location where the species is absent
#'
#' @export
#' @example
fillAbsence <- function(data, interestVar = "count", method = "once"){

  # Save those unique location/times
  id <- unique(data$ID)

  # Initialise a list of dataframe
  listData <- list(all = data)

  for (s in unique(data$species)){
    # print(s)

    # Filter data for the considered species
    dataSp <- data[data$species == s, ]

    # Save location/times where the species has been present once
    if (method == "once"){
      if( "point" %in% colnames(dataSp)){
        siteSp <- unique(dataSp$point)
        id <- unique(data[data$point %in% siteSp,]$ID)
      }
      
      else if( "transect" %in% colnames(dataSp)){
        siteSp <- unique(dataSp$transect)
        id <- unique(data[data$transect %in% siteSp,]$ID)
      }
      else{
        siteSp <- unique(dataSp$site)
        id <- unique(data[data$site %in% siteSp,]$ID)
      }
      
      
    }

    # Extract location/times that are indeed informed
    idSp <- unique(dataSp$ID)

    # Extract missing location/times for this species
    missing <- id[which(!(id %in% idSp))]

    if (length(missing) > 0){
      # Dataframe containing information regarding those missing location/times
      dataMissing <- data[data$ID %in% missing, !(colnames(data) %in% c('species','count'))]

      # Make sure this dataframe has no redondant lines
      dataMissing <- unique(dataMissing)

      # Comple the dataframe with species name and abundance set to 0
      dataMissing[, "species"] <- rep(s, nrow(dataMissing))
      dataMissing[, interestVar] <- 0

      # Add those lines to the general dataframe
      listData[[s]] <- dataMissing
    }

  }

  # Bind all dataframes together with the
  data = bind_rows(listData)

  cat("Absences have been correctly filled with 0s")

  return(data)
}
