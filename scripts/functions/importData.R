#' importData
#' 
#' A function that imports and concatenates multiple files with all observations
#' @param path `string`, the path towards the repertory that contains all data files
#' 
#' @return data, a `dataframe` that corresponds to the agregated dataframe
#' 
#' 
importData <- function(path){
  
  # Extract all files from data repertory
  listFiles = list.files(path = path)
  
  # Initialise an empty dataframe
  data = data.frame()
  
  # Agregate files one by one
  for (l in listFiles){
    dataSpecies = fread(paste0(path, l))
    
    data = rbind(data, dataSpecies)
  }
  
  # Remove automatic indices column 
  if ("V1" %in% colnames(data)){
    data$V1 <- NULL
  }
  
  # Turn to dataframe format
  data <- data.frame(data)
  
  # Return the agregated dataframe
  return(data)
}
