library(dplyr)

load_data <- function(data_location = "data/processed_data/"){
  # Loads all csv files from a specific loaction and combines them into a
  # single dataframe
  files <- list.files(data_location, full.names = T)
  allData <- readr::read_csv(files, na = "NaN", show_col_types = F)
  return(allData)
}

