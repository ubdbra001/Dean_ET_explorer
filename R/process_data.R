library(dplyr)

load_data <- function(data_location = "data/processed_data/"){
  # Loads all csv files from a specific loaction and combines them into a
  # single dataframe
  files <- list.files(data_location, full.names = T)
  allData <- readr::read_csv(files, na = "NaN", show_col_types = F)
  return(allData)
}

add_sample_n <- function(data){
  # Adds sample_ID column that numbers each sample on a per participant and per
  # trial basis
  
  grouped_data <- group_by(data, trial_ID, part_ID)
  data_WsampleN <- mutate(grouped_data, sample_ID = row_number())
  data_out <- ungroup(data_WsampleN)
  
  return(data_out)
}