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

average_gaze_pos <- function(data){
  
  # Averages gaze position across both eyes
  # Will ignore NAs so if one eye is not found then it will return the position
  # for only a single eye
  
  # May want to alter whether single eye is enough to be user specified?
  data_out <- mutate(data,
                     gaze_x = rowMeans(across(LX:RX), na.rm = T),
                     gaze_y = rowMeans(across(LY:RY), na.rm = T))
  
  return(data_out)
}
