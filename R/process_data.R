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

categorise_look <- function(data, AOI_info = NULL){
  
  
  # If no AOI info is provided then load defaults
  if(is.null(AOI_info)) {
    AOI_info <- default_AOIs()
  }
  
  AOI_names <- names(AOI_info)
  
  # For each AOI get the x & y values and gaze falls within them  
  for (AOI_name in AOI_names){
    
    # May be able to functionalise this further? 
    AOI_X <- AOI_info[[AOI_name]]$X
    AOI_Y <- AOI_info[[AOI_name]]$Y
    
    in_x <- between(gaze_x, AOI_X[1], AOI_X[2])
    in_y <- between(gaze_y, AOI_Y[1], AOI_Y[2])
    
    data <- mutate(data, "AOI_{AOI_name}" := in_x & in_y)
  }
  
  return(data)
}

default_AOIs <- function(){
  # Generate nested list with default AOIs
  AOIs <- list(L = list(X = c(0, 0.4),
                        Y = c(0, 1)),
               R = list(X = c(0.6, 1),
                        Y = c(0, 1)))
}

remove_oversampled <- function(data_in, sample_rate = 800) {
  
  # Find and remove participants with data that exceed a specified sample rate
  
  grouped_data <- group_by(data_in, part_ID)
  
  samples_per_participant <- summarise(grouped_data, N_samples = max(sample_ID),
                              .groups = "keep")
  
  oversampled <- filter(samples_per_participant, N_samples > sample_rate)
  
  data_out <- filter(data_in, !part_ID %in% oversampled$part_ID)
  return(data_out)
}

find_first_look <- function(data, run_length) {
  
  ## Finds the first time a run of Ts of a specified length appear in the
  # data and returns the starting location.
  
  # Get lengths of all runs
  runs <- rle(data)
  
  # Find location of all runs that meet the criteria
  first_look_run <- which(runs$values == TRUE & runs$lengths >= run_length)
  
  # Compute where the runs stop
  run_end_pos <- cumsum(runs$lengths)
  # Compute where the runs start
  run_start_pos <- c(1, run_end_pos[-length(run_end_pos)]+1)
  
  if (length(first_look_run) > 0) {
    # Compute location of run start if run of length found
    first_look_loc <- run_start_pos[first_look_run[1]]
  } else {
    # Otherwise return NA
    first_look_loc <- NA_integer_
  }
  
  return(first_look_loc)
}
