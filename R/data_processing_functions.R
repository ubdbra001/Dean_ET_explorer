library(dplyr)
source("R/user_defined_constants.R")

load_data <- function(data_location = "data/processed_data/"){
  # Loads all csv files from a specific loaction and combines them into a
  # single dataframe
  files <- list.files(data_location, full.names = T)
  allData <- readr::read_csv(files, na = "NaN", show_col_types = F)
  return(allData)
}

add_sample_n <- function(data){
  # Adds sample_ID column that numbers each sample on a per participant and per
  # trial  
  
  grouped_data <- group_by(data, trial_ID, part_ID)
  data_WsampleN <- mutate(grouped_data,
                          sample_ID = row_number(),
                          sample_time = round(
                            (sample_ID-1)/sample_rate,
                            digits = 5))
  data_out <- ungroup(data_WsampleN)
  
  return(data_out)
}

add_group <- function(data, groupFile_loc = "data/Groups.csv") {
  
  # Load and add group info to data
  
  group_info <- readr::read_csv(groupFile_loc, show_col_types = F)
  group_recoded <- mutate(group_info,
                          Group = recode(Group, M = "Monolingual", B = "Bilingual"))
  data <- left_join(data, group_recoded, by = "part_ID")

}

add_screen_looking <- function(data_in) {
  # Add variable that indicates if participant gaze falls on screen
  data_out <- mutate(data_in, screen_looking = !(is.na(gaze_x) | is.na(gaze_y)))
  
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
  
  if (is.null(AOI_info)) {
    AOI_info <- AOI_inputs_to_list()
  }
    
  AOI_names <- names(AOI_info)
  
  # For each AOI get the x & y values and gaze falls within them  
  for (AOI_name in AOI_names){
    
    # May be able to functionalise this further? 
    AOI_X <- AOI_info[[AOI_name]]$X
    AOI_Y <- AOI_info[[AOI_name]]$Y
    
    in_x <- between(data$gaze_x, AOI_X[1], AOI_X[2])
    in_y <- between(data$gaze_y, AOI_Y[1], AOI_Y[2])
    
    data <- mutate(data, "AOI_{AOI_name}" := in_x & in_y)
  }
  
  return(data)
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


AOI_inputs_to_list <- function(L_X = Left_x,
                               L_Y = Left_y,
                               R_X = Right_x,
                               R_Y = Right_y){
  
  # Take the individual AOI input vectors and convert them to a nested list
  # Currently only handles two AOIs
  
  AOIs <- list(L = list(X = L_X,
                        Y = L_Y),
               R = list(X = R_X,
                        Y = R_Y)
  )
  return(AOIs)
}

initial_processing <- function(data_in) {
  
  # Performs the initial data processing steps
  data_wGroups <- add_group(data_in)
  data_wSampleNs <- add_sample_n(data_wGroups)
  data_same_SR <- remove_oversampled(data_wSampleNs)
  data_avgGaze <- average_gaze_pos(data_same_SR)
  data_screen_look <- add_screen_looking(data_avgGaze)
  
  
  return(data_screen_look)
} 

add_first_look <- function(data_in, samples_for_look = 12){
  # Calculate when the first look in a specific AOI occurs
  # (first look defined as first incidence of a run of samples categorised
  # as within an AOI for the user input specified amount, default is 12)
  data_fl <- group_by(data_in, part_ID) %>%
    summarise(.groups = "keep",
              first_L = find_first_look(AOI_L, samples_for_look),
              first_R = find_first_look(AOI_R, samples_for_look))
  
  # Categorise which of these two first looks came first and thus which is
  # the true first look
  data_fl <- mutate(data_fl, 
                  FL = case_when(is.na(first_L) & is.na(first_R) ~ NA_character_,
                                 ((first_L < first_R) | is.na(first_R)) ~ "L",
                                 ((first_R < first_L) | is.na(first_L)) ~ "R"))
  
  # Add the first look for each participant and trial back to the samples
  # for those trials
  data_cat <- left_join(data_in, data_fl, by = "part_ID")
  
  # Determine whether an individual sample matches the first look for that
  # participant/trial
  data_cat <- mutate(data_cat,
                   at_first_look = case_when(FL == "L" ~ AOI_L,
                                             FL == "R" ~ AOI_R,
                                             is.na(FL) ~ NA),
                   at_first_look = case_when(
                     (AOI_L | AOI_R) == T ~ at_first_look))
  
  return(data_cat)
  
}

add_bins <- function(data_in, bin_width_s = 0.2){
  
  # Splits the trials into user defined bins
  data_out <- mutate(data_in,
                     bin_N = floor(epoch_time/bin_width_s) + 1,
                     # Generate labels for bins
                     bin_label = paste0((bin_N - 1) * bin_width_s * 1000, "-",
                                        (bin_N) * bin_width_s * 1000, "ms"))
  return(data_out)
}

  
  
  
  return(data_out)
}

sample_analysis <- function(){
  
  # Sample pipeline for analysing data
  data <- load_data()
  data <- initial_processing(data)
  data <- add_screen_looking(data)
  data <- add_bins(data)
  data <- categorise_look(data)
  data <- add_first_look(data)
  
  return(data)
}

