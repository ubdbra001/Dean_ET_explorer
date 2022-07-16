calculate_points <- function(x_points, y_points, group = NA){
  
  # Convert individual points to a data frame that can be used to draw
  # polygons on a plot
  
  # May just work for 4 sided polygons...
  
  # Produces every possible combination of points 
  points_list <- expand.grid(x_points, y_points)
  
  # This swaps the last two points of the first column so the lines of the
  # polygon don't cross over
  points_list[[1]][3:4] <- rev(points_list[[1]][3:4])
  
  points_list <- rename(points_list, x = Var1, y = Var2)
  
  if (!is.na(group)) {points_list$group <- group}
  
  return(points_list)
}


stimuli_areas <- function() {
  
  # Produces the default location for the stimuli presented on Screen
  L_x_vals = c(0.16, 0.32)
  R_x_vals = c(0.68, 0.84)
  y_vals = c(0.36, 0.64)
  
  L_stim <- calculate_points(L_x_vals, y_vals)
  R_stim <- calculate_points(R_x_vals, y_vals)
  
  gap <- tibble(x = NA, y = NA)
  
  stims <- bind_rows(L_stim, gap, R_stim)
  
  return(stims)
}


calculate_AOIs <- function(LX, LY, RX, RY) {

  # Calculates AOIs from inputs and returns dataframe to draw  
  L_points <- calculate_points(LX, LY, "left")
  R_points <- calculate_points(RX, RY, "right")
  
  AOI_points <- bind_rows(L_points, R_points)
  
  return(AOI_points)
}

summarise_looking <- function(data_in, split_groups) {
  
  # Group data differently depending on whether split data checkbox is ticked
  data_grouped <- group_by(data_in, sample_ID)
  if (split_groups) {
    data_grouped <- group_by(data_grouped, Group, .add = TRUE)
  }
  
  data_summary <- summarise(data_grouped, sample_time = first(sample_time),
                         AOI_L = mean(AOI_L), AOI_R = mean(AOI_R),
                         .groups = "keep")
  
  data_arranged <- arrange(data_summary, sample_ID)
  
  data_pivoted <- pivot_longer(data_arranged, cols = c("AOI_L", "AOI_R"),
                               names_to = "AOI_location")
  
  data_recoded <- mutate(data_pivoted, 
                         AOI_location = recode(AOI_location,
                                               AOI_L = "Left AOI",
                                               AOI_R = "Right AOI"))
  
  if (split_groups) {
    # Unite group and AOI labels into a single label
    data_recoded <- unite(data_recoded, Group_AOI, c("Group", "AOI_location"),
                          remove = F, sep = ": ")
  }
  
  return(data_recoded)
  
}
