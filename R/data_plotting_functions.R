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

