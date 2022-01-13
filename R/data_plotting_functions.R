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

