## global.R file of the meteoland shiny app
# This file contains custom functions that help to build the shiny app as well
# as to prepare the data for the analysis

################################################################################
# Helper to extract variable values from a netCDF file based on coordinates

ncExtractVarValueByCoords <- function(nc_file, x_coord, y_coord, var_names) {
  
  # open connection to the netCDF file
  nc <- nc_open(nc_file)
  
  # extract X and Y values from netCDF file
  nc_x_coord_vals <- nc$dim$X$vals
  nc_y_coord_vals <- nc$dim$Y$vals
  
  # index for X
  x_dist <- abs(nc_x_coord_vals - x_coord)
  x_index <- which.min(x_dist)
  
  # index for Y
  y_dist <- abs(nc_y_coord_vals - y_coord)
  y_index <- which.min(y_dist)
  
  # build the index
  index <- c(x_index, y_index)
  
  # extract the variables values
  var_values <- lapply(var_names, function(x){
    ncvar_get(nc, x, start = index, count = c(1,1))
  })
  
  # name the vars
  names(var_values) <- var_names
  
  # return the values (each element of the list is an array)
  return(var_values)
}
