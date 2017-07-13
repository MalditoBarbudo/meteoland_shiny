## global.R file of the meteoland shiny app
# This file contains custom functions that help to build the shiny app as well
# as to prepare the data for the analysis

# libraries
library(meteoland)

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
  
  # return the values (unlist to generate a named vector)
  return(unlist(var_values))
}

################################################################################
# Helper function to convert lat/long coordinates from the leaflet map to UTM
# coordinates to help extract variables from the Topography file

convertTopographyCoords <- function(coord_df) {
  
  # make a coordinates object from the data frame provided
  coordinates(coord_df) <- ~lng+lat
  
  # add the projection string attribute
  proj4string(coord_df) <- CRS("+proj=longlat +datum=WGS84")
  
  # transform the coordinates porjection to the Topography projection
  coord_utm <- spTransform(
    coord_df,
    CRS("+init=epsg:3043 +proj=utm +zone=31 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  )
  
  # return the result
  return(coord_utm)
}

################################################################################
# Interpolation process for "current mode"

current_points_mode_process <- function(user_df, user_dates,
                                        excludeRainFromStations = character(0)) {
  
  # STEP 1 BUILD THE INTERPOLATOR OBJECT
  
  # get the default parameters for the MetereologyInterpolationData object
  params <- defaultInterpolationParams()
  
  # build the dates vector to read the metereology files
  user_dates <- as.Date(user_dates)
  datevec <- (user_dates[[1]] - max(params$St_Precipitation, params$St_TemperatureRange)):user_dates[[2]]
  datevec <- as.Date(datevec, format = '%j', origin = as.Date('1970-01-01'))
  ndays <- length(datevec)
  
  # load the metereological files
  day_data <- vector('list', ndays)
  for (i in seq_along(datevec)) {
    # files
    day_data[[i]] <- readmeteorologypoint(
      file.path('Data', 'DailyCAT', paste0(as.character(datevec[[i]]), '.txt'))
    )
    # codes
    codes <- row.names(day_data[[i]])
    # excluded codes
    excodes <- codes[codes %in% excludeRainFromStations]
    # NAs to excluded (apply quality check results)
    day_data[[i]][excodes, 'Precipitation'] <- NA
  }
  
  # get general info needed later
  stations_codes <- row.names(day_data[[1]])
  stations_elevation <- day_data[[1]]$elevation
  stations_slope <- rep(0, length(stations_elevation))
  stations_aspect <- rep(0, length(stations_elevation))
  stations_coords <- cbind(day_data[[1]]$coords.x1, day_data[[2]]$coords.x2)
  stations_coords_sp <- SpatialPoints(
    stations_coords, CRS("+proj=longlat +datum=WGS84")
  )
  station_coords_utm <- spTransform(
    stations_coords_sp,
    CRS("+init=epsg:3043 +proj=utm +zone=31 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  )
  
  stations_n <- length(stations_elevation)
  
  # reshape the data to build the MetereologyInterpolationData object
  MinTemperature <- matrix(
    NA, nrow = stations_n, ncol = ndays,
    dimnames = list(stations_codes, as.character(datevec))
  )
  MaxTemperature <- MinTemperature
  Precipitation <- MinTemperature
  RelativeHumidity <- MinTemperature
  Radiation <- MinTemperature
  WindSpeed <- MinTemperature
  WindDirection <- MinTemperature
  
  # fill the data
  for (i in seq_along(datevec)) {
    MinTemperature[,i] <- day_data[[i]][stations_codes, 'MinTemperature']
    MaxTemperature[,i] <- day_data[[i]][stations_codes, 'MaxTemperature']
    Precipitation[,i] <- day_data[[i]][stations_codes, 'Precipitation']
    RelativeHumidity[,i] <- day_data[[i]][stations_codes, 'MeanRelativeHumidity']
    Radiation[,i] <- day_data[[i]][stations_codes, 'Radiation']
    WindSpeed[,i] <- day_data[[i]][stations_codes, 'WindSpeed']
    WindDirection[,i] <- day_data[[i]][stations_codes, 'WindDirection']
  }
  
  # Finally, we build the interpolator object
  interpolator <- MeteorologyInterpolationData(
    points = station_coords_utm,
    elevation = stations_elevation,
    slope = stations_slope,
    aspect = stations_aspect,
    MinTemperature = MinTemperature,
    MaxTemperature = MaxTemperature,
    Precipitation = Precipitation,
    RelativeHumidity = RelativeHumidity,
    Radiation = Radiation,
    WindSpeed = WindSpeed,
    WindDirection = WindDirection,
    params = params
  )
  
  # and set the parameters obtained in the calibration
  load('Data/calibrations.RData')
  interpolator@params$N_MinTemperature = tmin_cal$N
  interpolator@params$alpha_MinTemperature = tmin_cal$alpha
  interpolator@params$N_MaxTemperature = tmax_cal$N
  interpolator@params$alpha_MaxTemperature = tmax_cal$alpha
  interpolator@params$N_DewTemperature = tdew_cal$N
  interpolator@params$alpha_DewTemperature = tdew_cal$alpha
  interpolator@params$N_PrecipitationEvent = prec_cal$N
  interpolator@params$alpha_PrecipitationEvent = prec_cal$alpha
  interpolator@params$N_PrecipitationAmount = prec_cal$N
  interpolator@params$alpha_PrecipitationAmount = prec_cal$alpha
  rm(tmin_cal, tmax_cal, tdew_cal, prec_cal)
  
  # STEP 2 BUILD THE TOPOGRAPHY OBJECT
  
  # Convert latlong to utm
  user_coords_utm <- convertTopographyCoords(user_df)
  
  # get elevation, slope and aspect values
  n_coords <- length(user_coords_utm@coords[,1])
  
  vals <- vector('list', n_coords)
  
  for (i in 1:n_coords) {
    vals[[i]] <- ncExtractVarValueByCoords(
      nc_file = file.path('Data', 'Topology_grid.nc'),
      x_coord = user_coords_utm@coords[i,1],
      y_coord = user_coords_utm@coords[i,2],
      var_names = c('Elevation', 'Slope', 'Aspect')
    )
  }
  
  # vals is a list of named vectors, this must be converted to a data frame
  # for easily add the variable values to the spatial topography object
  vals_df <- as.data.frame(matrix(unlist(vals), nrow = length(vals),
                                  byrow = TRUE))
  
  names(vals_df) <- names(vals[[1]])
  
  # build the topography object
  user_topo <- SpatialPointsTopography(
    points = user_coords_utm,
    elevation = vals_df$Elevation,
    slope = vals_df$Slope,
    aspect = vals_df$Aspect
  )
  
  # STEP 3 MAKE THE INTERPOLATION
  
  res <- interpolationpoints(
    object = interpolator,
    points = user_topo,
    verbose = FALSE
  )
  
  return(res)
}

