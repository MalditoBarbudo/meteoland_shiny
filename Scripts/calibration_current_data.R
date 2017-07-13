# Script for the calibration of the 61 daily stations data (current mode)

# libraries
library(meteoland)

# Let's create the interpolation data object
# date vector and length
datevec <- seq(as.Date('2017-01-01'), as.Date('2017-06-30'), 1)
datevec <- datevec[-148]
datevec <- datevec[-164]
ndays <- length(datevec)
excludeRainFromStations <- character(0)

# load the meteorological files
day_data <- vector('list', ndays)
for (i in seq_along(datevec)) {
  # check if file exists
  file_to_read <- file.path('Data', 'DailyCAT', paste0(as.character(datevec[[i]]), '.txt'))
  if (!file.exists(file_to_read)) {
    stop('Faltan dias')
  }
  # read data
  day_data[[i]] <- readmeteorologypoint(file_to_read)
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
interpolator_current <- MeteorologyInterpolationData(
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
  params = defaultInterpolationParams()
)

# check for the spatial and the temporal coverage
head(interpolation.coverage(interpolator_current, percent = TRUE))
head(interpolation.coverage(interpolator_current, type = 'temporal', percent = TRUE))

# calibration, for each variable
# tmin
tmin_cal <- interpolation.calibration(
  interpolator_current, variable = 'Tmin',
  N_seq = seq(5, 60, by = 5),
  alpha_seq = seq(0.5, 10, by = 0.5)
)
# tmax
tmax_cal <- interpolation.calibration(
  interpolator_current, variable = 'Tmax',
  N_seq = seq(5, 60, by = 5),
  alpha_seq = seq(0.5, 10, by = 0.5)
)
# tdew
tdew_cal <- interpolation.calibration(
  interpolator_current, variable = 'Tdew',
  N_seq = seq(5, 60, by = 5),
  alpha_seq = seq(0.5, 10, by = 0.5)
)
# Prec
prec_cal <- interpolation.calibration(
  interpolator_current, variable = 'Prec',
  N_seq = seq(5, 60, by = 5),
  alpha_seq = seq(1, 10, by = 1)
)

# check the results

